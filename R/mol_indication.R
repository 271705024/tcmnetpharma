#' Screen drug indications (approved / investigational uses)
#'
#' From a drug (molecule name / ChEMBL ID / InChIKey) to its **indications**
#' (the diseases it is approved or being investigated for), using the ChEMBL 37
#' `drug_indication` layer (`tcm_mol_indication.parquet`, 60,055 rows; a
#' drug-disease **binary** reference table, deliberately kept out of the
#' standard herb-mol-target triple).
#'
#' The distinguishing column is **`max_phase_for_ind`** — the *indication-level*
#' development phase: the same drug can be `4` (approved) for one disease and
#' `2` for another. This is richer than the drug-level `max_phase` and is not
#' available from the target-side (`tcm_target_moa`) or the OpenTargets
#' disease-target layer.
#'
#' @param drug A vector of drug identifiers (interpretation depends on `type`).
#' @param type One of `"mol_name"` (default), `"mol_chembl_id"`, `"inchikey"`.
#' @param match `"exact"` (default) or `"contains"` (substring; only meaningful
#'   for `type = "mol_name"`). Name matching is case-insensitive.
#' @param min_phase Numeric (default `0`). Only indications with
#'   `max_phase_for_ind >= min_phase` are returned. Use `4` to keep approved
#'   uses only. **No effective threshold is applied by default** so nothing is
#'   silently dropped (phase can be `-1` when unknown).
#' @param natural_product Logical or `NULL` (default `NULL` = no filter). Set
#'   `TRUE` to keep only natural-product-derived drugs (higher TCM relevance),
#'   `FALSE` to exclude them.
#' @param ... additional parameters
#'
#' @return data.frame with columns `mol_chembl_id`, `mol_name`, `inchikey`,
#'   `natural_product`, `mol_max_phase`, `max_phase_for_ind`, `mesh_id`,
#'   `mesh_heading`, `efo_id`, `efo_term`. Returns 0 rows (same column contract)
#'   when nothing matches.
#'
#' @details
#' Rows are ordered by `max_phase_for_ind` descending then disease name, so the
#' most-developed uses surface first. Diseases carry **both** MeSH and EFO
#' vocabulary IDs when available.
#'
#' `inchikey` is the primary cross-database join key (present for ~75% of rows;
#' biologics have none). `mapped_to_mol_dict` from the source is dropped here —
#' it flags TCMSP/HERB ingredient overlap and is rarely useful to the end user.
#'
#' @seealso [disease_target()], [mol_moa()]
#' @export
#' @examples
#' head(mol_indication("Aspirin"))
#' head(mol_indication("CHEMBL25", type = "mol_chembl_id"))
#' head(mol_indication("Imatinib", min_phase = 4))
mol_indication <- function(drug, type = "mol_name",
                           match = c("exact", "contains"),
                           min_phase = 0, natural_product = NULL, ...) {
  type <- match.arg(type, c("mol_name", "mol_chembl_id", "inchikey"))
  match <- match.arg(match)

  if (!is.numeric(min_phase) || length(min_phase) != 1L || is.na(min_phase)) {
    stop("min_phase must be a single non-NA number")
  }
  if (!is.null(natural_product) &&
      (!is.logical(natural_product) || length(natural_product) != 1L || is.na(natural_product))) {
    stop("natural_product must be NULL or a single TRUE/FALSE")
  }

  empty <- .tcm_mol_indication_empty()
  w <- .tcm_mol_where(drug, type = type, match = match)
  if (!length(w$x)) return(empty)

  extra <- sprintf("max_phase_for_ind >= %s", sprintf("%.6f", as.numeric(min_phase)))
  if (!is.null(natural_product)) {
    # source stores natural_product as 0/1 (numeric) in the parquet
    extra <- c(extra, sprintf("natural_product %s", if (isTRUE(natural_product)) "= 1" else "IS DISTINCT FROM 1"))
  }

  res <- .tcm_query(sprintf(
    "SELECT mol_chembl_id, mol_name, inchikey, natural_product, mol_max_phase,
            max_phase_for_ind, mesh_id, mesh_heading, efo_id, efo_term
       FROM read_parquet('%s')
      WHERE %s AND %s
      ORDER BY max_phase_for_ind DESC, mesh_heading",
    .tcm_parquet_path("mol_indication"), w$where, paste(extra, collapse = " AND ")))
  if (!nrow(res)) {
    print(paste0("No indication at min_phase ", min_phase,
                 " for: ", paste(w$x, collapse = ", ")))
    return(empty)
  }
  rownames(res) <- NULL
  res
}

#' @noRd
.tcm_mol_indication_empty <- function() {
  data.frame(mol_chembl_id = character(), mol_name = character(),
             inchikey = character(), natural_product = integer(),
             mol_max_phase = numeric(), max_phase_for_ind = numeric(),
             mesh_id = character(), mesh_heading = character(),
             efo_id = character(), efo_term = character())
}

# ---------------------------------------------------------------------------
# 药物身份匹配条件构造 + 输入清洗（mol_name / mol_chembl_id / inchikey）
#
# 与 .tcm_disease_where 同一设计：唯一实现，保证 mol_indication / mol_moa 口径一致
# （铁律 54：同一事实多地声明必漏 → 收敛单一源）。
# 匹配语义：
#   · mol_name：默认**精确**（大小写不敏感，两侧 lower()），显式 contains 才子串；
#   · mol_chembl_id / inchikey：**精确**（ID 类不做子串，避免假阳性）；
#   · ID/键类输入只接受 `[A-Za-z0-9:-]`，含正则元字符的值显式拒绝并提示。
#   · inchikey 全大写对齐（库内标准 InChIKey 大写）。
# 返回 list(where = <SQL 条件或 NULL>, x = <清洗后的输入>)
#' @noRd
.tcm_mol_where <- function(x, type = "mol_name", match = "exact") {
  type <- match.arg(type, c("mol_name", "mol_chembl_id", "inchikey"))
  match <- match.arg(match, c("exact", "contains"))
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  x <- unique(trimws(x))
  if (type != "mol_name") {
    bad <- x[!grepl("^[A-Za-z0-9:-]+$", x)]
    if (length(bad)) {
      print(paste0(bad, " is/are not valid IDs for type = ", type, "."))
      x <- setdiff(x, bad)
    }
  }
  if (!length(x)) return(list(where = NULL, x = character()))

  # inchikey 归一大写；chembl_id 保留原样但比较走 lower() 双侧
  if (type == "inchikey") x <- toupper(x)

  where <- if (type == "mol_name") {
    if (identical(match, "exact")) {
      sprintf("lower(trim(mol_name)) IN (%s)", .sql_in(tolower(x)))
    } else {
      paste0("(", paste(sprintf("contains(lower(mol_name), %s)",
                                .sql_in(tolower(x))), collapse = " OR "), ")")
    }
  } else if (type == "mol_chembl_id") {
    sprintf("lower(mol_chembl_id) IN (%s)", .sql_in(tolower(x)))
  } else {
    sprintf("upper(inchikey) IN (%s)", .sql_in(toupper(x)))
  }
  list(where = where, x = x)
}
