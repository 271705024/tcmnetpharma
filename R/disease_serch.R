#' Screen diseases in the Layer A disease layer
#'
#' Look up diseases by name, MeSH ID, UMLS ID, or SymMap disease ID. This is the
#' consumption entry point of the **Layer A disease layer** (`disease_main.parquet`,
#' 14,434 SymMap SMDE disease entries aligned against OpenTargets disease ontology).
#'
#' @param disease A vector of disease names / MeSH IDs / UMLS IDs / SymMap
#'   disease IDs (interpretation depends on `type`).
#' @param type One of `"disease_name"` (default), `"mesh_id"`, `"umls_id"`,
#'   `"disease_id"` (SymMap internal disease id).
#' @param match `"exact"` (default) or `"contains"` (substring match; only
#'   meaningful for `type = "disease_name"`). Following the "precise match first"
#'   rule, `"exact"` is the default so that a query never silently widens.
#' @param ... additional parameters
#'
#' @return data.frame with columns `disease_id`, `disease_name`, `mesh_id`,
#'   `umls_id`, `matched`. Returns 0 rows (with the same column contract) when
#'   nothing matches.
#'
#' @details
#' **Ontology IDs may be multi-valued.** A single Disease can carry several
#' MeSH / UMLS IDs; they are stored in one field separated by `;`
#' (e.g. `"C0342770;C2936826"`). Matching is therefore done on **ID boundaries**,
#' so `disease_serch("C2936826", type = "umls_id")` still finds the row above.
#'
#' **MeSH IDs have two legal prefixes**: `C…` (Supplementary Concept) and
#' `D…` (Descriptor). Both occur in the data.
#'
#' **`matched`** is `TRUE` when the SymMap entry could be aligned to an
#' OpenTargets ontology ID. Rows with `matched = FALSE` are **kept on purpose** —
#' they exist in SymMap but the ontology alignment failed; they are *not*
#' silently dropped.
#'
#' @note There is currently **no disease -> target / ingredient relation** in
#'   this package: Layer A only contributed herb / target cross-references to the
#'   standard table plus this disease ontology map. Diseases are therefore
#'   resolvable to ontology IDs, but not yet to targets.
#'
#' @export
#' @examples
#' disease_serch("Cataract 13 With Adult I Phenotype")
#' disease_serch(c("C3805373", "C1845055"), type = "umls_id")
#' disease_serch("thalassemia", type = "disease_name", match = "contains")
disease_serch <- function(disease, type = "disease_name",
                          match = c("exact", "contains"), ...) {
  type <- match.arg(type, c("disease_name", "mesh_id", "umls_id", "disease_id"))
  match <- match.arg(match)

  x <- as.character(disease)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  x <- unique(trimws(x))
  empty <- data.frame(disease_id = character(), disease_name = character(),
                      mesh_id = character(), umls_id = character(),
                      matched = logical())
  if (!length(x)) return(empty)

  # ID 类查询只接受字母数字（MeSH/UMLS/SymMap id 的合法字符集）；
  # 混入正则元字符的值会被显式丢弃而不是静默变成一次宽松匹配。
  if (type != "disease_name") {
    bad <- x[!grepl("^[A-Za-z0-9]+$", x)]
    if (length(bad)) {
      print(paste0(bad, " is/are not valid IDs for type = ", type, "."))
      x <- setdiff(x, bad)
    }
    if (!length(x)) return(empty)
  } else if (identical(match, "exact")) {
    # 结果列契约里有 disease_name，故此处保留原样（仅大小写/空白归一由 SQL 处理）
    x <- x
  }

  p <- .tcm_parquet_path("disease")
  cols <- "SELECT disease_id, disease_name, mesh_id, umls_id, matched FROM read_parquet('%s')"
  base <- sprintf(cols, p)

  where <- if (type == "disease_name") {
    if (identical(match, "exact")) {
      sprintf("lower(trim(disease_name)) IN (%s)", .sql_in(tolower(x)))
    } else {
      paste0("(", paste(sprintf("contains(lower(disease_name), %s)",
                                .sql_in(tolower(x))), collapse = " OR "), ")")
    }
  } else {
    # 边界匹配：`;` 是并列分隔符 → 必须匹配「整段」而不是子串
    #   （否则 'C123' 会命中 'C1234' —— 典型假通过）
    # ⚠️ 必须**逐值**生成一条完整表达式再 OR 起来：把 `.sql_in(x)` 的
    #   `'a', 'b'` 直接塞进单条 regexp_matches(...) 会被逗号切成第三个参数
    #   （DuckDB 报 `Unrecognized Regex option 1` —— 2026-10-15 实测）。
    conds <- vapply(x, function(v) {
      sprintf("regexp_matches(lower(%s), '(^|;)' || %s || '(;|$)')",
              type, .sql_in(tolower(v)))
    }, character(1), USE.NAMES = FALSE)
    paste0("(", paste(conds, collapse = " OR "), ")")
  }

  hit <- .tcm_query(paste(base, "WHERE", where))
  if (!nrow(hit)) {
    print(paste0(x, " is/are not in the datasets."))
    return(empty)
  }
  # 同一疾病命中多条输入时只留一行；按 disease_id 数值排序便于比对
  hit <- hit[!duplicated(hit$disease_id), , drop = FALSE]
  ord <- order(suppressWarnings(as.numeric(hit$disease_id)), hit$disease_id)
  hit <- hit[ord, , drop = FALSE]
  rownames(hit) <- NULL
  hit
}
