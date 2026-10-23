#' Screen targets associated with a disease
#'
#' From a disease (name / MeSH ID / UMLS ID / SymMap disease ID) to its
#' **associated targets**, using the Layer A disease-target layer
#' (`disease_target.parquet`, 3,134,586 rows built from the OpenTargets 25.06
#' `association_overall_direct` dataset bridged to SymMap diseases via MeSH/UMLS).
#'
#' @param disease A vector of disease names / MeSH IDs / UMLS IDs / SymMap
#'   disease IDs (interpretation depends on `type`).
#' @param type One of `"disease_name"` (default), `"mesh_id"`, `"umls_id"`,
#'   `"disease_id"`.
#' @param match `"exact"` (default) or `"contains"` (substring; only meaningful
#'   for `type = "disease_name"`).
#' @param min_score Numeric (default `0`). Only associations with
#'   `score >= min_score` are returned. OpenTargets scores are 0-1; a useful
#'   filter for "strong" associations is often `>= 0.1`, but **no threshold is
#'   applied by default** so that nothing is silently dropped.
#' @param ... additional parameters
#'
#' @return data.frame with columns `disease_id`, `disease_name`, `ot_disease_id`
#'   (the OpenTargets disease the score came from), `target_symbol`,
#'   `target_entrez`, `target_id` (tcmnetpharma target id), `target_ensembl`, `score`,
#'   `evidence_count`, `n_ot_disease`. Returns 0 rows (same column contract) when
#'   nothing matches.
#'
#' @details
#' **`score` is OpenTargets' own overall *direct* association score (0-1)** — it
#' is reported as-is, not recomputed here. `evidence_count` is the number of
#' evidence items OpenTargets counted.
#'
#' A SymMap disease may reach the same target through **several** OpenTargets
#' diseases (via different MeSH / UMLS IDs). Rows are deduplicated on
#' `(disease_id, target_ensembl)`: `score` is the **maximum** over those paths,
#' `ot_disease_id` records which OpenTargets disease that score came from, and
#' `n_ot_disease` tells how many OpenTargets diseases were involved — so a single
#' "max" never hides the multi-source nature.
#'
#' `target_entrez` / `target_id` are `NA` for targets that OpenTargets knows but
#' the tcmnetpharma target dictionary does not (about 11% of rows); those rows are kept
#' so that the gap is visible rather than silently filtered out.
#'
#' @seealso [disease_serch()], [target_herb()]
#' @export
#' @examples
#' head(disease_target("Thalassemia"))
#' head(disease_target("Thalassemia", min_score = 0.1))
#' disease_target(c("C1845055"), type = "umls_id")
disease_target <- function(disease, type = "disease_name",
                           match = c("exact", "contains"),
                           min_score = 0, ...) {
  type <- match.arg(type, c("disease_name", "mesh_id", "umls_id", "disease_id"))
  match <- match.arg(match)

  if (!is.numeric(min_score) || length(min_score) != 1L || is.na(min_score)) {
    stop("min_score must be a single non-NA number")
  }

  empty <- .tcm_disease_target_empty()
  # 与 disease_serch 共用同一套匹配语义（唯一实现，避免两函数口径漂移）
  w <- .tcm_disease_where(disease, type = type, match = match)
  if (!length(w$x)) return(empty)

  # 两步比一次 join 更好读：① 解析疾病；② 取关联。
  # 这样"疾病不存在"与"该疾病没有关联靶点"能给出**不同**的提示，而不是都静默返回空。
  ids <- .tcm_query(sprintf(
    "SELECT DISTINCT disease_id FROM read_parquet('%s') WHERE %s",
    .tcm_parquet_path("disease"), w$where))$disease_id
  if (!length(ids)) {
    print(paste0(w$x, " is/are not in the datasets."))
    return(empty)
  }

  res <- .tcm_query(sprintf(
    "SELECT disease_id, disease_name, ot_disease_id, target_symbol, target_entrez,
            target_id, target_ensembl, score, evidence_count, n_ot_disease
       FROM read_parquet('%s')
      WHERE disease_id IN (%s) AND score >= %s
      ORDER BY score DESC, disease_id, target_symbol",
    .tcm_parquet_path("disease_target"), .sql_in(ids),
    sprintf("%.6f", as.numeric(min_score))))
  if (!nrow(res)) {
    print(paste0("No target association above score ", min_score,
                 " for: ", paste(w$x, collapse = ", ")))
    return(empty)
  }
  rownames(res) <- NULL
  res
}

#' @noRd
.tcm_disease_target_empty <- function() {
  data.frame(disease_id = character(), disease_name = character(),
             ot_disease_id = character(), target_symbol = character(),
             target_entrez = character(), target_id = character(),
             target_ensembl = character(), score = numeric(),
             evidence_count = integer(), n_ot_disease = integer())
}
