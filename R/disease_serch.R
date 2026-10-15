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
#' **Ontology IDs may be multi-valued.** A single disease can carry several
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
#' To go from a disease to its associated **targets**, see [disease_target()].
#'
#' @seealso [disease_target()]
#' @export
#' @examples
#' disease_serch("Cataract 13 With Adult I Phenotype")
#' disease_serch(c("C3805373", "C1845055"), type = "umls_id")
#' disease_serch("thalassemia", type = "disease_name", match = "contains")
disease_serch <- function(disease, type = "disease_name",
                          match = c("exact", "contains"), ...) {
  type <- match.arg(type, c("disease_name", "mesh_id", "umls_id", "disease_id"))
  match <- match.arg(match)

  empty <- .tcm_disease_empty()
  # 匹配语义与输入清洗的唯一实现在 .tcm_disease_where()（与 disease_target 共用）
  w <- .tcm_disease_where(disease, type = type, match = match)
  if (!length(w$x)) return(empty)

  hit <- .tcm_query(sprintf(
    "SELECT disease_id, disease_name, mesh_id, umls_id, matched
       FROM read_parquet('%s') WHERE %s", .tcm_parquet_path("disease"), w$where))
  if (!nrow(hit)) {
    print(paste0(w$x, " is/are not in the datasets."))
    return(empty)
  }
  # 同一疾病命中多条输入时只留一行；按 disease_id 数值排序便于比对
  hit <- hit[!duplicated(hit$disease_id), , drop = FALSE]
  ord <- order(suppressWarnings(as.numeric(hit$disease_id)), hit$disease_id)
  hit <- hit[ord, , drop = FALSE]
  rownames(hit) <- NULL
  hit
}

#' @noRd
.tcm_disease_empty <- function() {
  data.frame(disease_id = character(), disease_name = character(),
             mesh_id = character(), umls_id = character(),
             matched = logical())
}
