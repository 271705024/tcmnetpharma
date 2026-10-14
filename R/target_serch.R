#' Screen target based on ingredients from Chinese herbal medicine
#'
#' @param target The name of the target filter
#' @param resolve_herb Logical (default `FALSE`). When `TRUE`, results are mapped to
#'   the herb entity layer (extra `herb_id` column, canonical `herb` key, collapsed
#'   rows) — see `target_herb` for the full explanation.
#' @param with_trust Logical (default `FALSE`). When `TRUE`, attaches the trust
#'   columns of the standard table (`qc_level`, `qc_level_reason`, `cas_verified`,
#'   `latin_name_std`, `latin_match_status`); worst level wins on ambiguity.
#' @param ... additional parameters
#'
#' @return data.frame
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @examples
#' target <- c("PRSS3", "ADH1C")
#' target_filter<- target_serch(target)
#' head(target_filter)
target_serch <- function(target, resolve_herb = FALSE, with_trust = FALSE, ...) {
  # 惰性查询（P3.2）：DuckDB 直读 compat parquet
  hit <- .tcm_distinct_where("Target", target)
  target_deficiency <- setdiff(target, hit)
  if (length(target_deficiency)) {
    print(paste0(target_deficiency, " is/are not in the datasets."))
  }
  target <- intersect(target, hit)
  if (!length(target)) {
    return(data.frame(herb = character(), molecule_id = character(),
                      molecule = character(), target = character()))
  }
  out <- .tcm_filter(
    "Target", target,
    c(herb = "Herb_name_pin_yin", molecule_id = "Molecule_name",
      molecule = "MOL_ID", target = "Target")) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    tidyr::drop_na()
  if (isTRUE(resolve_herb)) out <- .tcm_attach_entity(out)
  if (isTRUE(with_trust)) out <- .tcm_attach_trust(out)
  return(out)
}

