#' Screen targets based on ingredients from Chinese herbal medicine
#'
#' @param herb The name of the compound Chinese herbal medicine
#' @param OB.value oral bioavailability
#' @param DL.value drug-likeness
#' @param type Writing format of herb name:
#' "Herb_name_pin_yin" or "Herb_cn_name"
#' @param ... additional parameters
#'
#' @return data.frame
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @examples
#' herbs <- c("ma huang", "ku xing ren")
#' fufang <- herb_target(herbs, type = "Herb_name_pin_yin")
#' head(fufang)
herb_target <- function(herb,
                        OB.value = 30,
                        DL.value = 0.18,
                        type = "Herb_cn_name",
                        ...) {
  # 惰性查询（P3.2）：DuckDB 直读 compat parquet
  check_col <- if (type == "Herb_cn_name") "Herb_cn_name" else "Herb_name_pin_yin"
  hit <- .tcm_distinct_where(check_col,
                             sprintf("%s IN (%s)", check_col, .sql_in(herb)))
  herb_deficiency <- setdiff(herb, hit)
  if (length(herb_deficiency)) {
    print(paste0(herb_deficiency, " is/are not in the datasets."))
  }
  herb <- intersect(herb, hit)
  if (!length(herb)) {
    return(data.frame(herb = character(), molecule_id = character(),
                      molecule = character(), target = character()))
  }
  extra <- .tcm_adme_where(
                   OB.value, DL.value)
  fufang <- .tcm_filter(
    check_col, herb,
    c(herb = "Herb_name_pin_yin", molecule_id = "Molecule_name",
      molecule = "MOL_ID", target = "Target"),
    extra_where = extra) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    tidyr::drop_na()
  return(fufang)
}
