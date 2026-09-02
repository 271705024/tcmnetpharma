#' Screen target based on ingredients from Chinese herbal medicine
#'
#' @param target The name of the target filter
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
#' target_filter<- target_serch(target, type = "Herb_name_pin_yin")
#' head(target_filter)
target_serch <- function(target, ...) {
  # 惰性查询（P3.2）：DuckDB 直读 compat parquet
  hit <- .tcm_distinct_where("Target", sprintf("Target IN (%s)", .sql_in(target)))
  target_deficiency <- setdiff(target, hit)
  if (length(target_deficiency)) {
    print(paste0(target_deficiency, " is/are not in the datasets."))
  }
  target <- intersect(target, hit)
  if (!length(target)) {
    return(data.frame(herb = character(), molecule_id = character(),
                      molecule = character(), target = character()))
  }
  fufang <- .tcm_filter(
    "Target", target,
    c(herb = "Herb_name_pin_yin", molecule_id = "Molecule_name",
      molecule = "MOL_ID", target = "Target")) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    tidyr::drop_na()
  return(fufang)
}

