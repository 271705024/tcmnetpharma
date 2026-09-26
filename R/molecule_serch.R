#' Screen molecules based on ingredients from Chinese herbal medicine
#'
#' @param molecule The name of the molecule filter
#' @param ... additional parameters
#'
#' @return data.frame
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @examples
#' molecule <- c("Hexyl acetate", "quercetin 3-o-rhamnopyranosyl")
#' molecule_filter<- molecule_serch(molecule)
#' head(molecule_filter)
molecule_serch <- function(molecule, ...) {
  # 惰性查询（P3.2）：DuckDB 直读 compat parquet，仅拉取 IN 子集
  hit <- .tcm_distinct_where("Molecule_name", molecule)
  molecule_deficiency <- setdiff(molecule, hit)
  if (length(molecule_deficiency)) {
    print(paste0(molecule_deficiency, " is/are not in the datasets."))
  }
  molecule <- intersect(molecule, hit)
  if (!length(molecule)) {
    return(data.frame(herb = character(), molecule_id = character(),
                      molecule = character(), target = character()))
  }
  molecule2 <- .tcm_filter(
    "Molecule_name", molecule,
    c(herb = "Herb_name_pin_yin", molecule_id = "Molecule_name",
      molecule = "MOL_ID", target = "Target")) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    tidyr::drop_na()
  return(molecule2)
}
