#' Find Chinese medicines and their active compounds through the target
#'
#' @param gene a vector of gene name
#' @param OB.value oral bioavailability
#' @param DL.value drug-likeness
#' @param ... additional parameters
#'
#' @return data.frame
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @examples
#' gene <- c("MAPK1", "JUN", "FOS", "RAC1", "IL1", "IL6")
#' target_herbs <- target_herb(gene)
target_herb <- function(gene, OB.value = 30, DL.value = 0.18, ...) {
  # 惰性查询（P3.2）：DuckDB 直读 compat parquet
  hit <- .tcm_distinct_where("Target", sprintf("Target IN (%s)", .sql_in(gene)))
  gene_deficiency <- setdiff(gene, hit)
  if (length(gene_deficiency)) {
    print(paste0(gene_deficiency, " is/are not in the datasets."))
  }
  gene <- intersect(gene, hit)
  if (!length(gene)) {
    return(data.frame(herb = character(), molecule_id = character(),
                      molecule = character(), target = character()))
  }
  extra <- .tcm_adme_where(
                   OB.value, DL.value)
  data_herbs <- .tcm_filter(
    "Target", gene,
    c(herb = "Herb_name_pin_yin", molecule_id = "Molecule_name",
      molecule = "MOL_ID", target = "Target"),
    extra_where = extra) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    tidyr::drop_na()
  return(data_herbs)
}
