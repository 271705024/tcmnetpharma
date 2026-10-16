#' Screen molecules based on ingredients from Chinese herbal medicine
#'
#' @param molecule The name of the molecule filter
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
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @examples
#' molecule <- c("Hexyl acetate", "quercetin 3-o-rhamnopyranosyl")
#' molecule_filter<- molecule_serch(molecule)
#' head(molecule_filter)
molecule_serch <- function(molecule, resolve_herb = FALSE, with_trust = FALSE, ...) {
  # 惰性查询（P3.2）：DuckDB 直读 compat parquet，仅拉取 IN 子集
  # 大小写不敏感（2026-10-16）：.tcm_distinct_where 已 lower() 两侧，hit 含规范值；
  # .tcm_ci_match 把用户输入归一到规范大小写再喂给 .tcm_filter。
  hit <- .tcm_distinct_where("Molecule_name", molecule)
  ci <- .tcm_ci_match(molecule, hit)
  if (length(ci$unmatched)) {
    print(paste0(ci$unmatched, " is/are not in the datasets."))
  }
  molecule <- ci$canonical
  if (!length(molecule)) {
    return(data.frame(herb = character(), molecule_id = character(),
                      molecule = character(), target = character()))
  }
  out <- .tcm_filter(
    "Molecule_name", molecule,
    c(herb = "Herb_name_pin_yin", molecule_id = "MOL_ID",
      molecule = "Molecule_name", target = "Target")) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    tidyr::drop_na()
  # 统一缺失 MOL ID 表示（2026-10-16）：多值字段中的 "NA"/"NA|" 残留收敛为纯 MOL 编号串
  # （见 .tcm_clean_mol_id：只保留真实编号、以 ';' 重连，空值归为 ""）。
  out <- dplyr::mutate(out, molecule_id = .tcm_clean_mol_id(.data$molecule_id))
  if (isTRUE(resolve_herb)) out <- .tcm_attach_entity(out)
  if (isTRUE(with_trust)) out <- .tcm_attach_trust(out)
  return(out)
}
