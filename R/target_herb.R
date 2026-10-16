#' Find Chinese medicines and their active compounds through the target
#'
#' @param gene a vector of gene name
#' @param OB.value oral bioavailability
#' @param DL.value drug-likeness
#' @param resolve_herb Logical (default `FALSE`). When `TRUE`, results are mapped to
#'   the herb entity layer: an extra `herb_id` column is added, the `herb` column is
#'   replaced by the entity's canonical pinyin key, and rows that become identical
#'   are collapsed. This stops one herb from appearing as several rows just because
#'   the source spells its pinyin differently (`houpo` / `houpu`).
#' @param with_trust Logical (default `FALSE`). When `TRUE`, the trust/provenance
#'   columns of the standard table are attached per row (`qc_level`,
#'   `qc_level_reason`, `cas_verified`, `latin_name_std`, `latin_match_status`).
#'   When one delivered row maps to several source rows the **worst** level is
#'   reported (conservative). Requires the standard table (shipped in the bundle).
#' @param ... additional parameters
#'
#' @return data.frame. Columns: `herb`, `molecule_id` (MOL ID), `molecule` (molecule name), `target`,
#'   `adme` (see `herb_target` for the 2026-09-28 "keep as much as possible" rule).
#'   With `resolve_herb = TRUE` an extra `herb_id` column; with `with_trust = TRUE`
#'   the five trust columns described above.
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @examples
#' gene <- c("MAPK1", "JUN", "FOS", "RAC1", "IL1", "IL6")
#' target_herbs <- target_herb(gene)
target_herb <- function(gene, OB.value = 30, DL.value = 0.18,
                        resolve_herb = FALSE, with_trust = FALSE, ...) {
  # 惰性查询（P3.2）：DuckDB 直读 compat parquet
  # 大小写不敏感（2026-10-16）：.tcm_distinct_where 已 lower() 两侧，hit 含规范值；
  # .tcm_ci_match 把用户输入归一到规范大小写再喂给 .tcm_filter。
  hit <- .tcm_distinct_where("Target", gene)
  ci <- .tcm_ci_match(gene, hit)
  if (length(ci$unmatched)) {
    print(paste0(ci$unmatched, " is/are not in the datasets."))
  }
  gene <- ci$canonical
  if (!length(gene)) {
    return(data.frame(herb = character(), molecule_id = character(),
                      molecule = character(), target = character(),
                      adme = character()))
  }
  # 口径「尽量全」（2026-09-28）：缺失 OB/DL 的行保留并以 adme 列标注。
  extra <- .tcm_adme_where(OB.value, DL.value)
  out <- .tcm_filter_ex(
    "Target", gene,
    c(herb = "Herb_name_pin_yin", molecule_id = "MOL_ID",
      molecule = "Molecule_name", target = "Target"),
    extra_where = extra,
    extra_select = c(adme = .tcm_adme_flag_sql())) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    tidyr::drop_na()
  # 统一缺失 MOL ID 表示（2026-10-16）：多值字段中的 "NA"/"NA|" 残留收敛为纯 MOL 编号串
  # （见 .tcm_clean_mol_id：只保留真实编号、以 ';' 重连，空值归为 ""）。
  out <- dplyr::mutate(out, molecule_id = .tcm_clean_mol_id(.data$molecule_id))
  if (isTRUE(resolve_herb)) out <- .tcm_attach_entity(out)
  if (isTRUE(with_trust)) out <- .tcm_attach_trust(out)
  return(out)
}
