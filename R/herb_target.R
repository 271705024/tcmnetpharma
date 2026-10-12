#' Screen targets based on ingredients from Chinese herbal medicine
#'
#' @param herb The name of the compound Chinese herbal medicine
#' @param OB.value oral bioavailability
#' @param DL.value drug-likeness
#' @param type Writing format of herb name:
#' "Herb_name_pin_yin" or "Herb_cn_name"
#' @param resolve_herb Logical (default `FALSE`). When `TRUE`, each input name is
#'   expanded to **all retrieval keys of the same herb entity** (via the herb
#'   entity layer `herb_key_map.parquet`), so that source-side spelling variants
#'   (`houpo` / `houpu`, `shenjincao` / `shenjingcao`) are not silently missed.
#'   Default `FALSE` keeps the exact-match semantics unchanged (backward compatible).
#' @param ... additional parameters
#'
#' @return data.frame. Columns: `herb`, `molecule_id`, `molecule`, `target`,
#'   and `adme` -- the ADME label introduced by the 2026-09-28 "keep as much as
#'   possible" rule (`'ok'` = passes OB/DL thresholds; `'no_adme'` = the source
#'   has no OB/DL for this pair and it is now retained instead of dropped).
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
                        resolve_herb = FALSE,
                        ...) {
  # 惰性查询（P3.2）：DuckDB 直读 compat parquet
  check_col <- if (type == "Herb_cn_name") "Herb_cn_name" else "Herb_name_pin_yin"
  # herb 实体归一（P0-B，2026-10-12）：显式开启才展开，默认口径不变。
  # 解决「同名异键漏检」——源侧同一味药有多种拼法，精确匹配只命中其中一个。
  if (isTRUE(resolve_herb)) {
    herb <- unique(.tcm_herb_resolve(herb, type = type))
    if (!length(herb)) {
      return(data.frame(herb = character(), molecule_id = character(),
                        molecule = character(), target = character(),
                        adme = character()))
    }
  }
  # 列名映射与 WHERE 构造均由 .tcm_distinct_where() 内部完成（契约统一，
  # 调用方不可能漏映射）。背景见 docs/data-management-spec-2026-08-24.md（3. 命名规则）。
  hit <- .tcm_distinct_where(check_col, herb)
  herb_deficiency <- setdiff(herb, hit)
  if (length(herb_deficiency)) {
    print(paste0(herb_deficiency, " is/are not in the datasets."))
  }
  herb <- intersect(herb, hit)
  if (!length(herb)) {
    return(data.frame(herb = character(), molecule_id = character(),
                      molecule = character(), target = character(),
                      adme = character()))
  }
  # 口径「尽量全」（2026-09-28）：缺失 OB/DL 的行保留并以 adme 列标注。
  extra <- .tcm_adme_where(OB.value, DL.value)
  fufang <- .tcm_filter_ex(
    check_col, herb,
    c(herb = "Herb_name_pin_yin", molecule_id = "Molecule_name",
      molecule = "MOL_ID", target = "Target"),
    extra_where = extra,
    extra_select = c(adme = .tcm_adme_flag_sql())) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    tidyr::drop_na()
  return(fufang)
}
