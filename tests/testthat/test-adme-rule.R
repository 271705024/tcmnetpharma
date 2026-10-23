# ============================================================================
# 口径契约：「尽量全」（boss 2026-09-28 拍板）
#
# 约定：默认结果**包含**源侧没有 OB/DL 的行，并以 `adme` 列标注：
#   'ok'      -> 通过 OB/DL 阈值
#   'no_adme' -> 源侧无 OB/DL（旧口径下会被静默丢弃）
#
# 背景：compat 表里约 80.33% 的行源侧无 OB/DL（多为 BATMAN predicted 边）。
#   旧口径只保留 OB>=x & DL>=y → 用户看到的不是「全部证据」。
#   详见 docs/legacy-issues-ledger-2026-09-28.md（B2/B3）与 data-refresh-plan。
# ============================================================================

.adme_test_compat_ok <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE)) return(FALSE)
  p <- system.file("extdata", "tcm_network_compat.parquet", package = "tcmnetpharma")
  if (!nzchar(p)) return(FALSE)
  file.exists(p)
}

.adme_skip <- function() {
  testthat::skip_if_not(.adme_test_compat_ok(),
                        "tcm_network_compat.parquet \u4e0d\u53ef\u7528")
}

test_that("herb_target \u8fd4\u56de adme \u6807\u6ce8\u5217", {
  .adme_skip()
  res <- herb_target("\u7f8c\u6d3b")          # 羌活
  expect_true(is.data.frame(res))
  expect_true("adme" %in% colnames(res))
  expect_true(all(res$adme %in% c("ok", "no_adme")))
})

test_that("target_herb \u8fd4\u56de adme \u6807\u6ce8\u5217", {
  .adme_skip()
  res <- target_herb("PTGS2")
  expect_true(is.data.frame(res))
  expect_true("adme" %in% colnames(res))
})

test_that("\u53e3\u5f84\u300c\u5c3d\u91cf\u5168\u300d\u786e\u5b9e\u6bd4\u65e7\u53e3\u5f84\u591a\u51fa no_adme \u884c", {
  .adme_skip()
  # \u786c\u8bc1\u636e\uff1a\u540c\u4e00\u67e5\u8be2\u5728\u4e24\u79cd\u53e3\u5f84\u4e0b\u7684\u884c\u6570\u5dee = \u88ab\u65e7\u53e3\u5f84\u4e22\u6389\u7684\u90e8\u5206\u3002
  keep_all <- herb_target("\u7f8c\u6d3b")
  old <- tcmnetpharma:::.tcm_query(sprintf(
    "SELECT COUNT(*) AS n FROM read_parquet('%s') WHERE \"%s\" IN ('\u7f8c\u6d3b') AND (%s)",
    tcmnetpharma:::.tcm_parquet_path(), tcmnetpharma:::.tcm_col_db("Herb_cn_name"),
    tcmnetpharma:::.tcm_adme_where(30, 0.18, include_missing = FALSE)))
  expect_gt(nrow(keep_all), old$n)
  expect_true(any(keep_all$adme == "no_adme"))
})

test_that("\u65e7\u53e3\u5f84 include_missing=FALSE \u4ecd\u53ef\u7528\uff08\u53ef\u56de\u9000\uff09", {
  .adme_skip()
  w <- tcmnetpharma:::.tcm_adme_where(30, 0.18, include_missing = FALSE)
  expect_false(grepl("no_adme", w, fixed = TRUE))
  expect_false(grepl(" IS NULL", w, fixed = TRUE))
})
