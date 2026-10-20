# tcm_ppi() 契约测试 —— STRING v12.0 physical subnetwork
#
# 定位：这是「交集靶点 → PPI 图」的桥梁。交集/查询结果只是**节点列表**，
# 而 ppi_plot 需要**配对**（node1/node2）。本函数负责这个转换。
#
# 断言重点不是"能跑"，而是：
#   ① 返回结构能直接喂 ppi_plot（node1/node2/combined_score）
#   ② 只返回**两端都在给定基因集内**的边（子网，不是邻居扩展）
#   ③ min_score 过滤是单调的
#   ④ 0 边时返回同结构 0 行（下游不必特判 NULL）
#   ⑤ 内容锚点：TP53–MDM2 是 STRING 里的高置信物理互作（不是偶然命中）
#
# 数据来源与许可见 docs/data-license-string-2026-10-20.md（CC BY 4.0）

skip_if_no_string <- function() {
  p <- tryCatch(TCMNP:::.tcm_parquet_path("string_ppi"), error = function(e) "")
  testthat::skip_if(!nzchar(p) || !file.exists(p),
                    "STRING parquet 未构建（先跑 data-raw/string/build_string_parquet.py）")
}
skip_if_no_string()

test_that("tcm_ppi: 返回结构可直接喂 ppi_plot", {
  e <- tcm_ppi(c("TP53", "MDM2", "BRCA1", "EGFR", "AKT1", "MTOR", "CDK1", "CCNB1"))
  expect_true(is.data.frame(e))
  expect_named(e, c("node1", "node2", "combined_score"))
  expect_gt(nrow(e), 0L)
  expect_type(e$node1, "character")
  expect_type(e$node2, "character")
  expect_true(is.numeric(e$combined_score))
  expect_true(all(e$combined_score >= 700))
})

test_that("tcm_ppi: 只返回两端都在输入集内的边（子网）", {
  genes <- c("TP53", "MDM2", "BRCA1", "EGFR", "AKT1", "MTOR")
  e <- tcm_ppi(genes)
  expect_true(all(e$node1 %in% genes))
  expect_true(all(e$node2 %in% genes))
})

test_that("tcm_ppi: 内容锚点 —— TP53–MDM2 是既有高置信互作", {
  e <- tcm_ppi(c("TP53", "MDM2"))
  expect_equal(nrow(e), 1L)
  pair <- sort(c(e$node1[[1]], e$node2[[1]]))
  expect_equal(pair, c("MDM2", "TP53"))
  expect_gt(e$combined_score[[1]], 700)
})

test_that("tcm_ppi: 边规范化 —— node1 < node2（无向去重的前提）", {
  e <- tcm_ppi(c("TP53", "MDM2", "BRCA1", "EGFR", "AKT1"))
  expect_true(all(e$node1 < e$node2))
  expect_false(anyDuplicated(paste(e$node1, e$node2)) > 0L)
})

test_that("tcm_ppi: min_score 单调（400 的边 ⊇ 700 的边）", {
  genes <- c("TP53", "MDM2", "BRCA1", "EGFR", "AKT1", "MTOR", "CDK1", "CCNB1",
             "CDK2", "RB1", "E2F1", "CCNA2")
  hi <- tcm_ppi(genes, min_score = 700)
  lo <- tcm_ppi(genes, min_score = 400)
  expect_gte(nrow(lo), nrow(hi))
  if (nrow(hi)) {
    key_hi <- paste(hi$node1, hi$node2)
    key_lo <- paste(lo$node1, lo$node2)
    expect_true(all(key_hi %in% key_lo))
  }
})

test_that("tcm_ppi: 0 边时返回同结构 0 行（下游不崩）", {
  e <- suppressMessages(tcm_ppi(c("NOTAGENE1", "NOTAGENE2")))
  expect_true(is.data.frame(e))
  expect_equal(nrow(e), 0L)
  expect_named(e, c("node1", "node2", "combined_score"))
})

test_that("tcm_ppi: 容忍 list 包装的输入（GUI 的 JSON 转 list）", {
  expect_no_error(
    e <- tcm_ppi(list("TP53", "MDM2"))
  )
  expect_equal(nrow(e), 1L)
})

test_that("tcm_ppi: 非法 min_score 给出可读错误", {
  expect_error(tcm_ppi(c("TP53", "MDM2"), min_score = 5000), "0-1000")
})
