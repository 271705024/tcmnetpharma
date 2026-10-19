# =============================================================================
# G1 拓扑模块识别 —— R 层列契约 + 空分支回归测试
#
# 背景（MEMORY 铁律 28：「无检查项 ≠ 通过」）：
#   中心性 / 模块识别是 G1 新功能，必须断言**列契约**（用户/下游依赖的列名
#   与类型），而非只看「函数没报错」。同时必须覆盖空输入、缺列等边界分支，
#   否则 GUI 接上时遇到真实查询结果形状会静默失败。
#
# 本文件覆盖范围：tcm_centrality / tcm_modules / module_barplot / module_netplot
# =============================================================================

test_that("T1 tcm_centrality 列契约 + 按 betweenness 降序", {
  cen <- tcm_centrality(network_data)
  expect_s3_class(cen, "data.frame")
  expect_true(all(c("node", "class", "degree", "betweenness", "closeness") %in% names(cen)))
  expect_gt(nrow(cen), 0)
  # betweenness 降序
  expect_true(isTRUE(all.equal(cen$betweenness, sort(cen$betweenness, decreasing = TRUE))))
  # class 取值受限于 herb/molecule/target/other
  expect_true(all(cen$class %in% c("herb", "molecule", "target", "other")))
})

test_that("T2 tcm_centrality class 过滤（仅 target）", {
  cen_t <- tcm_centrality(network_data, class = "target", top_n = 10)
  expect_true(all(cen_t$class == "target"))
  expect_lte(nrow(cen_t), 10)
  # 无效 class 必须明确报错（同 T4：match.arg 的实际消息是
  # "'arg' should be one of ..."，不含参数名 "class"）
  expect_error(tcm_centrality(network_data, class = "bad"), "should be one of")
})

test_that("T3 tcm_modules 结构契约 + MCODE 评分 ∈ [0,1]", {
  res <- tcm_modules(network_data, algorithm = "louvain")
  expect_type(res, "list")
  expect_named(res, c("nodes", "modules"))
  expect_s3_class(res$nodes, "data.frame")
  expect_s3_class(res$modules, "data.frame")
  expect_true(all(c("node", "class", "module", "degree", "betweenness",
    "closeness", "coreness", "mcode_score") %in% names(res$nodes)))
  expect_true(all(c("module", "size", "hub", "top_targets", "density",
    "mean_mcode") %in% names(res$modules)))
  # MCODE 风格评分严格落在 [0,1]
  expect_true(all(res$nodes$mcode_score >= 0 & res$nodes$mcode_score <= 1))
  # module 为非负整数
  expect_true(all(res$nodes$module >= 0 & res$nodes$module == as.integer(res$nodes$module)))
  # coreness 非负整数
  expect_true(all(res$nodes$coreness >= 0))
  # 模块汇总按 size 降序
  expect_true(isTRUE(all.equal(res$modules$size, sort(res$modules$size, decreasing = TRUE))))
})

test_that("T4 三种社区发现算法均产出模块", {
  for (algo in c("louvain", "infomap", "walktrap")) {
    res <- tcm_modules(network_data, algorithm = algo)
    expect_gt(nrow(res$nodes), 0)
    expect_gt(nrow(res$modules), 0)
    expect_true(all(c("node", "class", "module", "degree", "betweenness",
      "closeness", "coreness", "mcode_score") %in% names(res$nodes)))
  }
  # 无效算法必须明确报错（⚠️ 断言用实际消息片段：match.arg 的报错是
  # "'arg' should be one of ..." —— 不含 "algorithm" 字样，用 "algorithm" 会匹配失败）
  expect_error(tcm_modules(network_data, algorithm = "bad"), "should be one of")
})

test_that("T5 空输入不崩（返回带列名的空 data.frame）", {
  empty_nd <- network_data[0, ]
  e_cen <- tcm_centrality(empty_nd)
  expect_equal(nrow(e_cen), 0)
  expect_true(all(c("node", "class", "degree", "betweenness", "closeness") %in% names(e_cen)))
  e_mod <- tcm_modules(empty_nd)
  expect_equal(nrow(e_mod$nodes), 0)
  expect_equal(nrow(e_mod$modules), 0)
  expect_true(all(c("node", "class", "module", "degree", "betweenness",
    "closeness", "coreness", "mcode_score") %in% names(e_mod$nodes)))
})

test_that("T6 缺列必须明确报错（不静默按位置猜）", {
  bad <- network_data[, c("herb", "molecule"), drop = FALSE]
  expect_error(tcm_centrality(bad), "network")
  expect_error(tcm_modules(bad), "network")
})

test_that("T7 绘图函数返回 ggplot 对象", {
  res <- tcm_modules(network_data, algorithm = "louvain")
  p1 <- module_barplot(res$modules)
  p2 <- module_netplot(network_data, algorithm = "louvain")
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})
