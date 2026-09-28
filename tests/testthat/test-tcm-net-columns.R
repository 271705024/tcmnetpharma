# =============================================================================
# tcm_net 三元组列契约回归测试
#
# 背景（2026-09-28 GUI 实测定位）：
#   `tcm_net()` 用 `dplyr::select(herb, molecule, target)` **按列名**构图，
#   却用 `network.data[, 2]`/`[, 3]` **按列位置**给 `V(net)$class` 取值。
#   GUI 查询结果含第 4 列 `molecule_id` 时两者错位 →
#   igraph 报 `Length of new attribute value must be 1 or 888, ..., not 887.`
#   四种查询结果全部触发；而内置 `network_data` 恰好三列，所以既有测试全绿
#   —— 「测试没覆盖真实输入形状」的典型缺陷（见 MEMORY 铁律 28）。
#
# 本文件即该缺陷的**证伪测试**：T1/T2 在修复前必然 FAIL。
# =============================================================================

test_that("T1 tcm_net 接受含多余列的查询结果（原报 not 887）", {
  d <- target_herb("PTGS1", OB.value = 30, DL.value = 0.18)
  expect_true(all(c("herb", "molecule", "target") %in% names(d)))
  expect_gt(ncol(d), 3L)   # 确含 molecule_id，即真实触发形状
  expect_s3_class(tcm_net(d), "ggplot")
})

test_that("T2 tcm_net 对列顺序不敏感（按名取列，不按位置）", {
  d <- target_herb("PTGS1", OB.value = 30, DL.value = 0.18)
  shuffled <- d[, rev(names(d)), drop = FALSE]
  expect_s3_class(tcm_net(shuffled), "ggplot")
  expect_s3_class(tcm_net(shuffled, graph.layout = "fr"), "ggplot")
})

test_that("T3 缺列必须明确报错（不静默按位置猜）", {
  d <- target_herb("PTGS1", OB.value = 30, DL.value = 0.18)[, c("herb", "molecule"), drop = FALSE]
  expect_error(tcm_net(d), "network\\.data")
})

test_that("T4 内置三列数据仍正常（行为不变）", {
  expect_s3_class(tcm_net(as.data.frame(network_data)), "ggplot")
})

test_that("T5 rem.dis.inter=TRUE 分支同样通过列契约", {
  d <- target_herb("PTGS1", OB.value = 30, DL.value = 0.18)
  expect_s3_class(tcm_net(d, rem.dis.inter = TRUE), "ggplot")
})
