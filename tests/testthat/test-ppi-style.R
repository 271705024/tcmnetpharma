# ppi_plot 的风格 / 可读性 / 可复现性契约（2026-10-21）
#
# 由来：boss 反馈「PPI 图能出但不美观，想接近 Cytoscape」→ 加了 7 种风格；
# 随后又报「PPI 结果输出有问题」（737 节点散成一堆孤点）。
# 排查出**三个独立成因**，这个文件把三条都锁死：
#   ① 节点不筛选 -> 737 节点在 kk 布局下必然散成"中心一团 + 外围孤点"
#      （default 曾因"保持原始行为"放行，已修：所有风格统一走 .ppi_cap_nodes）
#   ② fr 布局随机初始化 -> 同一数据两次出图一次密集一次散开
#      （已修：固定种子，同一数据必须完全可复现）
#   ③ 边表没有 combined_score -> aes(edge_width = score) 映射成 NA，整片边消失
#      （已修：has_score 兜底，退回固定线宽）

skip_if_no_ppi <- function() {
  p <- tryCatch(tcmnetpharma:::.tcm_parquet_path("string_ppi"), error = function(e) "")
  testthat::skip_if(!nzchar(p) || !file.exists(p), "STRING parquet 未构建")
}
skip_if_no_ppi()

make_edges <- function(n_hub = 12, n_leaf = 200, seed = 11) {
  set.seed(seed)
  hub <- paste0("HUB", seq_len(n_hub))
  leaf <- paste0("LEAF", seq_len(n_leaf))
  # hub 之间两两相连（保证强连通），再把每个 leaf 挂到一个 hub 上
  grid <- expand.grid(a = hub, b = hub, stringsAsFactors = FALSE)
  core <- data.frame(node1 = grid$a, node2 = grid$b,
                     combined_score = 900L, stringsAsFactors = FALSE)
  core <- core[core$node1 != core$node2, ]
  bind <- data.frame(node1 = leaf, node2 = sample(hub, n_leaf, replace = TRUE),
                     combined_score = 800L, stringsAsFactors = FALSE)
  out <- rbind(core, bind)
  stopifnot(!anyNA(out$node1), !anyNA(out$node2))   # 测试数据自身不能带 NA
  out
}

test_that("七个风格都接受，且输出都是 ggplot/ggraph", {
  e <- make_edges()
  for (st in c("default", "journal", "cytoscape", "presentation",
               "hub30", "module", "minimal")) {
    p <- suppressMessages(ppi_plot(e, style = st))
    expect_true(inherits(p, "ggplot"), info = st)
  }
  # 2026-10-26：style 拆成 view × style 两轴后，非法值改由显式校验拦截，
  # 报错文案从 match.arg 的 "should be one of" 改为中文说明 + 可选值清单
  # （同时列出旧名 hub30 / module）。
  expect_error(ppi_plot(e, style = "no_such_style"), "style")
})

test_that("所有风格的节点数都受 max_nodes 约束（default 也不例外）", {
  e <- make_edges(n_leaf = 400)
  n_in <- length(unique(c(e$node1, e$node2)))
  expect_gt(n_in, 150L)  # 前提：输入确实超限
  for (st in c("default", "journal", "cytoscape", "presentation", "module", "minimal")) {
    p <- suppressMessages(ppi_plot(e, style = st))
    expect_lte(nrow(p$data), 150L)
  }
  # hub30 自带更严的上限
  p30 <- suppressMessages(ppi_plot(e, style = "hub30"))
  expect_lte(nrow(p30$data), 30L)
})

test_that("同一数据两次出图坐标完全一致（可复现）", {
  e <- make_edges()
  p1 <- suppressMessages(ppi_plot(e, style = "cytoscape"))
  p2 <- suppressMessages(ppi_plot(e, style = "cytoscape"))
  expect_equal(p1$data$x, p2$data$x, tolerance = 1e-12)
  expect_equal(p1$data$y, p2$data$y, tolerance = 1e-12)
})

test_that("没有 combined_score 列也能画（退回固定线宽，边不能消失）", {
  e <- make_edges()
  e2 <- e[, c("node1", "node2")]
  p <- suppressMessages(ppi_plot(e2, style = "cytoscape"))
  expect_true(inherits(p, "ggplot"))
  # 边层必须有几何对象；若映射失败会只剩点层
  expect_true(length(p$layers) >= 2L)
})

test_that("combined_score 全为 NA 也不崩", {
  e <- make_edges()
  e$combined_score <- NA_real_
  p <- suppressMessages(ppi_plot(e, style = "journal"))
  expect_true(inherits(p, "ggplot"))
})

test_that("边少于节点上限时不做任何删减（不误伤小网络）", {
  e <- make_edges(n_hub = 3, n_leaf = 12)
  n_in <- length(unique(c(e$node1, e$node2)))
  expect_lte(n_in, 150L)
  p <- suppressMessages(ppi_plot(e, style = "cytoscape"))
  expect_equal(nrow(p$data), n_in)
})

# ---- 2026-10-23 P2：显式暴露 max_nodes / min_degree / keep 三个容量形参 ----
test_that("显式 max_nodes 覆盖默认 150（风格与容量解耦）", {
  e <- make_edges(n_leaf = 400)
  p50 <- suppressMessages(ppi_plot(e, style = "cytoscape", max_nodes = 50))
  expect_lte(nrow(p50$data), 50L)
  # max_nodes = Inf 回退为不裁剪（全量节点）
  p_inf <- suppressMessages(ppi_plot(e, style = "cytoscape", max_nodes = Inf))
  n_in <- length(unique(c(e$node1, e$node2)))
  expect_equal(nrow(p_inf$data), n_in)
})

test_that("keep = 'ego' 与 keep = 'top_degree' 产出的节点集不同但都受上限约束", {
  e <- make_edges(n_leaf = 400)
  p_top <- suppressMessages(ppi_plot(e, style = "cytoscape", max_nodes = 40, keep = "top_degree"))
  p_ego <- suppressMessages(ppi_plot(e, style = "cytoscape", max_nodes = 40, keep = "ego"))
  expect_lte(nrow(p_top$data), 40L)
  # ego 保留 top + 一跳邻居，节点数可略超上限但封顶 2x
  expect_lte(nrow(p_ego$data), 80L)
  expect_false(identical(sort(p_top$data$name), sort(p_ego$data$name)))
})

test_that("非法 keep 报错（与 tcm_filter_network 同口径）", {
  e <- make_edges()
  expect_error(suppressMessages(ppi_plot(e, keep = "bogus")), "should be one of")
})
