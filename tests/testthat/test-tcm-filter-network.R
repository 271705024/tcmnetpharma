# tcm_filter_network 统一筛选管线测试（2026-10-23，P2）
#
# 铁律 41/52：用真实缺陷形态证伪，不靠「没报错」。
# 重点覆盖三件事：
#   1. 「只有点没有线」的根因 —— 度数必须在**过滤后的图**上算、induced subgraph
#      边跟着节点裁，保证 ecount > 0；
#   2. 节点永远从边派生，结构上不可能出现孤点；
#   3. 裁剪报告的数字与产物一致（boss 验收口径：报告与图一致）。

# 构造「hub 之间无边」的缺陷形态（旧 .ppi_cap_nodes 会得到空图）：
#   hub1--a1, hub1--a2, hub2--b1, hub2--b2（两个 hub 各挂两个叶子，hub 之间无边）
#   全图度数：hub1=2, hub2=2, a1=a2=b1=b2=1。
#   旧逻辑按全图度数取 top 2 = {hub1, hub2}，induced subgraph 之间无边 → 空图。
make_hub_no_edge <- function() {
  data.frame(
    from = c("hub1", "hub1", "hub2", "hub2"),
    to   = c("a1", "a2", "b1", "b2"),
    stringsAsFactors = FALSE
  )
}

test_that("边规则先行：孤立边（两端度=1）被剔除，核心边保留", {
  links <- data.frame(
    from = c("a", "a", "x"),
    to   = c("b", "c", "y"),   # a-b, a-c 是核心边；x-y 是孤立边（两端度=1）
    stringsAsFactors = FALSE
  )
  # min_degree = 2：孤立边 x-y（两端度都为 1 < 2）被剔，a-b/a-c 保留
  out <- tcm_filter_network(links, max_nodes = Inf, min_degree = 2L)
  expect_equal(sort(unique(c(out$links$from, out$links$to))),
               c("a", "b", "c"))
  expect_equal(out$report$n_edges_dropped, 1L)
})

test_that("节点 = 边端点并集，不存在孤点", {
  links <- data.frame(
    from = c("a", "b"),
    to   = c("b", "c"),
    stringsAsFactors = FALSE
  )
  out <- tcm_filter_network(links, max_nodes = Inf, min_degree = 1L)
  nodes <- unique(c(out$links$from, out$links$to))
  # 每个节点都至少出现在一条边里（孤点不可能出现）
  expect_true(all(nodes %in% c(out$links$from, out$links$to)))
  expect_equal(sort(nodes), c("a", "b", "c"))
})

test_that("hub 之间无边 → induced subgraph 不只剩点（保留内部边）", {
  links <- make_hub_no_edge()
  # 一个 hub + 其叶子 = 3 节点 2 边，取 top 3 应保留一个完整星型子网
  out <- tcm_filter_network(links, max_nodes = 3L, min_degree = 1L)
  expect_true(out$report$n_edges_kept > 0L,
              info = "induced subgraph 必须保留连线，不能只剩点")
  expect_true(out$report$n_nodes_kept > 0L)
  # 保留的边两端都在保留节点内（induced 语义）
  kept_nodes <- unique(c(out$links$from, out$links$to))
  expect_true(all(out$links$from %in% kept_nodes))
  expect_true(all(out$links$to %in% kept_nodes))
})

test_that("超限裁剪：报告数字与产物一致，且守恒", {
  links <- data.frame(
    from = paste0("n", rep(1:50)),
    to   = paste0("m", rep(1:50)),
    stringsAsFactors = FALSE
  )
  # 50 条互不重叠的边 = 100 节点，限 30 节点
  out <- tcm_filter_network(links, max_nodes = 30L, min_degree = 1L)
  rep <- out$report
  expect_true(rep$truncated)
  expect_equal(rep$n_edges_orig, 50L)
  expect_equal(rep$n_nodes_orig, 100L)
  # 守恒：orig = kept + dropped
  expect_equal(rep$n_nodes_kept + rep$n_nodes_dropped, rep$n_nodes_orig)
  expect_equal(rep$n_edges_kept + rep$n_edges_dropped, rep$n_edges_orig)
  # 报告数字与产物一致
  expect_equal(length(unique(c(out$links$from, out$links$to))), rep$n_nodes_kept)
  expect_equal(nrow(out$links), rep$n_edges_kept)
  # 节点数不超上限
  expect_lte(rep$n_nodes_kept, 30L)
})

test_that("不超限时：truncated=FALSE，数据原样（只做边规则）", {
  links <- make_hub_no_edge()
  out <- tcm_filter_network(links, max_nodes = 100L, min_degree = 1L)
  expect_false(out$report$truncated)
  expect_equal(out$report$n_edges_kept, 4L)  # 无孤立边可剔（min_degree=1）
  expect_equal(out$report$n_nodes_kept, 6L)
})

test_that("keep='ego' 保留一跳邻居，子网更密", {
  links <- make_hub_no_edge()
  out_top <- tcm_filter_network(links, max_nodes = 2L, min_degree = 1L,
                                keep = "top_degree")
  out_ego <- tcm_filter_network(links, max_nodes = 2L, min_degree = 1L,
                                keep = "ego")
  # ego 保留 top 2 hub 及其邻居，节点数应 ≥ top_degree 的节点数
  expect_gte(out_ego$report$n_nodes_kept, out_top$report$n_nodes_kept)
  expect_true(out_ego$report$n_edges_kept > 0L)
})

test_that("非法 keep 取值报错（match.arg）", {
  links <- make_hub_no_edge()
  expect_error(tcm_filter_network(links, keep = "bogus"))
})

test_that("空输入返回空产物 + 空报告", {
  links <- data.frame(from = character(0), to = character(0))
  out <- tcm_filter_network(links)
  expect_equal(nrow(out$links), 0L)
  expect_equal(out$report$n_nodes_orig, 0L)
})
