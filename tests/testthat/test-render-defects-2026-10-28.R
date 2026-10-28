# =============================================================================
# ppi_plot 渲染层缺陷回归（2026-10-28）
#
# 对应 ISSUE-ppi_plot-render-defects-2026-10-28.md 的 D1–D5。
# 每条断言都写成「能被修复前实现证伪」的形式，注释里写明旧实现的实际取值。
# 规则（铁律 52/53/91）：用确定性的合成图，不依赖随机布局的偶然结果。
# =============================================================================

# ---- 共用辅助 --------------------------------------------------------------
.rd_node_layer <- function(b) {
  which(vapply(b$data, function(l)
    all(c("shape", "size") %in% names(l)) && !"edge_colour" %in% names(l),
    logical(1)))[1]
}
.rd_nodes <- function(p) {
  b <- ggplot2::ggplot_build(p)
  nrow(b$data[[.rd_node_layer(b)]])
}
.rd_n_legends <- function(p) {
  gt <- ggplot2::ggplotGrob(p)
  i <- grep("^guide-box-right$", gt$layout$name)
  if (!length(i)) return(0L)
  g <- gt$grobs[[i]]
  if (!length(g$grobs)) return(0L)
  sum(!vapply(g$grobs, function(z) inherits(z, "zeroGrob"), logical(1)))
}
.rd_ppi_data <- function() as.data.frame(tcmnetpharma::ppi_data)

# 确定性合成图：n 个节点的路径 1-2-3-…-n（度数 1,2,…,2,1）
.rd_path_edges <- function(n) {
  data.frame(node1 = as.character(seq_len(n - 1L)),
             node2 = as.character(seq_len(n - 1L) + 1L),
             stringsAsFactors = FALSE)
}

# 主簇（n 节点优先连接图）+ 3 节点孤立三角
.rd_two_components <- function(n = 100L) {
  set.seed(20261028)
  g <- igraph::sample_pa(n, power = 1.1, directed = FALSE)
  e <- igraph::as_data_frame(g, what = "edges")[, 1:2]
  names(e) <- c("node1", "node2")
  tri <- data.frame(node1 = c("ISO1", "ISO2", "ISO3"),
                    node2 = c("ISO2", "ISO3", "ISO1"),
                    stringsAsFactors = FALSE)
  rbind(e, tri)
}

# ---- D1：min_degree 在 max_nodes 未超限时仍然生效 -------------------------
test_that("D1 min_degree 与 max_nodes 正交：路径图 20 节点 → 18", {
  d <- .rd_path_edges(20)
  # 旧实现：max_nodes = 1000 > 20 → .tcm_net_cap 提前 return，
  # min_degree 从未被使用 → 仍是 20 个节点，且**没有任何提示**。
  p <- suppressMessages(ppi_plot(d, min_degree = 2, max_nodes = 1000))
  expect_equal(.rd_nodes(p), 18L)
  # 不传 min_degree 时全保留（对照，证明上面不是被别的通道筛掉的）
  expect_equal(.rd_nodes(suppressMessages(ppi_plot(d, max_nodes = 1000))), 20L)
  # max_nodes = Inf 同样生效（「不截断、只按度数筛」的主用法）
  expect_equal(.rd_nodes(suppressMessages(
    ppi_plot(d, min_degree = 2, max_nodes = Inf))), 18L)
})

test_that("D1 report 增加 floored / floor_dropped，且口径自洽", {
  d <- .rd_path_edges(20)
  net <- igraph::graph_from_data_frame(d, directed = FALSE)
  igraph::V(net)$degree <- igraph::degree(net)
  capt <- tcmnetpharma:::.tcm_net_cap(net, view = "overview",
                                      max_nodes = 1000, min_degree = 2)
  expect_true(isTRUE(capt$report$floored))
  expect_false(isTRUE(capt$report$capped))
  expect_equal(capt$report$total_nodes, 20L)
  expect_equal(capt$report$shown_nodes, 18L)
  expect_equal(capt$report$floor_dropped, 2L)
  # 拓扑相关属性不能留在子图上（否则下游沿用全图度数）
  expect_null(igraph::V(capt$net)$degree)
  # 未触发下限时 floored 必须是 FALSE（否则 message 会乱报）
  capt0 <- tcmnetpharma:::.tcm_net_cap(net, view = "overview",
                                       max_nodes = 1000, min_degree = 0)
  expect_false(isTRUE(capt0$report$floored))
  expect_equal(capt0$report$floor_dropped, 0L)
})

test_that("D1 被下限筛掉节点时必须给出 message（不静默减点）", {
  d <- .rd_path_edges(20)
  msg <- paste(testthat::capture_messages(
    ppi_plot(d, min_degree = 2, max_nodes = 1000)), collapse = "")
  expect_match(msg, "min_degree")
  expect_match(msg, "18")
})

test_that("D1 min_degree 高于全图最大度数 → 明确报错，不静默退回全图", {
  d <- .rd_path_edges(20)
  expect_error(suppressMessages(ppi_plot(d, min_degree = 99, max_nodes = 1000)),
               "min_degree")
})

# ---- 2026-10-28 追加：清孤点（默认）+ k-core 迭代（开关）------------------
test_that("单次下限后清掉失联孤点（113→112 的口径），不级联", {
  # K4（a/b/c/d 各 3 度）+ X 挂着 3 个叶子（l1..l3 各 1 度）
  # min_degree = 3：第一遍留下 K4 + X；X 的邻居全被筛掉 → X 成孤点 → 一并剔除
  # 旧行为（只做 induceed subgraph）会留下 5 个节点，其中 1 个是孤点。
  e <- rbind(
    data.frame(node1 = c("a", "a", "a", "b", "b", "c"),
               node2 = c("b", "c", "d", "c", "d", "d")),
    data.frame(node1 = "X", node2 = paste0("l", 1:3))
  )
  net <- igraph::graph_from_data_frame(e, directed = FALSE)
  igraph::V(net)$degree <- igraph::degree(net)
  capt <- tcmnetpharma:::.tcm_net_cap(net, view = "overview",
                                      max_nodes = 1000, min_degree = 3)
  expect_equal(igraph::vcount(capt$net), 4L)      # 只剩 K4
  expect_equal(capt$report$floor_isolated, 1L)    # X
  expect_equal(capt$report$floor_rounds, 0L)      # 单次模式不记轮数
  expect_equal(capt$report$floor_dropped, 4L)     # l1..l3 + X
  expect_equal(sum(igraph::degree(capt$net) == 0), 0L)
})

test_that("min_degree_iterate=TRUE 迭代到稳定（k-core），轮数与节点数都变", {
  # 同一张图：单次 3 个（A B C 已是 2-core，稳定）；换一张会级联的：
  # 三角 + 长尾 A-D-E-F（D3 E2 F1）→ 单次留 A B C D E，迭代把 D、E 也筛掉
  e <- data.frame(
    node1 = c("A", "B", "A", "A", "D", "E"),
    node2 = c("B", "C", "C", "D", "E", "F"),
    stringsAsFactors = FALSE
  )
  net <- igraph::graph_from_data_frame(e, directed = FALSE)
  igraph::V(net)$degree <- igraph::degree(net)
  single <- tcmnetpharma:::.tcm_net_cap(net, view = "overview",
                                        max_nodes = 1000, min_degree = 2)
  iter <- tcmnetpharma:::.tcm_net_cap(net, view = "overview",
                                      max_nodes = 1000, min_degree = 2,
                                      min_degree_iterate = TRUE)
  expect_gt(igraph::vcount(single$net), igraph::vcount(iter$net))
  expect_equal(igraph::vcount(iter$net), 3L)      # 只剩三角
  expect_gte(iter$report$floor_rounds, 1L)
  expect_true(all(igraph::degree(iter$net) >= 2)) # k-core 属性
  # 迭代模式没有"孤点"可清（稳定后每个点都 ≥ min_degree）
  expect_equal(iter$report$floor_isolated, 0L)
})

test_that("下限把整张图筛空时报错，而不是给一张空白图", {
  # 星型：中心 6 度、叶子 1 度 → min_degree=2 的 2-core 为空
  e <- data.frame(node1 = rep("hub", 6), node2 = paste0("l", 1:6),
                  stringsAsFactors = FALSE)
  expect_error(suppressMessages(ppi_plot(e, min_degree = 2, max_nodes = 1000)),
               "min_degree")
})

test_that("message 交代下限剔除细节（孤点个数 / 迭代轮数）", {
  # 与上一条同款 K4+X+叶子：孤点 X 会被清掉，message 必须说明
  e <- rbind(
    data.frame(node1 = c("a", "a", "a", "b", "b", "c"),
               node2 = c("b", "c", "d", "c", "d", "d")),
    data.frame(node1 = "X", node2 = paste0("l", 1:3))
  )
  m1 <- paste(testthat::capture_messages(
    ppi_plot(e, min_degree = 3, max_nodes = 1000)), collapse = "")
  expect_match(m1, "min_degree")
  expect_match(m1, "因失去全部连线被剔除")

  # 级联用例：三角 + A-D + D-E + E-F（md=2 单次留 5、迭代留 3）
  e2 <- data.frame(
    node1 = c("A", "B", "A", "A", "D", "E"),
    node2 = c("B", "C", "C", "D", "E", "F"),
    stringsAsFactors = FALSE
  )
  m2 <- paste(testthat::capture_messages(
    ppi_plot(e2, min_degree = 2, max_nodes = 1000,
             min_degree_iterate = TRUE)), collapse = "")
  expect_match(m2, "k-core")
  expect_match(m2, "轮")
})

test_that("D1 真实数据：degree>=4 的节点数作为期望值（72 节点 ppi_data）", {
  d <- .rd_ppi_data()
  deg <- as.integer(table(c(d[[1]], d[[2]]))[unique(c(d[[1]], d[[2]]))])
  n_ge4 <- sum(deg >= 4)
  expect_lt(n_ge4, length(deg))                 # 保证这个 fixture 有意义
  p <- suppressMessages(ppi_plot(d, min_degree = 4, max_nodes = 1000))
  expect_equal(.rd_nodes(p), n_ge4)
})

# ---- D2：布局坐标归一化 ---------------------------------------------------
test_that("D2 .tcm_normalize_layout 是尺度/平移不变的，并把画布归到 [-1,1]", {
  raw <- data.frame(x = c(-10, 0, 30, 30), y = c(0, 40, -20, 60))
  a <- tcmnetpharma:::.tcm_normalize_layout(raw)
  b <- tcmnetpharma:::.tcm_normalize_layout(
    data.frame(x = raw$x * 7 + 123, y = raw$y * 7 - 55))
  expect_equal(a$x, b$x, tolerance = 1e-12)
  expect_equal(a$y, b$y, tolerance = 1e-12)
  # 中心在原点、最大绝对坐标 = 1
  expect_equal(mean(range(a$x)), 0, tolerance = 1e-12)
  expect_equal(mean(range(a$y)), 0, tolerance = 1e-12)
  expect_equal(max(abs(c(a$x, a$y))), 1, tolerance = 1e-12)
  # 相对几何不变：两两距离整体等比（缩放不改变形状）
  d_raw <- as.matrix(stats::dist(cbind(raw$x, raw$y)))
  d_new <- as.matrix(stats::dist(cbind(a$x, a$y)))
  expect_equal(d_new / max(d_new), d_raw / max(d_raw), tolerance = 1e-10)
})

test_that("D2 多分量网络：主簇占画面宽度 >= 70%（旧实现会塌缩）", {
  d <- .rd_two_components(100)
  p <- suppressMessages(ppi_plot(d, max_nodes = 1000, style = "journal"))
  lay <- p$data
  expect_equal(nrow(lay), 103L)
  isl <- lay$name %in% c("ISO1", "ISO2", "ISO3")
  frac <- diff(range(lay$x[!isl])) / diff(range(lay$x))
  # 旧实现：主簇横跨 17.1 / 全图 17.1 → 但整幅画布由 max(abs) 决定，
  # 存在远处分量时主簇被压到 15% 量级；归一化后画布恒为 [-1,1]。
  expect_gte(frac, 0.70)
  # 坐标必须落在 [-1, 1]，否则 coord_fixed 下又会塌缩
  expect_lte(max(abs(c(lay$x, lay$y))), 1 + 1e-9)
})

test_that("D2 单连通分量：归一化后形态比例不变（近似恒等）", {
  d <- .rd_path_edges(40)
  p <- suppressMessages(ppi_plot(d, max_nodes = 1000, style = "journal"))
  lay <- p$data
  expect_equal(max(abs(c(lay$x, lay$y))), 1, tolerance = 1e-9)
  # 路径图的 fr 布局近似一条折线：长宽比应与「归一化后再归一化」一致
  r1 <- diff(range(lay$x)) / diff(range(lay$y))
  again <- tcmnetpharma:::.tcm_normalize_layout(lay[, c("x", "y")])
  r2 <- diff(range(again$x)) / diff(range(again$y))
  expect_equal(r1, r2, tolerance = 1e-9)
})

test_that("D2 卫星分量大小随节点数单调（记录两处下标错位的回归）", {
  # 60 节点主分量 + 5 / 7 / 11 节点三条链。旧实现两处错位：
  #   ① `area` 的数值顺序与赋给它的名字顺序不一致（排过序的 ids 配没排序的计数）
  #   ② `member_idx[[k]]` 用整数下标取到「第 k 大」的分量而不是编号 k 的分量
  # 两处都会让卫星大小与节点数反序（实测 n=5 最大、n=11 最小）。
  e <- rbind(
    data.frame(node1 = as.character(1:59), node2 = as.character(2:60)),
    data.frame(node1 = paste0("a", 1:4), node2 = paste0("a", 2:5)),
    data.frame(node1 = paste0("b", 1:6), node2 = paste0("b", 2:7)),
    data.frame(node1 = paste0("c", 1:10), node2 = paste0("c", 2:11))
  )
  p <- suppressMessages(ppi_plot(e, max_nodes = 1000, style = "journal"))
  lay <- p$data
  span <- function(prefix) {
    s <- lay[startsWith(lay$name, prefix), ]
    diff(range(s$x))
  }
  expect_lt(span("a"), span("b"))
  expect_lt(span("b"), span("c"))
  # 主分量必须显著大于每个卫星分量
  expect_gt(diff(range(lay$x[!grepl("^[abc]", lay$name)])),
            max(span("a"), span("b"), span("c")))
  # 画布长宽比贴近方形：卫星带排歪了就会拉成细高/细长
  asp <- diff(range(lay$x)) / diff(range(lay$y))
  expect_gte(asp, 0.6)
  expect_lte(asp, 1.6)
})

# ---- D3：显式 max_nodes 优先于 view 预设 ----------------------------------
test_that("D3 view=hub 的 30 是默认值，可被显式 max_nodes 覆盖", {
  d <- .rd_ppi_data()
  expect_equal(.rd_nodes(suppressMessages(ppi_plot(d, view = "hub"))), 30L)
  # 旧实现：cap_nodes 被 vp$max_nodes = 30 无条件覆盖 → 仍是 30，参数被丢弃
  expect_equal(.rd_nodes(suppressMessages(
    ppi_plot(d, view = "hub", max_nodes = 72))), 72L)
  expect_equal(.rd_nodes(suppressMessages(
    ppi_plot(d, view = "hub", max_nodes = 1000))), 72L)
  # 显式要更小的上限时也照办
  expect_equal(.rd_nodes(suppressMessages(
    ppi_plot(d, view = "hub", max_nodes = 20))), 20L)
})

test_that("D3 大图 + view=hub + 显式上限：不再固定 30", {
  d <- .rd_path_edges(200)
  expect_equal(.rd_nodes(suppressMessages(
    ppi_plot(d, view = "hub", max_nodes = 150))), 150L)
  expect_equal(.rd_nodes(suppressMessages(ppi_plot(d, view = "hub"))), 30L)
})

# ---- 2026-10-28 追加：hub 少节点 / 报告与图一致 ---------------------------
test_that("裁剪 message 报的是图上实际节点数（不是上限）", {
  # 两个分量各一个高 degree 核心，上限 3 只够留下 A、B 与 C；C 与 A/B 无边
  e <- data.frame(
    node1 = c("A", "A", "A", "A", "B", "B", "B", "C", "C"),
    node2 = c("a1", "a2", "a3", "B", "b1", "b2", "b3", "c1", "c2"),
    stringsAsFactors = FALSE
  )
  msg <- paste(testthat::capture_messages(ppi_plot(e, max_nodes = 3)), collapse = "")
  p <- suppressMessages(ppi_plot(e, max_nodes = 3))
  n <- .rd_nodes(p)
  # 旧实现：message 一律写「保留前 3 个」，而图上只有 2 个
  expect_equal(n, 2L)
  expect_match(msg, "保留 2 个")
  expect_match(msg, "1 个节点在保留子集内无连线已剔除")
})

test_that("上限小到保留节点之间无边时明确报错，而不是给一张空白图", {
  e <- data.frame(node1 = c("A", "A", "B", "B"),
                  node2 = c("a1", "a2", "b1", "b2"),
                  stringsAsFactors = FALSE)
  expect_error(suppressMessages(ppi_plot(e, max_nodes = 2)), "max_nodes")
})

test_that("度数打平时选点确定（正序/反序输入给出同一节点集）", {
  d <- .rd_ppi_data()
  d_rev <- d[rev(seq_len(nrow(d))), , drop = FALSE]
  names_of <- function(x) {
    p <- suppressMessages(ppi_plot(x, max_nodes = 30))
    sort(as.character(p$data$name))
  }
  expect_identical(names_of(d), names_of(d_rev))
})

# ---- D4：图例收敛 --------------------------------------------------------
test_that("D4 legend 三态：auto 只出颜色图例，all 出两份，none 全关", {
  d <- .rd_ppi_data()
  expect_equal(.rd_n_legends(suppressMessages(ppi_plot(d, view = "degree"))), 1L)
  # 2026-10-28 追加：presentation 默认由 "all" 改 "auto" → 1 份；
  # 两份图例改为显式 legend = "all" 才出（发表图只要一份色条）。
  expect_equal(.rd_n_legends(suppressMessages(
    ppi_plot(d, view = "degree", style = "presentation"))), 1L)
  expect_equal(.rd_n_legends(suppressMessages(
    ppi_plot(d, view = "degree", style = "presentation", legend = "all"))), 2L)
  expect_equal(.rd_n_legends(suppressMessages(
    ppi_plot(d, view = "degree", style = "journal"))), 0L)
  expect_equal(.rd_n_legends(suppressMessages(
    ppi_plot(d, view = "degree", style = "minimal"))), 0L)
})

test_that("D4 auto 模式下 size 标度的 guide 为 none（不出残影图例）", {
  d <- .rd_ppi_data()
  .size_guide <- function(p) {
    i <- which(vapply(p$scales$scales,
                      function(s) "size" %in% s$aesthetics, logical(1)))
    expect_length(i, 1L)
    p$scales$scales[[i[1]]]$guide
  }
  p_auto <- suppressMessages(ppi_plot(d, view = "degree"))
  expect_identical(.size_guide(p_auto), "none")
  # "all" 时必须是**可见的** guide 对象：旧做法给字符串 "legend"，ringed 节点的
  # fill 在图例里没有色值 → 画出一排白点（白底上看就是「只有数字没有键」）。
  g_all <- .size_guide(suppressMessages(
    ppi_plot(d, view = "degree", style = "presentation", legend = "all")))
  expect_false(identical(g_all, "none"))
  expect_true(inherits(g_all, "guide") || inherits(g_all, "ggproto"))
  # ggplot2 4.x 把 override.aes 收在 guide$params 下
  expect_equal(g_all$params$override.aes$fill, "grey40")
})

test_that("D4 三面板拼图 guides=collect 后只剩 1 份图例", {
  skip_if_not_installed("patchwork")
  d <- .rd_ppi_data()
  pA <- suppressMessages(ppi_plot(d, view = "degree"))
  pB <- suppressMessages(ppi_plot(d, view = "degree", style = "cytoscape"))
  pC <- suppressMessages(ppi_plot(d, view = "degree"))
  pw <- (pA | pB | pC) + patchwork::plot_layout(guides = "collect")
  gt <- ggplot2::ggplotGrob(pw)
  i <- grep("^guide-box-right$", gt$layout$name)
  expect_length(i, 1L)
  n <- sum(!vapply(gt$grobs[[i]]$grobs,
                   function(z) inherits(z, "zeroGrob"), logical(1)))
  expect_equal(n, 1L)
})

# ---- D5：PPI 边表 → 中心性 ----------------------------------------------
test_that("D5 input=edge 可算中心性，且与 igraph 逐值一致", {
  d <- .rd_ppi_data()
  cen <- tcm_centrality(d, input = "edge")
  expect_true(all(c("node", "class", "degree", "betweenness", "closeness",
                    "coreness") %in% names(cen)))
  expect_true(all(is.na(cen$class)))          # 边表没有角色信息
  g <- igraph::graph_from_data_frame(d[, 1:2], directed = FALSE)
  # 默认口径 = igraph 默认（原始介数，与 CytoNCA 的 BC 对齐）
  ref <- igraph::betweenness(g)
  names(ref) <- igraph::V(g)$name
  expect_equal(cen$betweenness, unname(ref[cen$node]), tolerance = 1e-9)
  expect_equal(cen$degree, unname(igraph::degree(g)[cen$node]))
  expect_equal(cen$coreness, unname(igraph::coreness(g)[cen$node]))
  # normalized = TRUE 时与 igraph 归一化口径逐值一致
  cen_n <- tcm_centrality(d, input = "edge", normalized = TRUE)
  ref_n <- igraph::betweenness(g, normalized = TRUE)
  names(ref_n) <- igraph::V(g)$name
  expect_equal(cen_n$betweenness, unname(ref_n[cen_n$node]), tolerance = 1e-9)
  expect_lte(max(cen_n$betweenness), 1 + 1e-12)
  # 按 betweenness 降序（与三条目输入口径一致）
  expect_true(isTRUE(all.equal(cen$betweenness,
                               sort(cen$betweenness, decreasing = TRUE))))
})

test_that("D5 input=edge 时 class 过滤必须报错（而不是静默 0 行）", {
  d <- .rd_ppi_data()
  expect_error(tcm_centrality(d, input = "edge", class = "target"), "class")
})

test_that("D5 input 默认仍是 triple：三层网络报错文本指向 input=edge", {
  d <- .rd_ppi_data()
  expect_error(tcm_centrality(d), 'input = "edge"')
  expect_error(tcm_modules(d), 'input = "edge"')
})

test_that("D5 tcm_modules(input=edge) 可跑，top_targets 不出现 \"NA\"", {
  d <- .rd_ppi_data()
  res <- tcm_modules(d, input = "edge")
  expect_true(all(c("node", "class", "module", "coreness") %in% names(res$nodes)))
  expect_true(all(is.na(res$nodes$class)))
  expect_false(any(grepl("NA", res$modules$top_targets, fixed = TRUE)))
  expect_gt(nrow(res$modules), 0L)
})

test_that("D5 0 行边表：返回 0 行且列契约完整", {
  empty <- data.frame(node1 = character(0), node2 = character(0),
                      combined_score = numeric(0), stringsAsFactors = FALSE)
  cen <- tcm_centrality(empty, input = "edge")
  expect_equal(nrow(cen), 0L)
  expect_setequal(names(cen), c("node", "class", "degree", "betweenness",
                                "closeness", "coreness"))
})

test_that("D5 边表列名无法识别时报错并列出候选列", {
  bad <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  expect_error(tcm_centrality(bad, input = "edge"), "node1")
})

test_that("D5 tcm_ppi 输出（node1/node2/combined_score）可直接进中心性", {
  d <- .rd_ppi_data()
  expect_setequal(names(d), c("node1", "node2", "combined_score"))
  cen <- tcm_centrality(d, input = "edge", top_n = 5)
  expect_equal(nrow(cen), 5L)
})
