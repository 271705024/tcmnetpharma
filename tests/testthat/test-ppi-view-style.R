# ppi_plot：view × style 两轴拆分 + 裁剪属性丢失修复（2026-10-26）
#
# 断言一律写成「能被旧实现证伪」的形式：每条注释写明旧实现的实际取值。
#
# 旧实现的三个问题（见 docs/plot-module-audit-2026-10-24.md §12）：
#   1. 一个 7 值 `style` 枚举混装了两个正交维度：`hub30` 偷偷改 max_nodes、
#      `module` 偷偷改 colour 通道（都是数据轴），其余 5 个只改外观。
#      → 「module + 大字号」「hub + 无图例」这类组合无法表达。
#   2. `hub30` **100% 渲染失败**：`.ppi_cap_nodes` 走 `.tcm_filter_igraph`
#      从边表重建图 → 顶点属性 `size` 丢失 → `aes(size = size)` 报
#      `object 'size' not found`。ppi_data 72 节点，只有默认 max_nodes=150
#      因为「不触发裁剪」而侥幸正常。
#   3. 同一路径下 `E(net)$score` 也丢 → 「边宽按 score」静默退化成固定线宽。


# ---- 辅助 ----------------------------------------------------------------
.ppi_d <- function() as.data.frame(tcmnetpharma::ppi_data)
.ppi_node_layer <- function(b) {
  which(vapply(b$data, function(l)
    all(c("shape", "size") %in% names(l)) && !"edge_colour" %in% names(l),
    logical(1)))[1]
}
.ppi_edge_layer <- function(b) {
  which(vapply(b$data, function(l) "edge_width" %in% names(l), logical(1)))[1]
}
.ppi_n_legends <- function(p) {
  gt <- ggplot2::ggplotGrob(p)
  i <- grep("^guide-box-right$", gt$layout$name)
  if (!length(i)) return(0L)
  g <- gt$grobs[[i]]
  if (!length(g$grobs)) return(0L)
  sum(!vapply(g$grobs, function(z) inherits(z, "zeroGrob"), logical(1)))
}
.ppi_labels <- function(p) {
  b <- ggplot2::ggplot_build(p)
  z <- unlist(lapply(b$data, function(l)
    if ("label" %in% names(l)) as.character(l$label) else NULL))
  z[!is.na(z) & nzchar(z)]
}
.ppi_nodes <- function(p) {
  nrow(ggplot2::ggplot_build(p)$data[[.ppi_node_layer(ggplot2::ggplot_build(p))]])
}

# ---- 1. P0：裁剪后可渲染（旧实现 object 'size' not found）----------------
test_that("max_nodes 触发裁剪后仍可渲染，且节点数正确", {
  d <- .ppi_d()
  n_full <- length(unique(c(d[[1]], d[[2]])))
  expect_gt(n_full, 30L)                      # 保证下面确实触发裁剪
  # 旧实现：任何 max_nodes < 节点数的调用都在 ggplot_build 阶段报
  # "object 'size' not found"（.ppi_cap_nodes 未重算被重建图丢掉的顶点属性）
  for (mn in c(30, 50)) {
    p <- suppressMessages(ppi_plot(d, max_nodes = mn))
    expect_error(ggplot2::ggplot_build(p), NA)
    expect_equal(.ppi_nodes(p), mn)
  }
})

test_that("style = \"hub30\" 从崩溃变为可用（30 节点全标）", {
  d <- .ppi_d()
  p <- suppressMessages(ppi_plot(d, style = "hub30"))
  expect_error(ggplot2::ggplot_build(p), NA)
  expect_equal(.ppi_nodes(p), 30L)
  expect_equal(length(.ppi_labels(p)), 30L)   # view=hub 的 label_all
})

test_that("裁剪后 degree 按子图重算，不沿用全图度数", {
  d <- .ppi_d()
  p_full <- suppressMessages(ppi_plot(d, max_nodes = Inf))
  p_cap  <- suppressMessages(ppi_plot(d, max_nodes = 30))
  # ggraph 把顶点属性挂在 `p$data`（layout data frame）上，可直接读 degree
  expect_true("degree" %in% names(p_cap$data))
  # 旧实现若沿用全图度数，裁剪前后 max(degree) 会相同
  expect_lt(max(p_cap$data$degree), max(p_full$data$degree))
  # 第二个判据：fill 色阶的 limits 由 degree 训练 —— 若沿用全图度数，
  # 上限会等于全图最大度，而不是子图最大度
  sc <- p_cap$scales$scales[vapply(p_cap$scales$scales,
                                   function(x) "fill" %in% x$aesthetics,
                                   logical(1))]
  expect_length(sc, 1L)
  expect_lt(max(sc[[1]]$get_limits()), max(p_full$data$degree))
})

test_that("裁剪后边的 score 属性仍映射到线宽（旧实现静默退化为固定线宽）", {
  d <- .ppi_d()
  p <- suppressMessages(ppi_plot(d, max_nodes = 30, style = "cytoscape"))
  b <- ggplot2::ggplot_build(p)
  ed <- b$data[[.ppi_edge_layer(b)]]
  # 旧实现：.tcm_filter_igraph 重建图时丢掉 score 列 → 走「固定线宽」分支，
  # 该列根本不存在或恒定
  expect_true("edge_width" %in% names(ed))
  expect_gt(diff(range(ed$edge_width)), 0.5)
})

# ---- 2. 两轴正交 --------------------------------------------------------
test_that("view 决定节点数与着色通道，且不影响外观参数", {
  d <- .ppi_d()
  for (st in c("default", "journal", "cytoscape", "presentation", "minimal")) {
    n_deg <- .ppi_nodes(suppressMessages(ppi_plot(d, view = "degree", style = st)))
    n_mod <- .ppi_nodes(suppressMessages(ppi_plot(d, view = "module", style = st)))
    n_hub <- .ppi_nodes(suppressMessages(ppi_plot(d, view = "hub", style = st)))
    expect_equal(n_deg, 72L, info = st)
    expect_equal(n_mod, 72L, info = st)
    expect_equal(n_hub, 30L, info = st)       # 只有 hub 改选点
  }
})

test_that("style 决定 size 范围与图例，且不改变节点数", {
  d <- .ppi_d()
  got <- lapply(c("default", "journal", "cytoscape", "presentation", "minimal"),
                function(st) {
                  p <- suppressMessages(ppi_plot(d, view = "degree", style = st))
                  b <- ggplot2::ggplot_build(p)
                  nd <- b$data[[.ppi_node_layer(b)]]
                  list(n = nrow(nd), rng = range(nd$size), lg = .ppi_n_legends(p))
                })
  names(got) <- c("default", "journal", "cytoscape", "presentation", "minimal")
  expect_true(all(vapply(got, function(z) z$n, numeric(1)) == 72))
  expect_equal(got$default$rng, c(1, 10))
  expect_equal(got$journal$rng, c(1.2, 5))
  expect_equal(got$presentation$rng, c(3, 13))
  expect_equal(vapply(got, function(z) z$lg, numeric(1)),
               c(default = 1, journal = 0, cytoscape = 1,
                 presentation = 2, minimal = 0))
})

test_that("15 种 view × style 组合全部可渲染", {
  d <- .ppi_d()
  k <- 0L
  for (v in c("degree", "module", "hub")) {
    for (s in c("default", "journal", "cytoscape", "presentation", "minimal")) {
      p <- suppressMessages(ppi_plot(d, view = v, style = s))
      expect_s3_class(p, "ggplot")
      expect_error(ggplot2::ggplot_build(p), NA)
      k <- k + 1L
    }
  }
  expect_equal(k, 15L)
})

# ---- 3. 旧名映射 --------------------------------------------------------
test_that("旧名 hub30 / module 被拆成 view × style 并给出提示", {
  d <- .ppi_d()
  # ⚠️ 用 capture_messages 而不是 expect_message：hub30 还会额外触发「裁剪」
  #    提示，而 expect_message 只截第一条、其余会漏到测试输出里。
  m1 <- paste(capture_messages(ppi_plot(d, style = "hub30")), collapse = "")
  m2 <- paste(capture_messages(ppi_plot(d, style = "module")), collapse = "")
  expect_match(m1, "view = \"hub\"")
  expect_match(m2, "view = \"module\"")
  # module 旧名确实按社区着色（离散），而不是 degree 渐变（连续）
  p <- suppressMessages(ppi_plot(d, style = "module"))
  b <- ggplot2::ggplot_build(p)
  nd <- b$data[[.ppi_node_layer(b)]]
  expect_lte(length(unique(nd$fill)), 15L)     # 社区数远小于节点数
  expect_equal(.ppi_nodes(p), 72L)
})

test_that("默认调用不产生 message（5 个同名 style 不该报废弃）", {
  d <- .ppi_d()
  for (st in c("default", "journal", "cytoscape", "presentation", "minimal")) {
    expect_length(testthat::capture_messages(ppi_plot(d, style = st)), 0L)
  }
})

# ---- 4. 标签策略：0 条 > 绝对阈值 > view 全标 > style 默认 --------------
test_that("minimal 的 0 标签压过 view=hub 的全标", {
  d <- .ppi_d()
  expect_equal(length(.ppi_labels(suppressMessages(
    ppi_plot(d, view = "hub", style = "minimal")))), 0L)
  # 对照：hub + 其他 style 全标
  expect_equal(length(.ppi_labels(suppressMessages(
    ppi_plot(d, view = "hub", style = "journal")))), 30L)
})

test_that("样式默认标签数是精确 N 个（度数打平时不超发）", {
  d <- .ppi_d()
  # 旧实现用「度数 >= 第 N 名的度数」当阈值 → 同分节点全进来：
  # cytoscape 声称 15 实际 18、presentation 声称 20 实际 29（2026-10-26 实测）
  expect_equal(length(.ppi_labels(suppressMessages(
    ppi_plot(d, style = "cytoscape")))), 15L)
  expect_equal(length(.ppi_labels(suppressMessages(
    ppi_plot(d, style = "presentation")))), 20L)
})

test_that("label.degree 显式阈值覆盖 style 的默认条数", {
  d <- .ppi_d()
  n_all <- length(.ppi_labels(suppressMessages(
    ppi_plot(d, style = "minimal", label.degree = 0))))
  expect_gt(n_all, 0L)                          # minimal 默认 0 条，被阈值覆盖
  n_hi <- length(.ppi_labels(suppressMessages(
    ppi_plot(d, label.degree = 100))))
  expect_equal(n_hi, 0L)                        # 没有人度数 >= 100
})

# ---- 5. 显式参数覆盖 style ---------------------------------------------
test_that("node.size / label.size / edge.width 显式传入覆盖 style", {
  d <- .ppi_d()
  p <- suppressMessages(ppi_plot(d, style = "journal", node.size = c(4, 12)))
  nd <- ggplot2::ggplot_build(p)$data[[.ppi_node_layer(ggplot2::ggplot_build(p))]]
  expect_equal(range(nd$size), c(4, 12))
  # 不传时回到 style 预设
  p2 <- suppressMessages(ppi_plot(d, style = "journal"))
  nd2 <- ggplot2::ggplot_build(p2)$data[[.ppi_node_layer(ggplot2::ggplot_build(p2))]]
  expect_equal(range(nd2$size), c(1.2, 5))
})

test_that("graph.layout = \"auto\" 用 style 布局，显式布局覆盖", {
  d <- .ppi_d()
  xy <- function(...) {
    p <- suppressMessages(ppi_plot(d, ...))
    p$data[, c("x", "y")]
  }
  # default 的 style 布局是 kk；journal 是 fr → 两者坐标不同
  expect_false(isTRUE(all.equal(xy(style = "default"), xy(style = "journal"))))
  # 同一 style 下显式指定 circle → 与 auto 不同
  expect_false(isTRUE(all.equal(xy(style = "journal"),
                                xy(style = "journal", graph.layout = "circle"))))
  # 非力导向布局不再因 niter 报错
  for (ly in c("kk", "circle", "grid", "nicely")) {
    expect_error(suppressMessages(ppi_plot(d, graph.layout = ly)), NA)
  }
})

# ---- 6. rem.dis.inter：锁定旧语义（含方向无关）-------------------------
test_that("rem.dis.inter 去掉「from 侧 1 次且 to 侧 1 次」的边", {
  # 旧语义（保留）：只看 from 列的计数与 to 列的计数，**不是**节点总度数。
  #   from 计数 A=1 C=2 D=1 E=1 F=1；to 计数 B=1 D=1 E=1 F=1 G=1 H=1
  #   保留 C-D / C-E（from 侧 C=2），其余 4 条被删 → 2 条边、3 个节点
  syn <- data.frame(node1 = c("A", "C", "C", "D", "E", "F"),
                    node2 = c("B", "D", "E", "F", "G", "H"),
                    combined_score = c(900, 800, 700, 600, 500, 400),
                    stringsAsFactors = FALSE)
  expect_equal(.ppi_nodes(suppressMessages(
    ppi_plot(syn, rem.dis.inter = FALSE, max_nodes = Inf))), 8L)
  expect_equal(.ppi_nodes(suppressMessages(
    ppi_plot(syn, rem.dis.inter = TRUE, max_nodes = Inf))), 3L)
  # 列方向不再影响结果：旧实现用基数猜 from/to（tibble 与 data.frame 结果不同），
  # 同一份数据换方向会翻转 rem.dis.inter 的结果；现在列按位置固定命名。
  syn_rev <- syn[, c(2, 1, 3)]
  expect_equal(.ppi_nodes(suppressMessages(
    ppi_plot(syn_rev, rem.dis.inter = TRUE, max_nodes = Inf))), 3L)
})

# ---- 7. 输入校验与兜底 --------------------------------------------------
test_that("非法输入给出可读报错（旧实现只 print 后继续执行）", {
  d <- .ppi_d()
  expect_error(ppi_plot("x"), "data.frame")
  expect_error(ppi_plot(data.frame(a = 1)), "\u4e24\u5217")
  expect_error(ppi_plot(d, style = "nope"), "style")
  expect_error(ppi_plot(d, view = "nope"), "view")
})

test_that("两列输入（无 combined_score）退化为固定线宽而不是无图", {
  d <- .ppi_d()[, 1:2]
  p <- suppressMessages(ppi_plot(d))
  b <- ggplot2::ggplot_build(p)
  expect_gt(.ppi_nodes(p), 0L)
  # 没有 score → 不出 edge_width 映射（固定 linewidth 兜底）
  el <- .ppi_edge_layer(b)
  if (!is.na(el)) expect_equal(length(unique(b$data[[el]]$edge_width)), 1L)
})
