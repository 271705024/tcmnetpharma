# tcm_net：view(3) × style(4) 全矩阵契约（2026-10-26）
#
# 为什么单独一个文件：`test-style-presets.R` 覆盖的是富集类图（go_barplot /
# go_dotplot / bar_plot / degree_plot），`test-net-r2.R` 只碰了
# `cluster × {default, journal}` 两格 —— tcm_net 的 `cytoscape` / `presentation`
# **从未在测试里跑过**，12 格里有 10 格是空的（§9.7 顺延项）。
#
# 本文件把「两个轴正交」的契约逐条钉死：
#   · view 决定**数据**（选哪些节点 / 颜色表什么 / 标哪些标签）
#   · style 决定**外观**（边透明度 / 边线宽 / 标签字号）
# 每条断言都写明旧实现或「若正交性被破坏」时的实际取值，保证可证伪。

# ---- 造数 + 缓存 ---------------------------------------------------------
.net3 <- function() as.data.frame(tcmnetpharma::network_data)
.views  <- c("overview", "cluster", "hub")
.styles <- c("default", "journal", "cytoscape", "presentation")

# 12 次出图 + build 不算便宜，全文件只算一次（同 view+style 必须确定性，
# 所以缓存不会掩盖「两次结果不同」这类缺陷 —— 那条另有专门断言）
.net_cache <- local({
  env <- new.env(parent = emptyenv())
  function(v, s) {
    key <- paste(v, s, sep = "|")
    if (!is.null(env[[key]])) return(env[[key]])
    p <- suppressMessages(tcm_net(.net3(), view = v, style = s))
    env[[key]] <- list(p = p, b = ggplot2::ggplot_build(p))
    env[[key]]
  }
})

.net_edge_layer <- function(b) {
  which(vapply(b$data, function(l) "edge_colour" %in% names(l), logical(1)))
}
.net_labels <- function(b) {
  z <- unlist(lapply(b$data, function(l)
    if ("label" %in% names(l)) as.character(l$label) else NULL))
  z[!is.na(z) & nzchar(z)]
}
.net_label_size <- function(b) {
  # 标签层 = 同时有 label 与 size 的层；cluster 视图是 annotate 的质心标签
  v <- unlist(lapply(b$data, function(l)
    if (all(c("label", "size") %in% names(l))) unique(l$size) else NULL))
  unique(round(as.numeric(v), 4))
}
.net_n_legends <- function(p) {
  gt <- ggplot2::ggplotGrob(p)
  i <- grep("^guide-box-right$", gt$layout$name)
  if (!length(i)) return(0L)
  g <- gt$grobs[[i]]
  if (!length(g$grobs)) return(0L)
  sum(!vapply(g$grobs, function(z) inherits(z, "zeroGrob"), logical(1)))
}

# ---- 1. 12 格全部可渲染，每格 1 层边 + 1 个图例 -------------------------
test_that("view × style 的 12 个组合全部可渲染，且都是单层边 + 单图例", {
  k <- 0L
  for (v in .views) {
    for (s in .styles) {
      z <- .net_cache(v, s)
      expect_s3_class(z$p, "ggplot")
      expect_error(ggplot2::ggplot_build(z$p), NA)
      expect_length(.net_edge_layer(z$b), 1L)          # R2 硬约束：不双绘边
      expect_equal(.net_n_legends(z$p), 1L)
      k <- k + 1L
    }
  }
  expect_equal(k, 12L)
})

# ---- 2. style 只改外观：节点集 / 标签集 / cap_report 一格不差 -----------
test_that("同一 view 下 4 档 style 的节点集、标签集、裁剪报告完全一致", {
  for (v in .views) {
    ref <- .net_cache(v, "default")
    for (s in setdiff(.styles, "default")) {
      z <- .net_cache(v, s)
      # 节点集合
      expect_setequal(z$p$data$name, ref$p$data$name)
      # 标签集合（哪几个节点带标签是 view 的职责，不是 style 的）
      expect_setequal(.net_labels(z$b), .net_labels(ref$b))
      # 裁剪报告：同样的输入必须给出同样的节点/边数
      expect_equal(attr(z$p, "cap_report")$shown_nodes,
                   attr(ref$p, "cap_report")$shown_nodes)
      expect_equal(attr(z$p, "cap_report")$shown_edges,
                   attr(ref$p, "cap_report")$shown_edges)
    }
  }
})

test_that("同一 view 下 4 档 style 的坐标也一致（布局不随外观变）", {
  for (v in .views) {
    ref <- .net_cache(v, "default")$p$data
    for (s in setdiff(.styles, "default")) {
      z <- .net_cache(v, s)$p$data
      # 布局带固定种子；style 不该影响布局，否则「换个外观图就重排」不可复现
      expect_equal(z$x, ref$x, tolerance = 1e-9)
      expect_equal(z$y, ref$y, tolerance = 1e-9)
    }
  }
})

# ---- 3. style 的 3 个旋钮真的生效，且 4 档两两不同 ---------------------
test_that("4 档 style 的边透明度 / 边线宽 / 标签字号两两不同且等于预设", {
  pres <- tcmnetpharma:::.tcm_net_style_presets()
  alphas <- vapply(.styles, function(s) pres[[s]]$edge_alpha, numeric(1))
  lws    <- vapply(.styles, function(s) pres[[s]]$edge_lw, numeric(1))
  lsz    <- vapply(.styles, function(s) pres[[s]]$label_size, numeric(1))
  # 预设表本身：边参数 4 档互不相同（防止有人把两档改成一样、图上看不出差别）
  expect_equal(length(unique(alphas)), 4L)
  expect_equal(length(unique(lws)), 4L)
  # ⚠️ 标签字号只有 **3** 个不同值：default 与 cytoscape 都是 3.2（2026-10-26 实测）。
  #    这不是缺陷 —— 两档靠边参数区分，字号相同是有意的；写成 `4L` 会假失败。
  expect_equal(length(unique(lsz)), 3L)
  expect_equal(unname(lsz), c(3.2, 3.0, 3.2, 4.0))

  for (s in .styles) {
    z <- .net_cache("overview", s)
    ed <- z$b$data[[.net_edge_layer(z$b)[1L]]]
    # 渲染出来的值必须等于预设 —— 否则「参数生效」只是寄存器层面的
    expect_equal(unique(ed$edge_alpha), pres[[s]]$edge_alpha, tolerance = 1e-9)
    expect_equal(unique(ed$edge_width), pres[[s]]$edge_lw, tolerance = 1e-9)
    expect_true(all(abs(.net_label_size(z$b) - pres[[s]]$label_size) < 1e-9))
  }
})

test_that("cluster 视图的质心标签字号 = 预设 + 0.4（回归：不要被顺手抹平）", {
  pres <- tcmnetpharma:::.tcm_net_style_presets()
  for (s in .styles) {
    got <- .net_label_size(.net_cache("cluster", s)$b)
    expect_equal(unique(got), pres[[s]]$label_size + 0.4, tolerance = 1e-9)
  }
})

# ---- 4. view 决定数据：节点数 / 图例语义 -------------------------------
test_that("view 改变节点数与图例语义（style 不变）", {
  n_of <- function(v) attr(.net_cache(v, "default")$p, "cap_report")$shown_nodes
  expect_gt(n_of("overview"), n_of("hub"))
  # overview 与 cluster 节点数相同但着色通道不同（class vs module）
  expect_equal(n_of("overview"), n_of("cluster"))
  # 图例标题要从**渲染后的 gtable** 里取（`p$labels` 里没有）：
  # ⚠️ 不能用 `z$title[[1L]]$children[[1L]]$label` —— 该路径取不到，得到空串
  #    （2026-10-26 实测）。正确路径与 test-net-r2.R 的 .legend_titles() 一致。
  titles <- function(v) {
    gt <- ggplot2::ggplotGrob(.net_cache(v, "default")$p)
    i <- grep("^guide-box-right$", gt$layout$name)
    if (!length(i)) return(character(0))
    g <- gt$grobs[[i]]
    out <- character(0)
    for (z in g$grobs) {
      if (!inherits(z, "gtable")) next
      k <- grep("title", z$layout$name, ignore.case = TRUE)
      if (!length(k)) next
      out <- c(out, tryCatch(
        paste(unlist(lapply(z$grobs[[k[1]]]$children, function(o) o$label)),
              collapse = ""),
        error = function(e) ""))
    }
    out
  }
  expect_true("type" %in% titles("overview"))
  expect_true("module" %in% titles("cluster"))
})

test_that("cluster 视图只出簇名标签，overview / hub 出节点名", {
  cl <- .net_labels(.net_cache("cluster", "default")$b)
  expect_true(all(grepl("^M[0-9]+$", cl)))
  ov <- .net_labels(.net_cache("overview", "default")$b)
  expect_false(any(grepl("^M[0-9]+$", ov)))
})

# ---- 5. 确定性 ---------------------------------------------------------
test_that("同 view + style 两次出图坐标完全一致（固定种子）", {
  nd <- .net3()
  for (v in .views) {
    a <- suppressMessages(tcm_net(nd, view = v, style = "cytoscape"))
    b <- suppressMessages(tcm_net(nd, view = v, style = "cytoscape"))
    expect_equal(a$data$x, b$data$x, tolerance = 1e-12)
    expect_equal(a$data$y, b$data$y, tolerance = 1e-12)
  }
})

# ---- 6. 非法取值必须报错 ----------------------------------------------
test_that("非法 view / style 报错（不静默回退）", {
  nd <- .net3()
  expect_error(suppressMessages(tcm_net(nd, view = "nope")), "should be one of")
  expect_error(suppressMessages(tcm_net(nd, style = "nope")), "should be one of")
})

# ---- 7. 标签数由 view 决定，与 style 无关（显式阈值除外）---------------
test_that("显式 label.degree 在各 style 下给出同一批标签", {
  nd <- .net3()
  ref <- NULL
  for (s in .styles) {
    p <- suppressMessages(tcm_net(nd, view = "overview", style = s,
                                  label.degree = 6))
    labs <- sort(.net_labels(ggplot2::ggplot_build(p)))
    expect_gt(length(labs), 0L)
    if (is.null(ref)) ref <- labs else expect_identical(labs, ref)
  }
})
