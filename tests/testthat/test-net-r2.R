# R2 网络视觉层 + 静默错误修复（2026-10-24）
#
# 本文件的断言都刻意写成「**能被旧实现证伪**」的形式：
# 每条都在注释里写明旧实现的实际取值，证明断言不是恒真（铁律 28/52）。

# ---- 工具 -----------------------------------------------------------------
# ⚠️ 不要用 `from`/`to` 列判断边层：ggraph 的 geom_edge_link（路径型）在
#    ggplot_build 结果里**不保留** from/to（geom_edge_link0/geom_edge_fan 才保留）。
#    用 edge_colour / edge_width 这类**边专属美学**判断才可靠。
.edge_layers <- function(b) {
  which(vapply(b$data, function(l) "edge_colour" %in% names(l), logical(1)))
}
# 节点点层：有 shape 且没有边美学、没有 label（label 层是 geom_node_label）
.node_layer <- function(b) {
  idx <- which(vapply(b$data, function(l) {
    "shape" %in% names(l) && !("edge_colour" %in% names(l)) &&
      !("label" %in% names(l))
  }, logical(1)))
  if (!length(idx)) NULL else b$data[[idx[[1L]]]]
}
# 所有非空标签文本（跨图层收集）—— 不预设哪一层是标签层
.all_labels <- function(b) {
  v <- unlist(lapply(b$data, function(l) {
    if ("label" %in% names(l)) as.character(l$label) else NULL
  }))
  v[!is.na(v) & nzchar(v)]
}
# guide-box-right 里真实存在（非 zeroGrob）的图例个数
.n_legends <- function(p) {
  gt <- ggplot2::ggplotGrob(p)
  i <- grep("^guide-box-right$", gt$layout$name)
  if (!length(i)) return(0L)
  g <- gt$grobs[[i]]
  if (!length(g$grobs)) return(0L)
  sum(!vapply(g$grobs, function(z) inherits(z, "zeroGrob"), logical(1)))
}
# 图例标题文本
.legend_titles <- function(p) {
  gt <- ggplot2::ggplotGrob(p)
  i <- grep("^guide-box-right$", gt$layout$name)
  if (!length(i)) return(character(0))
  g <- gt$grobs[[i]]
  out <- character(0)
  for (z in g$grobs) {
    if (!inherits(z, "gtable")) next
    k <- grep("title", z$layout$name, ignore.case = TRUE)
    if (!length(k)) next
    t <- tryCatch(paste(unlist(lapply(z$grobs[[k[1]]]$children,
                                      function(o) o$label)), collapse = ""),
                  error = function(e) "")
    out <- c(out, t)
  }
  out
}

.make_net3 <- function() as.data.frame(tcmnetpharma::network_data)
.make_ppi <- function() {
  d <- as.data.frame(tcmnetpharma::network_data)[1:2]
  colnames(d) <- c("node1", "node2")
  d$weight <- seq(0.4, 0.9, length.out = nrow(d))
  d
}

# ===========================================================================
# 1. tcm_net 只画一层边（旧实现 = 2 层：geom_edge_link0 + geom_edge_fan）
# ===========================================================================
test_that("tcm_net 只画一层边，不再双绘", {
  nd <- .make_net3()
  for (v in c("overview", "cluster", "hub")) {
    b <- ggplot2::ggplot_build(tcm_net(nd, view = v))
    expect_equal(length(.edge_layers(b)), 1L,
                 info = paste("view =", v, "的边图层数应为 1（旧实现是 2）"))
  }
})

# ===========================================================================
# 2. view 真的改数据（不只是改样式），且裁剪报告可读
# ===========================================================================
test_that("view 改变节点集合，且 cap_report 与图上节点数一致", {
  nd <- .make_net3()
  p_ov <- tcm_net(nd, view = "overview")
  p_hub <- tcm_net(nd, view = "hub")

  r_ov <- attr(p_ov, "cap_report")
  r_hub <- attr(p_hub, "cap_report")
  expect_type(r_ov, "list")
  # 2026-10-28：report 增加 floored / floor_dropped（min_degree 从「裁剪时才生效」
  # 改为「与 max_nodes 正交的严格度数下限」，报告必须同步说明是否被下限筛过）；
  # 另加 cap_used（实际生效的上限，供 message 说「上限 N、实际画 M」）。
  expect_named(r_ov, c("shown_nodes", "total_nodes", "shown_edges",
                       "total_edges", "capped", "cap_used",
                       "floored", "floor_dropped",
                       "floor_isolated", "floor_rounds"))
  expect_true(r_ov$capped)
  expect_true(r_ov$shown_nodes <= 50L)
  expect_equal(r_ov$total_nodes, r_hub$total_nodes)
  # ★ 鉴别力：两个 view 选出的是**不同的子图**（旧实现没有 view，二者必然相同）
  expect_false(identical(
    c(r_ov$shown_nodes, r_ov$shown_edges),
    c(r_hub$shown_nodes, r_hub$shown_edges)
  ))

  # 图上实际点数 == report 的 shown_nodes
  b <- ggplot2::ggplot_build(p_ov)
  nl <- .node_layer(b)
  expect_false(is.null(nl))
  expect_equal(nrow(nl), r_ov$shown_nodes)
})

# ===========================================================================
# 3. view 与 style 正交：换 style 不改节点集合
# ===========================================================================
test_that("style 只改外观，不改节点集合（view × style 正交）", {
  nd <- .make_net3()
  a <- attr(tcm_net(nd, view = "cluster", style = "default"), "cap_report")
  bbb <- attr(tcm_net(nd, view = "cluster", style = "journal"), "cap_report")
  expect_equal(a$shown_nodes, bbb$shown_nodes)
  expect_equal(a$shown_edges, bbb$shown_edges)
  # 但边透明度确实变了（不同 style 的视觉不同）
  pa <- tcm_net(nd, view = "cluster", style = "default")
  pb <- tcm_net(nd, view = "cluster", style = "journal")
  ba <- ggplot2::ggplot_build(pa)
  bb <- ggplot2::ggplot_build(pb)
  ea <- ba$data[[.edge_layers(ba)[1L]]]
  eb <- bb$data[[.edge_layers(bb)[1L]]]
  expect_false(isTRUE(all.equal(unique(ea$edge_alpha), unique(eb$edge_alpha))))
})

# ===========================================================================
# 4. cluster 视图只标簇名（M1/M2...），不标节点名
# ===========================================================================
test_that("cluster 视图的标签只有簇名", {
  nd <- .make_net3()
  p <- tcm_net(nd, view = "cluster")
  b <- ggplot2::ggplot_build(p)
  labs <- .all_labels(b)
  expect_true(length(labs) >= 1L)
  expect_true(all(grepl("^M[0-9]+$", labs)),
              info = paste("簇名应形如 M1/M2，实际：",
                           paste(utils::head(labs, 5), collapse = ", ")))
  # 鉴别力：节点名一个都不该出现
  node_names <- unique(c(nd$herb, nd$molecule, nd$target))
  expect_false(any(labs %in% node_names))
})

# ===========================================================================
# 5. 图例收敛：每个 view 只出一个图例
# ===========================================================================
test_that("tcm_net 每个 view 只有一个图例", {
  nd <- .make_net3()
  for (v in c("overview", "cluster", "hub")) {
    expect_equal(.n_legends(tcm_net(nd, view = v)), 1L,
                 info = paste("view =", v, "的图例数应为 1"))
  }
  # 图例标题是语义名，不是内部列名
  expect_true(all(.legend_titles(tcm_net(nd, view = "overview")) %in% c("type")))
  expect_true(all(.legend_titles(tcm_net(nd, view = "hub")) %in% c("degree")))
})

# ===========================================================================
# 6. ppi_plot: node.size 生效（旧实现 c(1,10) 与 c(6,30) 出图完全相同）
# ===========================================================================
test_that("ppi_plot 的 node.size 真的改变点大小", {
  d <- .make_ppi()
  rng <- function(p) {
    l <- .node_layer(ggplot2::ggplot_build(p))
    range(l$size)
  }
  a <- rng(ppi_plot(d, max_nodes = Inf, node.size = c(1, 10)))
  b <- rng(ppi_plot(d, max_nodes = Inf, node.size = c(6, 30)))
  expect_false(isTRUE(all.equal(a, b)),
               info = "node.size 不同却得到相同点大小 = 参数被静默忽略（旧版缺陷）")
  expect_equal(round(a, 1), c(1, 10))
})

# ===========================================================================
# 7. ppi_plot: 图例从 3 个降到 1 个，且不存在说谎的 "score" 图例
# ===========================================================================
test_that("ppi_plot default 只有一个图例，且没有 score 图例", {
  p <- ppi_plot(.make_ppi(), max_nodes = Inf)
  expect_equal(.n_legends(p), 1L,
               info = "旧实现有 3 个：degree(幽灵) / size / score(说谎)")
  expect_false("score" %in% .legend_titles(p))
  expect_true("degree" %in% .legend_titles(p))
})

test_that("ppi_plot 的 edge.color 真的改变边颜色", {
  d <- .make_ppi()
  col_of <- function(p) {
    b <- ggplot2::ggplot_build(p)
    unique(as.character(b$data[[.edge_layers(b)[1L]]]$edge_colour))
  }
  a <- col_of(ppi_plot(d, max_nodes = Inf, edge.color = "lightgrey"))
  b <- col_of(ppi_plot(d, max_nodes = Inf, edge.color = "red"))
  expect_equal(length(a), 1L)
  expect_equal(length(b), 1L)
  expect_false(identical(a, b))
})

# ===========================================================================
# 8. ppi_plot style 的图例档位生效（旧实现 show.legend=FALSE 全部失效）
# ===========================================================================
test_that("ppi_plot style 的 legend 档位真的生效", {
  d <- .make_ppi()
  expect_equal(.n_legends(ppi_plot(d, max_nodes = Inf, style = "journal")), 0L)
  expect_true(.n_legends(ppi_plot(d, max_nodes = Inf, style = "presentation")) >= 1L,
              info = "presentation 的 legend=\"all\" 旧实现一个图例都出不来")
})

# ===========================================================================
# 9. venn_plot: 反向基数不再猜错列
# ===========================================================================
test_that("venn_plot 在「基因数少于来源数」时不把两列对调", {
  # 3 个基因 × 5 个来源（ggvenn 上限 5 集合）：
  # 旧判据 3 > 5 = FALSE -> 两列被对调，ggvenn 的集合名变成 G1..G3
  # （把基因当来源），且不报错。
  df <- expand.grid(gene = paste0("G", 1:3), source = paste0("S", 1:5),
                    stringsAsFactors = FALSE)
  p <- venn_plot(df, type = 1)
  sets <- setdiff(names(p$data), "_key")
  expect_true(all(grepl("^S", sets)),
              info = paste("集合名应为来源（S1..S5），实际：",
                           paste(sets, collapse = ", ")))
  expect_false(any(grepl("^G[0-9]+$", sets)))
})

test_that("venn_plot 识别不了列名时报错，而不是静默猜", {
  df <- data.frame(a = c("x", "y"), b = c("p", "q"), stringsAsFactors = FALSE)
  expect_error(venn_plot(df), "gene")
})

test_that("venn_plot 支持显式指定列名", {
  df <- data.frame(my_gene = c("g1", "g2"), my_set = c("A", "B"),
                   stringsAsFactors = FALSE)
  p <- venn_plot(df, gene_col = "my_gene", source_col = "my_set")
  sets <- setdiff(names(p$data), "_key")
  expect_setequal(sets, c("A", "B"))
})

# ===========================================================================
# 10. tf_cirplot: 列解析
# ===========================================================================
test_that("tf_cirplot 识别不了列名时报错", {
  df <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  expect_error(tf_cirplot(df), "TF")
})

test_that("tf_cirplot 认识 TF / Target 列名", {
  df <- data.frame(TF = rep(c("T1", "T2"), each = 3),
                   Target = c("g1", "g2", "g3", "g2", "g3", "g4"),
                   stringsAsFactors = FALSE)
  expect_silent(tf_cirplot(df))
})

# ===========================================================================
# 11. 契约样本数据仍然可用（回归：内置数据集列名必须能自动匹配）
# ===========================================================================
test_that("内置契约样本数据无需显式列名即可出图", {
  expect_s3_class(venn_plot(tcmnetpharma::venn_data), "ggplot")
  expect_silent(tf_cirplot(tcmnetpharma::tf_data))
  expect_s3_class(tcm_net(tcmnetpharma::network_data), "ggplot")
  expect_s3_class(ppi_plot(tcmnetpharma::ppi_data), "ggplot")
})
