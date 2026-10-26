# 共享视觉层三项新增能力（2026-10-26 第二轮）
#
# 为什么单独一个文件：这三项是 `.tcm_net_plot()` 的能力，不是某个函数的参数。
# 它们的存在理由是「让 ppi_plot 不再需要自己的渲染器」——
#   ① node_style = "ringed"（fill + 白描边）
#   ② edge_width_by（按边属性定宽）
#   ③ colour_by = "degree"（连续 degree 作为 colour / fill 通道）
# 外加 label_top / legend / niter / palette_colours 四个外观旋钮。
#
# 断言都直接调用内部函数：这些旋钮**没有**对外导出，只有包内调用方用，
# 走 ppi_plot 转一手会掩盖「旋钮本身是否生效」。

# ---- 造数：带 score 的小网络（二分 + 单类型两种）-------------------------
.ppi_graph <- function() {
  d <- data.frame(
    from = c("A", "A", "B", "C", "D", "E", "F", "G"),
    to   = c("B", "C", "C", "D", "E", "F", "G", "A"),
    score = c(900, 800, 700, 600, 500, 400, 300, 200),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(d, directed = FALSE)
  igraph::V(g)$degree <- igraph::degree(g)
  igraph::V(g)$size <- igraph::degree(g)
  igraph::E(g)$score <- igraph::E(g)$score
  g
}

.net_node_layer <- function(b) {
  which(vapply(b$data, function(l) all(c("shape", "size") %in% names(l)),
               logical(1)))[1]
}
.net_legends <- function(p) {
  gt <- ggplot2::ggplotGrob(p)
  i <- grep("^guide-box-right$", gt$layout$name)
  if (!length(i)) return(0L)
  g <- gt$grobs[[i]]
  if (!length(g$grobs)) return(0L)
  sum(!vapply(g$grobs, function(z) inherits(z, "zeroGrob"), logical(1)))
}
.net_plot <- function(...) {
  tcmnetpharma:::.tcm_net_plot(.ppi_graph(), ...)
}

# ---- 1. node_style：solid 用 colour + shape 19；ringed 用 fill + shape 21 --
test_that("node_style 切换 colour/fill 通道与 shape", {
  p_solid <- .net_plot(node_style = "solid")
  p_ring  <- .net_plot(node_style = "ringed", node_stroke = 0.8)

  b1 <- ggplot2::ggplot_build(p_solid)
  b2 <- ggplot2::ggplot_build(p_ring)
  nd1 <- b1$data[[.net_node_layer(b1)]]
  nd2 <- b2$data[[.net_node_layer(b2)]]
  # ⚠️ 不能只判 "fill" 在不在 names 里：GeomPoint 的 build 数据**总有** fill 列，
  #    未映射时整列是 NA。判「是否真的被映射」要判非 NA。
  expect_true("colour" %in% names(nd1))
  expect_true(all(is.na(nd1$fill)))
  expect_false(all(is.na(nd2$fill)))
  # ringed 的 colour 是**固定**白描边（不是 NA，也不是映射）
  expect_equal(unique(nd2$colour), "white")

  # shape / stroke 是 geom 的固定参数
  expect_equal(p_solid$layers[[2]]$aes_params$shape, NULL)
  expect_equal(p_ring$layers[[2]]$aes_params$shape, 21)
  expect_equal(p_ring$layers[[2]]$aes_params$stroke, 0.8)
  expect_equal(p_ring$layers[[2]]$aes_params$colour, "white")
})

test_that("ringed 配的是 fill 标度、solid 配的是 colour 标度", {
  # ⚠️ 混用会得到「节点全灰 + 空图例」——这条断言就是为了钉死它
  aes_of <- function(p) {
    unlist(lapply(p$scales$scales, function(s) s$aesthetics))
  }
  expect_true("colour" %in% aes_of(.net_plot(node_style = "solid")))
  expect_false("fill" %in% aes_of(.net_plot(node_style = "solid")))
  expect_true("fill" %in% aes_of(.net_plot(node_style = "ringed")))
  expect_false("colour" %in% aes_of(.net_plot(node_style = "ringed")))
})

# ---- 2. colour_by：新增连续 degree 通道 ---------------------------------
test_that("colour_by = \"degree\" 走连续色阶（不是分档）", {
  p_c <- .net_plot(colour_by = "degree")
  p_b <- .net_plot(colour_by = "degree_bin")
  grad_of <- function(p) {
    sc <- p$scales$scales[vapply(p$scales$scales,
                                 function(s) "colour" %in% s$aesthetics,
                                 logical(1))]
    if (length(sc) == 1L) class(sc[[1]])[1] else NA_character_
  }
  expect_match(grad_of(p_c), "Continuous|Gradient")
  expect_match(grad_of(p_b), "Discrete")
  # 连续通道的节点颜色取值数应接近节点数（分档只有 4 档）
  ncol_of <- function(p) {
    b <- ggplot2::ggplot_build(p)
    length(unique(b$data[[.net_node_layer(b)]]$colour))
  }
  expect_gt(ncol_of(p_c), ncol_of(p_b))
})

test_that("未知 colour_by 明确报错（switch 无匹配会静默返回 NULL）", {
  expect_error(.net_plot(colour_by = "nope"), "colour")
})

# ---- 3. edge_width_by：按边属性定宽 -------------------------------------
test_that("edge_width_by 让边宽随属性变化；不给则恒定", {
  p_w <- .net_plot(edge_width_by = "score", edge_width_range = c(0.3, 2))
  p_f <- .net_plot()
  ew <- function(p) {
    b <- ggplot2::ggplot_build(p)
    i <- which(vapply(b$data, function(l) "edge_width" %in% names(l),
                      logical(1)))[1]
    b$data[[i]]$edge_width
  }
  expect_gt(diff(range(ew(p_w))), 1)
  expect_equal(length(unique(ew(p_f))), 1L)
  # 范围确实按 edge_width_range 缩放
  expect_equal(range(ew(p_w)), c(0.3, 2))
})

test_that("edge_width_by 与 edge_colour_by 可以同时用（两个通道各自独立）", {
  g <- .ppi_graph()
  igraph::E(g)$src <- rep(c("s1", "s2"), length.out = igraph::ecount(g))
  p <- tcmnetpharma:::.tcm_net_plot(g, edge_colour_by = "src",
                                    edge_width_by = "score")
  b <- ggplot2::ggplot_build(p)
  ed <- b$data[[which(vapply(b$data, function(l)
    "edge_colour" %in% names(l), logical(1)))[1]]]
  expect_equal(length(unique(ed$edge_colour)), 2L)
  expect_gt(diff(range(ed$edge_width)), 0)
})

# ---- 4. label_top：Inf = 全标，0 = 不标，N = 精确 N ----------------------
test_that("label_top 的三个特殊值", {
  nl <- function(p) {
    b <- ggplot2::ggplot_build(p)
    z <- unlist(lapply(b$data, function(l)
      if ("label" %in% names(l)) as.character(l$label) else NULL))
    length(z[!is.na(z) & nzchar(z)])
  }
  n <- igraph::vcount(.ppi_graph())
  expect_equal(nl(.net_plot(label_top = Inf)), n)
  expect_equal(nl(.net_plot(label_top = 0)), 0L)
  expect_equal(nl(.net_plot(label_top = 3)), 3L)
  # 显式阈值仍然压过 label_top
  expect_equal(nl(.net_plot(label_top = 3, label_degree = 100)), 0L)
})

# ---- 5. legend 档位 -----------------------------------------------------
test_that("legend 三档：none / auto / all", {
  expect_equal(.net_legends(.net_plot(legend = "none")), 0L)
  expect_equal(.net_legends(.net_plot(legend = "auto")), 1L)
  # "all" 额外给 size 图例
  expect_equal(.net_legends(.net_plot(legend = "all")), 2L)
})

# ---- 6. palette_colours：名义色板可由调用方指定 -------------------------
test_that("palette_colours 覆盖名义色板", {
  g <- .ppi_graph()
  igraph::V(g)$class <- rep(c("x", "y"), length.out = igraph::vcount(g))
  p <- tcmnetpharma:::.tcm_net_plot(
    g, colour_by = "class",
    palette_colours = c("#ff0000", "#00ff00")
  )
  b <- ggplot2::ggplot_build(p)
  got <- unique(b$data[[.net_node_layer(b)]]$colour)
  expect_setequal(toupper(got), c("#FF0000", "#00FF00"))
})

# ---- 7. 默认值不变：老调用方（tcm_net / module_netplot / venn_net）不受影响
test_that("不带新参数时行为与之前一致（solid + 1 个图例 + 固定边宽）", {
  p <- .net_plot()
  b <- ggplot2::ggplot_build(p)
  nd <- b$data[[.net_node_layer(b)]]
  expect_true("colour" %in% names(nd))
  expect_equal(.net_legends(p), 1L)
  # 边宽恒定（默认不按属性映射）
  i <- which(vapply(b$data, function(l) "edge_width" %in% names(l),
                    logical(1)))[1]
  expect_equal(length(unique(b$data[[i]]$edge_width)), 1L)
})
