# venn_net 接入共享视觉层 + 猜列缺陷修复（2026-10-25）
#
# 断言一律写成「能被旧实现证伪」的形式：每条注释写明旧实现的实际取值。
#
# 旧实现的三个真实缺陷（见 docs/plot-module-audit-2026-10-24.md §11）：
#   1. 用 `length(unique(data[, 1])) > length(unique(data[, 2]))` 猜哪列是 from/to。
#      该判据在 tibble 与 data.frame 上取值不同 → **同一个函数换个输入类就坏**。
#   2. `edge.type` 非 arc/fan/hive 时函数体一路 if/else if 走完 **返回 NULL**，
#      不报错也不出图。
#   3. `scale_edge_color_manual(values = edge.color)` 写死 5 色，来源 > 5 必崩。

# ---- 辅助 ----------------------------------------------------------------
.edge_layer <- function(b) {
  which(vapply(b$data, function(l) "edge_colour" %in% names(l), logical(1)))
}
.node_layer <- function(b) {
  which(vapply(b$data, function(l)
    all(c("shape", "size") %in% names(l)) && !"edge_colour" %in% names(l),
    logical(1)))[1]
}
.all_labels <- function(p) {
  b <- ggplot2::ggplot_build(p)
  z <- unlist(lapply(b$data, function(l)
    if ("label" %in% names(l)) as.character(l$label) else NULL))
  z[!is.na(z) & nzchar(z)]
}
.n_legends <- function(p) {
  gt <- ggplot2::ggplotGrob(p)
  i <- grep("^guide-box-right$", gt$layout$name)
  if (!length(i)) return(0L)
  g <- gt$grobs[[i]]
  if (!length(g$grobs)) return(0L)
  sum(!vapply(g$grobs, function(z) inherits(z, "zeroGrob"), logical(1)))
}
.vd <- function() tcmnetpharma::venn_data

# ---- 1. 猜列缺陷：tibble 与 data.frame 必须等价 -------------------------
test_that("tibble 与 as.data.frame 出的图完全一致（旧实现后者直接报错）", {
  d <- .vd()
  b1 <- ggplot2::ggplot_build(venn_net(d))
  b2 <- ggplot2::ggplot_build(venn_net(as.data.frame(d)))
  # 旧实现：as.data.frame 后走 c("to","from") 分支，边按**基因名**着色
  # （149 个水平）而色板只有 5 色 → build 阶段报
  # "Insufficient values in manual scale. 149 needed but only 5 provided."
  expect_error(venn_net(as.data.frame(d)), NA)
  expect_equal(length(b1$data), length(b2$data))
  for (i in seq_along(b1$data)) {
    expect_equal(nrow(b1$data[[i]]), nrow(b2$data[[i]]))
    expect_equal(b1$data[[i]]$x, b2$data[[i]]$x)
    expect_equal(b1$data[[i]]$y, b2$data[[i]]$y)
  }
})

# ---- 2. 编码：边 = 来源，节点 = 角色，各一个通道 ------------------------
test_that("边按来源着色、节点按角色着色，图例各一个", {
  d <- .vd()
  p <- venn_net(d)
  b <- ggplot2::ggplot_build(p)
  expect_length(.edge_layer(b), 1L)            # 单层边（R2 硬约束）
  ed <- b$data[[.edge_layer(b)]]
  nd <- b$data[[.node_layer(b)]]
  n_src <- length(unique(as.character(d$source)))
  # 旧实现 show.legend = FALSE → 边上色但不给图例，读者无法解码
  expect_equal(length(unique(ed$edge_colour)), n_src)
  expect_equal(sum(is.na(ed$edge_colour)), 0L)
  # 节点只有 gene / source 两类（不是 degree 渐变：旧实现 colour = degree）
  expect_equal(length(unique(nd$colour)), 2L)
  expect_equal(.n_legends(p), 2L)              # type + source
  # 旧实现 1 个图例（degree 大小），且边色无图例
})

# ---- 3. 标签：恰好 top-15，且所有 source 必在其中 ----------------------
test_that("默认标签恰为 15 个，且包含全部来源节点", {
  d <- .vd()
  labs <- .all_labels(venn_net(d))
  # 旧实现默认 label.degree = 1 → 全部 153 个节点都出标签，糊成一团
  expect_equal(length(labs), 15L)
  src <- unique(as.character(d$source))
  expect_true(all(src %in% labs))
})

test_that("度数打平时「前 N 个」不会超发（旧阈值法实测发了 ~40 个）", {
  # venn_data：149 个基因里绝大多数 degree 只有 1–2，第 15 名的度数会与一大票
  # 节点同分 → 用 `degree >= 阈值` 实现「top 15」会把同分节点全放进来。
  d <- .vd()
  deg_tab <- table(table(d$gene))
  expect_gt(length(deg_tab), 1L)               # 确认造数确实有打平
  expect_equal(length(.all_labels(venn_net(d))), 15L)
  # 显式阈值仍走旧语义（向后兼容）
  expect_equal(length(.all_labels(venn_net(d, label.degree = 1))),
               length(unique(c(d$gene, d$source))))
})

test_that("来源多到 top-N 装不下时，度数极低的来源仍带标签", {
  set.seed(1)
  m <- data.frame(
    gene   = paste0("G", seq_len(120)),
    source = c(rep(paste0("S", 1:19), length.out = 119), "RARE"),
    stringsAsFactors = FALSE
  )
  labs <- .all_labels(venn_net(m, max_nodes = Inf))
  expect_true("RARE" %in% labs)                # RARE 只连 1 个基因（degree = 1）
})

# ---- 4. 来源数 > 5 不再崩（旧实现写死 5 色）-----------------------------
test_that("来源数 20 时仍可渲染，边色数等于来源数", {
  set.seed(2)
  m <- data.frame(
    gene   = paste0("G", seq_len(120)),
    source = rep(paste0("S", seq_len(20)), length.out = 120),
    stringsAsFactors = FALSE
  )
  p <- venn_net(m, max_nodes = Inf)
  b <- ggplot2::ggplot_build(p)
  ed <- b$data[[.edge_layer(b)]]
  # 旧实现：5 个颜色 vs 20 个水平 → "Insufficient values in manual scale"
  expect_equal(length(unique(ed$edge_colour)), 20L)
  # 显式给不足色数时自动插值补足，而不是报错。
  # ⚠️ 只断言"没有 NA"：colorRampPalette 在色距很近时会插出**重复**的十六进制值
  #    （实测 `c("#111111","#222222")` 扩到 20 色只得到 18 个不同值），
  #    这是插值本身的极限，不是缺陷 —— 要 20 个可区分色请给足色板。
  p2 <- venn_net(m, max_nodes = Inf, edge.color = c("#111111", "#222222"))
  b2 <- ggplot2::ggplot_build(p2)
  ed2 <- b2$data[[.edge_layer(b2)]]
  expect_equal(sum(is.na(ed2$edge_colour)), 0L)
  expect_gte(length(unique(ed2$edge_colour)), 15L)
})

# ---- 5. 边几何：四种都能出图，且几何类确实不同 --------------------------
test_that("edge.type 四种几何都能渲染，且边坐标两两不同", {
  d <- .vd()
  # ⚠️ 不能用 `class(p$layers[[1]]$geom)` 判几何：ggraph 的 link/arc/fan/hive
  #    **都是 GeomEdgePath**（2026-10-25 实测），类名区分不出来 —— 这正是
  #    「假判据」的典型：名字像对的，事实不对。改判**渲染出来的边坐标**。
  sig <- lapply(c("arc", "fan", "hive", "link"), function(k) {
    p <- venn_net(d, edge.type = k, label.degree = 1)
    expect_s3_class(p, "ggplot")
    b <- ggplot2::ggplot_build(p)
    L <- b$data[[.edge_layer(b)]]
    list(n = nrow(L), sig = paste(c(round(L$x, 6), round(L$y, 6)),
                                   collapse = ","))
  })
  n <- vapply(sig, function(z) z$n, numeric(1))
  # ⚠️ 指纹必须用**全坐标串**：先用 `sum(x)+sum(y)` 时两种几何的总和恰好相等，
  #    4 种只判出 3 种（2026-10-25 实测），属「弱指标判身份」的翻版。
  fp <- vapply(sig, function(z) z$sig, character(1))
  names(fp) <- c("arc", "fan", "hive", "link")
  expect_equal(unname(n), rep(n[[1L]], 4L))          # 边数一致
  # arc / hive / link 三者两两不同
  expect_length(unique(fp[c("arc", "hive", "link")]), 3L)
  # ★ `fan` 与 `link` 在本数据上**坐标完全相同**（2026-10-25 实测）：
  #   geom_edge_fan 只在同一对端点之间有**多条边**时才把边分开，而 venn_net
  #   会先 `distinct()`，(基因, 来源) 每对最多一条边 → fan 退化成直线。
  #   保留该选项是为了向后兼容，不代表在这类数据上有视觉差异。
  expect_equal(unname(fp["fan"]), unname(fp["link"]))
})

# ---- 6. 非法输入必须报错，不能静默返回 NULL ----------------------------
test_that("edge.type 非法值报错（旧实现静默返回 NULL）", {
  d <- .vd()
  expect_error(venn_net(d, edge.type = "xxx"), "should be one of")
  # match.arg 对长度 > 1 的实参报 "'arg' must be of length 1"（不是 "should be one of"）
  expect_error(venn_net(d, edge.type = c("arc", "fan")), "length 1")
})

test_that("输入形状非法时给出可读报错（旧实现报内部错误）", {
  d <- .vd()
  # 旧实现：print 一句提示后继续执行 → "attempt to set 'colnames' on an object
  # with less than two dimensions"
  expect_error(venn_net("not a df"), "data.frame")
  expect_error(venn_net(data.frame(a = 1)), "\u81f3\u5c11\u9700\u8981\u4e24\u5217")
  # 旧实现：3 列时 colnames<- 赋 2 个名字 → "names attribute [2] must be ..."
  expect_error(venn_net(data.frame(a = 1, b = 2, c = 3)),
               "\u65e0\u6cd5\u4ece\u5217\u540d\u8bc6\u522b")
  expect_error(venn_net(d, gene_col = "nope"), "\u627e\u4e0d\u5230")
})

# ---- 7. 列解析：显式 > 别名 > 位置 -------------------------------------
test_that("列解析支持显式指定、别名与位置回退", {
  d <- .vd()
  ref <- .all_labels(venn_net(d))
  expect_equal(.all_labels(venn_net(d, gene_col = "gene", source_col = "source")),
               ref)
  # 别名（from / to / set 等）
  m <- data.frame(from = d$gene, set = d$source, stringsAsFactors = FALSE)
  expect_equal(.all_labels(venn_net(m)), ref)
  # 两列且无任何别名 → 按位置（第 1 列 = 基因、第 2 列 = 来源）
  m2 <- data.frame(A = d$gene, B = d$source, stringsAsFactors = FALSE)
  expect_equal(.all_labels(venn_net(m2)), ref)
  # 列号也可以
  expect_equal(.all_labels(venn_net(m2, gene_col = 1, source_col = 2)), ref)
})

# ---- 8. 废弃参数给警告而不是静默改义 -----------------------------------
test_that("node.color 已废弃：给警告且不影响出图", {
  d <- .vd()
  expect_warning(venn_net(d, node.color = "Spectral"), "node.color")
  p <- suppressWarnings(venn_net(d, node.color = "Spectral"))
  expect_equal(.n_legends(p), 2L)
})

# ---- 9. style × layout 正交，且非 fr 布局不再报错 ----------------------
test_that("四档 style 出图且边透明度不同（旧实现没有 style 轴）", {
  d <- .vd()
  al <- vapply(c("default", "journal", "cytoscape", "presentation"),
               # ⚠️ ggraph 把 `alpha=` 存进 `edge_alpha`（边专属美学名），
               #    读 `aes_params$alpha` 会得到 NULL —— 2026-10-25 实测踩到。
               function(st) venn_net(d, style = st)$layers[[1]]$aes_params$edge_alpha,
               numeric(1))
  expect_equal(unname(al), c(0.35, 0.30, 0.45, 0.55))
})

test_that("非力导向布局不再因 niter 报错（共享层 2026-10-25 前的缺陷）", {
  d <- .vd()
  # 共享层曾写死 create_layout(niter = 3000) → kk / circle / grid 等全部报
  # "unused argument (niter = 3000)"，而 venn_net 的默认布局正是 kk。
  for (ly in c("kk", "circle", "grid", "nicely")) {
    expect_error(venn_net(d, graph.layout = ly), NA)
  }
})

test_that("graph.layout 改变节点坐标（布局参数真的生效）", {
  d <- .vd()
  xy <- function(ly) {
    b <- ggplot2::ggplot_build(venn_net(d, graph.layout = ly))
    b$data[[.node_layer(b)]][, c("x", "y")]
  }
  expect_false(isTRUE(all.equal(xy("kk"), xy("circle"))))
})

# ---- 10. 容量与过滤 ------------------------------------------------------
test_that("max_nodes 生效，且裁剪后点大小按子图重算", {
  d <- .vd()
  b_full <- ggplot2::ggplot_build(venn_net(d, max_nodes = Inf))
  b_cap  <- ggplot2::ggplot_build(venn_net(d, max_nodes = 20))
  expect_equal(nrow(b_full$data[[.node_layer(b_full)]]), 153L)
  expect_equal(nrow(b_cap$data[[.node_layer(b_cap)]]), 20L)
  # 约定：点大小的上界只在子图内取到 → 裁剪后必须仍有节点落到 max(node.size)
  expect_equal(max(b_cap$data[[.node_layer(b_cap)]]$size), 10)
})
