# R2.4 图面质量：多行 y 轴标签的槽位机制 + 画布尺寸建议（2026-10-24）
#
# 断言一律写成「能被旧实现证伪」的形式：每条注释写明旧实现的实际取值。
#
# 背景：ggplot 离散 y 的位置间距恒为 1 个 level；`str_wrap` 折行后的标签以该
# 位置垂直居中渲染，行数 × 行高 > 1 个 level 时必然压到邻居。R2.4 给第 i 条
# 分配 k_i 个 level（k_i = 标签行数），上下插 placeholder 使「占用 level 数 = 行数」。
# 配套 `tcm_plot_size()` 按槽位数算建议画布（包层不知道最终画布多大）。

# ---- 造数：长度可控的富集结果（含 ontology 以便分组）-----------------------
.slot_demo <- function(n = 12L, words = 6L) {
  set.seed(7)
  cnt <- rep(8:40, length.out = n)
  pool <- c("regulation", "positive", "negative", "signaling", "response",
            "cellular", "metabolic", "developmental", "immune", "apoptotic")
  data.frame(
    Description = vapply(seq_len(n), function(i) paste0(
      "GO:", sprintf("%07d", 6000L + i), " ",
      paste(sample(pool, words, TRUE), collapse = " ")), character(1)),
    GeneRatio = paste0(cnt, "/200"),
    BgRatio   = "50/20000",
    Count     = cnt,
    pvalue    = 10^(-seq(2, 10, length.out = n)),
    p.adjust  = 10^(-seq(2, 9, length.out = n)),
    qvalue    = 10^(-seq(2, 8, length.out = n)),
    geneID    = "TP53/EGFR/AKT1",
    ontology  = rep(c("BP", "CC", "MF"), length.out = n),
    stringsAsFactors = FALSE
  )
}

# ---- 1. 槽位数 = 各行数之和（不是条目数）---------------------------------
test_that("槽位轨道长度等于各行标签行数之和", {
  d <- .slot_demo(12L, words = 6L)
  p <- enrich_barplot(d, top = "full", layout = "segmented", text.width = 30)
  py <- tcmnetpharma:::.enrich_bar_prep_y(
    p$data[, setdiff(names(p$data), c(".ykey", ".ylab_txt", ".n_lines", ".pos"))],
    text.width = 30
  )
  n_items <- py$n_items
  n_lines <- py$n_lines_total
  # 旧实现没有槽位概念：level 数 == 条目数，且 n_lines_total 与 level 数无关。
  expect_gt(n_lines, n_items)              # 造数保证折行 → 行数 > 条目数
  expect_identical(py$n_levels, n_lines)   # ★ 一一对应：占的 level 数 == 行数
  expect_gt(py$n_levels, n_items)
})

# ---- 2. 单行标签不引入多余空白 -------------------------------------------
test_that("全部单行时不插 placeholder（槽位数 == 条目数）", {
  d <- .slot_demo(6L, words = 1L)          # 1 个词 → 不会折行
  py <- tcmnetpharma:::.enrich_bar_prep_y(d, text.width = 200)
  expect_equal(py$n_levels, py$n_items)
  expect_equal(py$n_lines_total, py$n_items)
})

# ---- 3. 相邻真实条目的位移恰好等于「上下预留之和 + 1」--------------------
test_that("相邻真实条目的槽位位移与上下预留一致", {
  d <- .slot_demo(12L, words = 6L)
  py <- tcmnetpharma:::.enrich_bar_prep_y(d, text.width = 30)
  dd <- py$d
  pos <- sort(unique(dd$.pos))
  nl  <- dd$.n_lines[match(pos, dd$.pos)]
  below <- floor((nl - 1L) / 2)
  above <- ceiling((nl - 1L) / 2)
  gaps <- diff(pos)
  expect_equal(gaps, above[-length(above)] + below[-1L] + 1L)
})

# ---- 4. 标签块两两不重叠（槽位机制的核心承诺）----------------------------
test_that("相邻标签块的占用区间不相交", {
  d <- .slot_demo(12L, words = 6L)
  py <- tcmnetpharma:::.enrich_bar_prep_y(d, text.width = 30)
  dd <- py$d
  pos <- sort(unique(dd$.pos))
  nl  <- dd$.n_lines[match(pos, dd$.pos)]
  lo <- pos - floor((nl - 1L) / 2)
  hi <- pos + ceiling((nl - 1L) / 2)
  expect_true(all(hi[-length(hi)] <= lo[-1L] - 0L))
  # 旧实现：所有标签都居中在 pos 上、块跨度 > 1 level 时必然相交。
  expect_true(all(nl > 1L))   # 造数保证有多行，否则上面的断言没有检验力
})

# ---- 5. y 轴刻度只出现在真实条目上 ---------------------------------------
test_that("仅真实条目产生 y 轴刻度（placeholder 不出裸刻度线）", {
  d <- .slot_demo(12L, words = 6L)
  py <- tcmnetpharma:::.enrich_bar_prep_y(d, text.width = 30)
  expect_identical(length(py$breaks), py$n_items)
  expect_true(all(nzchar(py$labs[py$breaks])))   # 真实条目的标签非空
  # 早期实现只给 drop = FALSE 不给 breaks → placeholder 位置也出刻度，
  # 视觉上表现为「y 轴有一串没有文字的短横线」。
})

# ---- 6. 显示名映射不被 data.frame 剥掉 names（R1 踩过的坑）---------------
test_that("y 轴显示的是描述文本而不是内部分组键", {
  d <- .slot_demo(12L, words = 6L)
  p <- enrich_barplot(d, top = "full", layout = "segmented", text.width = 30)
  b <- ggplot2::ggplot_build(p)
  yl <- b$layout$panel_params[[1]]$y$get_labels()
  yl <- yl[!is.na(yl) & nzchar(yl)]
  expect_gt(length(yl), 0L)
  # 旧 bug：显示成 "BP\u0001GO:0006000 ..."（含控制字符的内部分组键）
  expect_false(any(grepl("\u0001", yl, fixed = TRUE)))
  expect_true(any(grepl("GO:", yl, fixed = TRUE)))
  # .pos 与 .ykey 必须一致（段分隔线靠 .pos 定位）
  expect_identical(as.integer(p$data$.ykey), p$data$.pos)
})

# ---- 7. 分面形制不吃别组的槽位（R2.4 引入后立刻修掉的回归）--------------
test_that("facet 每个面板只保留自己的槽位", {
  d <- .slot_demo(12L, words = 6L)
  p <- enrich_barplot(d, top = "full", layout = "facet", text.width = 30)
  b <- ggplot2::ggplot_build(p)
  py <- tcmnetpharma:::.enrich_bar_prep_y(d, text.width = 30, facet = TRUE)
  spans <- vapply(b$layout$panel_params, function(z) diff(z$y.range), numeric(1))
  expect_gt(length(spans), 1L)
  # 旧（错的）实现用 drop = FALSE：全局锁 limits → 每个面板跨度 == 总槽位，
  # 实测 8 条/组 × 3 组时 16 槽位被撑到 48（3 倍空白）。
  expect_true(all(spans < py$n_levels))
  # 三个面板跨度之和应接近总槽位（相邻组共享 1 个边界 level，允许少量误差）
  expect_lte(sum(spans), py$n_levels + length(spans))
  # facet 必须 drop = TRUE，否则面板拿不到"瘦身"
  expect_true(py$drop)
  expect_false({
    py2 <- tcmnetpharma:::.enrich_bar_prep_y(d, text.width = 30)
    py2$drop
  })
})

# ---- 8. tcm_plot_size：按槽位算尺寸 --------------------------------------
test_that("tcm_plot_size 随槽位数与标签宽度增长", {
  d1 <- .slot_demo(6L, words = 1L)
  d2 <- .slot_demo(18L, words = 8L)
  p1 <- enrich_barplot(d1, top = "full", layout = "segmented", text.width = 200)
  p2 <- enrich_barplot(d2, top = "full", layout = "segmented", text.width = 24)
  s1 <- tcm_plot_size(p1)
  s2 <- tcm_plot_size(p2)
  expect_named(s1, c("width", "height"))
  expect_true(all(s1 > 0) && all(s2 > 0))
  # 槽位越多、标签折行越多 → 越高
  expect_gt(s2[["height"]], s1[["height"]])
  # 宽度由「折行后单行最长」驱动：d1 单行短标签 < d2 折行后的最长行
  expect_gt(s2[["width"]], s1[["width"]])
  # 下限生效
  expect_gte(s1[["width"]], 6)
  expect_gte(s1[["height"]], 3)
})

test_that("tcm_plot_size 对非富集图硬报错", {
  p <- tcm_net(as.data.frame(network_data), view = "overview")
  expect_error(tcm_plot_size(p), "tcm_plot_size\\(\\)")
  expect_error(tcm_plot_size(42), "ggplot")
})

# ---- 9. go_dotplot / go_lollipop 同样走槽位 -------------------------------
test_that("go_dotplot 与 go_lollipop 也插入槽位", {
  d <- .slot_demo(12L, words = 6L)
  for (fn in list(go_dotplot, go_lollipop)) {
    p <- fn(d, top = "full", text.width = 30)
    nl <- nlevels(p$data$.ykey)
    expect_gt(nl, nrow(p$data))
    # 显示名不是内部分组键
    b <- ggplot2::ggplot_build(p)
    yl <- b$layout$panel_params[[1]]$y$get_labels()
    yl <- yl[!is.na(yl) & nzchar(yl)]
    expect_false(any(grepl("\u0001", yl, fixed = TRUE)))
  }
})
