# 出图风格预设契约（2026-10-22）
#
# 由来：boss 反馈「GO 富集文献里柱状图比气泡图用得多，想要预设」，并要求其余
# 绘图模块也按文献常用形式补预设。这个文件把 style 的**唯一承诺**锁死：
#
#   风格只改视觉，不改数据与统计口径。
#
# 为什么必须断言这一点：只怕「出图不报错」的话，「某风格悄悄少画一半条目」
# 这类缺陷会全绿通过。故每个风格都断言**图里承载的条目集合与 default 完全一致**。
#
# 覆盖：
#   · go_barplot 6 种（classic / grouped / significance 为文献形制）
#   · go_dotplot 4 种（genratio 为文献 GeneRatio 口径）
#   · bar_plot / degree_plot 的通用 journal / presentation
#   · 非法取值必须报错（match.arg），不做静默降级

go_fixture <- function() {
  skip_if_not_installed("clusterProfiler", minimum_version = NULL)
  data(xfbdf, package = "tcmnetpharma", envir = environment())
  res <- tryCatch(
    tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human",
               pvalueCutoff = 0.05, qvalueCutoff = 0.2),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(res) || is.null(res$go), "富集结果不可用")
  res$go
}

# items_of() / items_diff() 来自 helper-plots.R（testthat 自动加载）。
# 它默认走 $data；patchwork 形态下会自动穿透子图取并集（防御性保留，
# 见该文件头部说明）。

test_that("go_barplot 的每种风格都出图，且条目集合与 default 一致", {
  go <- go_fixture()
  styles <- c("default", "classic", "grouped", "significance",
              "journal", "presentation")
  ref <- NULL
  for (st in styles) {
    p <- go_barplot(go, top = 5, style = st)
    expect_true(inherits(p, "ggplot"), info = st)
    it <- items_of(p)
    expect_gt(length(it), 0L)
    if (is.null(ref)) {
      ref <- it
    } else {
      # 承诺：风格不改入选条目（只改 x 轴口径 / 配色 / 分面 / 主题）
      expect_identical(it, ref, info = st)
    }
  }
})

test_that("柱状图不用色阶：classic / grouped / significance 都按本体三色（分类标度）", {
  go <- go_fixture()

  # boss 2026-10-23 决定：「气泡图类用色阶，柱状图不用色阶了，按 BP/CC/MF
  # 分类给 3 个颜色」。这条测试锁住**柱状图不许出现连续色阶**。
  #
  # 为什么这条不能省：色阶与分类色都能"出图成功"，inherits("ggplot") 也都能过。
  # 只看「能出图」的话，哪天有人把 scale_fill_gradientn 加回来，测试照样全绿。
  # 判据用 fill 标度的**离散/连续**属性（实测：scale_fill_manual ->
  # "ScaleDiscrete"，scale_fill_gradientn -> "ScaleContinuous"）。
  fill_scale_of <- function(p) {
    sc <- p$scales$scales[vapply(p$scales$scales,
                                 function(x) "fill" %in% x$aesthetics, logical(1))]
    if (!length(sc)) return(NULL)
    sc[[1]]
  }

  for (st in c("classic", "grouped", "significance")) {
    p <- go_barplot(go, top = 3, style = st)
    expect_s3_class(p, "ggplot")
    # 柱状图整体不该是拼图对象（本轮的 patchwork 方案已被"三色分类"取代）
    expect_false(inherits(p, "patchwork"), info = st)

    sc <- fill_scale_of(p)
    expect_false(is.null(sc), info = st)
    expect_true(inherits(sc, "ScaleDiscrete"), info = st)
    expect_false(inherits(sc, "ScaleContinuous"), info = st)
    # 颜色确实映射到本体（而不是 p.adjust 之类的连续量）
    expect_identical(sc$aesthetics, "fill", info = st)
  }
})

test_that("grouped 保留分面：BP/CC/MF 三段，共用一条离散图例", {
  go <- go_fixture()
  p <- go_barplot(go, top = 3, style = "grouped")

  # 三段靠 facet 实现（分类色全图同一个标度，不需要独立标度）。
  # ⚠️ 判据走 ggplot_build() 的 layout，**不要**去读 `p$facet$params$facets`：
  #    ggplot2 4.x 里 FacetGrid 的字段是 rows / cols（quosures），
  #    `params$facets` 已不存在 —— 直接读会是 NULL，断言恒失败（2026-10-23 实测）。
  #    layout 是渲染结果的真实结构，版本差异小，也更接近"用户看到什么"。
  b <- ggplot2::ggplot_build(p)$layout$layout
  # 2026-10-24：分面列由 ONTOLOGY 改为 .grp（来源/本体统一的显式分组键，
  # 见 R/enrich-source.R 的 .tcm_resolve_group()）。GO 数据下 .grp 仍是 BP/CC/MF。
  expect_true(".grp" %in% names(b))
  expect_equal(nrow(b), 3L)
  expect_setequal(as.character(b$.grp), c("BP", "CC", "MF"))

  # 图例只有一条（离散）。⚠️ 别去读 `sc[[1]]$get_limits()`：
  #   离散标度在 build 之前拿到的是调色板索引 "0"/"1"，不是本体名
  #   （2026-10-23 实测踩到）。改用**渲染结果**判：
  bld <- ggplot2::ggplot_build(p)
  sc <- p$scales$scales[vapply(p$scales$scales,
                               function(x) "fill" %in% x$aesthetics, logical(1))]
  expect_length(sc, 1L)
  expect_true(inherits(sc[[1]], "ScaleDiscrete"))

  # boss 要的是「按 BP/CC/MF 分类给 3 个颜色」：**每个本体恰好一种颜色，
  # 三个颜色互不相同**。若有人偷偷换回渐变，每个 PANEL 内会出现多种 fill
  # （或者 fill 数远超 3），这条立刻失败。
  bar <- bld$data[[1]]
  lay <- bld$layout$layout
  fill_by_onto <- tapply(bar$fill, lay$.grp[match(bar$PANEL, lay$PANEL)],
                         function(x) length(unique(x)))
  # tapply 返回带 dim 的一维数组，unname() 不会去掉 dim -> 断言会因维度不符失败
  expect_equal(as.integer(fill_by_onto[c("BP", "CC", "MF")]), c(1L, 1L, 1L))
  expect_equal(length(unique(bar$fill)), 3L)

  # 数据未被分面切碎：$data 仍是全量条目（这是改用 facet 后重新变简单的证据）
  expect_equal(nrow(p$data), length(items_of(p)))

  # 传进来的总标题仍在（走 labs(title = )，不再需要 plot_annotation）
  p2 <- go_barplot(go, top = 3, style = "grouped", title = "我的富集图")
  expect_true(any(grepl("我的富集图", ggplot_titles(p2))))
  p3 <- go_barplot(go, top = 3, style = "grouped", title = NULL)
  expect_false(any(grepl("^NA$", ggplot_titles(p3))))
})

test_that("go_dotplot 的每种风格都出图，且条目集合与 default 一致", {
  go <- go_fixture()
  ref <- NULL
  for (st in c("default", "genratio", "journal", "presentation")) {
    p <- go_dotplot(go, top = 5, style = st)
    expect_true(inherits(p, "ggplot"), info = st)
    it <- items_of(p)
    if (is.null(ref)) {
      ref <- it
    } else {
      expect_identical(it, ref, info = st)
    }
  }
})

test_that("genratio 用的是 GeneRatio 口径（x 值 = k/n，落在 0-1）", {
  go <- go_fixture()
  p <- go_dotplot(go, top = 5, style = "genratio")
  d <- p$data
  # 仅在 GeneRatio 列存在时断言（否则函数会退回 Rich Factor，属另一条路径：
  # 那条路径已由「各风格出图」用例覆盖，其 x 轴名为 Rich Factor）
  if ("GeneRatio" %in% names(d)) {
    x <- d$x_value
    expect_true(all(is.finite(x)))
    expect_true(all(x > 0 & x <= 1))
    # 与**同一批条目**的 GeneRatio 重算值一致（口径必须是 k/n，不能掺 Rich Factor）
    k <- as.numeric(sub("/.*$", "", as.character(d$GeneRatio)))
    n <- as.numeric(sub("^.*/", "", as.character(d$GeneRatio)))
    expect_equal(x[order(d$Description)], (k / n)[order(d$Description)],
                 tolerance = 1e-12)
  }
})

test_that("bar_plot / degree_plot 的 journal、presentation 出图且条目数不变", {
  data("kegg.filter", package = "tcmnetpharma", envir = environment())
  n_def <- nrow(bar_plot(kegg.filter, top = 5, style = "default")$data)
  for (st in c("journal", "presentation")) {
    p <- bar_plot(kegg.filter, top = 5, style = st)
    expect_true(inherits(p, "ggplot"), info = st)
    expect_identical(nrow(p$data), n_def, info = st)
  }

  data("network_data", package = "tcmnetpharma", envir = environment())
  for (st in c("default", "journal", "presentation")) {
    p <- degree_plot(as.data.frame(network_data), top = 3, style = st)
    expect_true(inherits(p, "ggplot"), info = st)
  }
})

test_that("go_barplot 的 top 是「每个本体各 top 条」，不是全局 top", {
  go <- go_fixture()
  # 实测语义（2026-10-22 核对）：函数内先 group_by(ONTOLOGY) 再 slice(1:top)，
  # 所以 top=5 最多给出 5*3=15 条（BP/CC/MF 各 5），不是 5 条。
  # 锁这条是为防「顺手改成全局 top」—— 那会让用户拿到的图莫名少一半。
  n5 <- nrow(go_barplot(go, top = 5, style = "classic")$data)
  n9 <- nrow(go_barplot(go, top = 9, style = "classic")$data)
  n_ont <- length(unique(as.character(go@result$ONTOLOGY)))
  expect_lte(n5, 5L * n_ont)
  expect_gt(n5, 5L)          # 若哪天变成全局 top，这条会失败
  expect_gt(n9, n5)          # top 增大条目必须变多（否则 top 被静默忽略）
})

test_that("各风格下 top 的效果一致（风格不能改变入选条目数）", {
  go <- go_fixture()
  # ⚠️ 必须用 items_of() 而不是 nrow(p$data)：
  #    grouped 风格返回 patchwork，`p$data` 只拿到**第一段**（BP）的条目，
  #    直接 nrow 会得到 4 而不是 12（2026-10-23 实测假失败）。
  ns <- vapply(
    c("default", "classic", "grouped", "significance",
      "journal", "presentation"),
    function(st) length(items_of(go_barplot(go, top = 4, style = st))),
    integer(1)
  )
  expect_equal(unname(ns), rep(ns[[1]], length(ns)))
  expect_gt(ns[[1]], 0L)
})

test_that("非法风格值必须报错（不静默出图）", {
  data("kegg.filter", package = "tcmnetpharma", envir = environment())
  expect_error(bar_plot(kegg.filter, style = "cytoscape"))
  data(xfbdf, package = "tcmnetpharma", envir = environment())
  expect_error(
    go_barplot(xfbdf, style = "genratio"),
    info = "genratio 是 go_dotplot 的预设，不该被 go_barplot 接受"
  )
})
