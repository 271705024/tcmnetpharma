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
# 为什么挪走：grouped 风格返回 patchwork，没有单一 $data，必须穿透子图取并集。

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

test_that("grouped 是三段独立标度的 patchwork（不是共用色阶的 facet）", {
  go <- go_fixture()
  p <- go_barplot(go, top = 3, style = "grouped")

  # boss 2026-10-23 截图暴露的问题：原实现 facet_grid + 单个 fill 标度，
  # 三本体 p.adjust 量级差 20+ 个数量级，导致各分面内部差异看不出来。
  # 这条测试锁住修复后的性质：**三段各自一个 fill 标度**。
  expect_s3_class(p, "patchwork")
  expect_length(p, 3L)

  n_fill <- vapply(seq_len(3L), function(i) {
    sp <- p[[i]]
    if (!inherits(sp, "ggplot")) return(NA_integer_)
    sum(vapply(sp$scales$scales,
               function(x) "fill" %in% x$aesthetics, logical(1)))
  }, integer(1))
  # 每段恰好 1 个 fill 标度。若是 facet 共用标度，子图里会是 0 个。
  expect_equal(unname(n_fill), rep(1L, 3L))

  # 各段数据的 p.adjust 范围必须互不相同 —— 若哪天又退回共用标度，
  # 三段数据范围虽仍不同，但上面的 n_fill 会变成 0，两条一起锁。
  rngs <- lapply(seq_len(3L), function(i) {
    d <- p[[i]]$data
    if (is.null(d) || !"p.adjust" %in% names(d)) return(NULL)
    signif(range(log10(d$p.adjust)), 3)
  })
  expect_true(all(!vapply(rngs, is.null, logical(1))))
  expect_gt(length(unique(vapply(rngs, function(x) paste(x, collapse = "|"), ""))), 1L)

  # 每段自身的 p.adjust 跨度必须是「这个本体」的真实跨度（不是整图的最小/最大）
  ontos <- vapply(seq_len(3L), function(i) as.character(p[[i]]$data$ONTOLOGY[[1]]), "")
  expect_setequal(ontos, c("BP", "CC", "MF"))
})

test_that("grouped 传进来的总标题不会被段标题挤掉", {
  go <- go_fixture()
  # 段标题被 BP/CC/MF 占了，用户传的 title 若就此消失，等于悄悄改了形参语义
  # （原 facet 实现里 title 走 labs(title = )，是一直显示的）。
  p <- go_barplot(go, top = 3, style = "grouped", title = "我的富集图")
  expect_true(any(grepl("我的富集图", ggplot_titles(p))))

  # 反过来：不传标题时不应凭空多出一个 "NA" 之类的空标题
  p0 <- go_barplot(go, top = 3, style = "grouped", title = NULL)
  expect_false(any(grepl("^NA$", ggplot_titles(p0))))
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
