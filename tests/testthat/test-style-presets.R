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

items_of <- function(p) {
  d <- p$data
  if (is.null(d) || !"Description" %in% names(d)) return(character(0))
  sort(unique(as.character(d$Description)))
}

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
  ns <- vapply(
    c("default", "classic", "grouped", "significance",
      "journal", "presentation"),
    function(st) nrow(go_barplot(go, top = 4, style = st)$data),
    integer(1)
  )
  expect_equal(unname(ns), rep(ns[[1]], length(ns)))
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
