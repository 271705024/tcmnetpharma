# 配色体系契约（2026-10-22）
#
# 由来：boss「配色问题需要考虑，从高分文献或配色网站拔几套好用的配色体系
# 放设置选项」。这个文件锁死三条承诺：
#
#   1) **色值有出处**：每个色板必须登记 source（期刊色板/色盲友好/ColorBrewer），
#      且色值合法（6 位十六进制）——防止以后有人凭手感改色。
#   2) **只改颜色，不改数据**：同一份数据换配色，条目集合必须完全一致。
#   3) **可复现**：同一份数据 + 同一配色，两次出图颜色必须完全相同
#      （tcm_sankey 原来用 sample() 打乱配色，这条断言就是它的回归锚点）。

all_palettes <- tcm_palette_names()

test_that("每个色板都登记了出处，且色值合法", {
  info <- tcm_palette_names(names_only = FALSE)
  expect_true(all(nzchar(info$source)))
  expect_true(all(nzchar(info$label)))
  expect_true(all(info$n >= 3L))
  for (nm in all_palettes) {
    cols <- tcm_palette(nm)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)), info = nm)
    expect_identical(anyDuplicated(cols), 0L, info = nm)
  }
})

test_that("色盲友好色板存在，且含 Okabe-Ito 的规范色", {
  expect_true("okabe" %in% all_palettes)
  cols <- tcm_palette("okabe")
  # 严格断言：Okabe & Ito (2008) 色彩通用设计 8 色必须全部在内。
  # 注意不能写成逐元素 `A %in% cols | B %in% cols` —— 那样只要命中一个
  # 元素就过，是假通过（铁律 28）。
  oi8 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
           "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  expect_true(all(oi8 %in% cols))
})

test_that("取色接口：n 切片、超量插值、非法名报错", {
  expect_length(tcm_palette("npg", 3), 3L)
  expect_identical(tcm_palette("npg", 3), tcm_palette("npg")[1:3])
  expect_length(tcm_palette("jama", 20), 20L)   # 7 色板取 20 -> 插值
  expect_length(tcm_palette("viridis", 5, type = "continuous"), 5L)
  expect_error(tcm_palette("no_such_palette"), "unknown palette")
  expect_error(tcm_palette("npg", 0), "positive")
})

go_fixture <- function() {
  data(xfbdf, package = "tcmnetpharma", envir = environment())
  res <- tryCatch(
    tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human",
               pvalueCutoff = 0.05, qvalueCutoff = 0.2),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(res) || is.null(res$go), "\u5bcc\u96c6\u7ed3\u679c\u4e0d\u53ef\u7528")
  res$go
}

items_of <- function(p) {
  d <- p$data
  if (is.null(d) || !"Description" %in% names(d)) return(character(0))
  sort(unique(as.character(d$Description)))
}

test_that("换配色不改数据：条目集合与不指定配色时完全一致", {
  go <- go_fixture()
  for (st in c("default", "classic", "grouped", "significance")) {
    base <- items_of(go_barplot(go, top = 5, style = st))
    for (pal in c("npg", "okabe", "viridis")) {
      p <- go_barplot(go, top = 5, style = st, palette = pal)
      expect_true(inherits(p, "ggplot"), info = paste(st, pal))
      expect_identical(items_of(p), base, info = paste(st, pal))
    }
  }
  base_dot <- items_of(go_dotplot(go, top = 5))
  for (pal in c("npg", "viridis")) {
    p <- go_dotplot(go, top = 5, palette = pal)
    expect_identical(items_of(p), base_dot, info = pal)
  }
})

test_that("配色确实作用到图上（不是被忽略）", {
  go <- go_fixture()
  p_npg <- go_barplot(go, top = 5, style = "classic", palette = "npg")
  p_okabe <- go_barplot(go, top = 5, style = "classic", palette = "okabe")
  # 取第一层的 fill 标度，断言取值不同 —— 否则「palette 参数被静默忽略」也会全绿
  fills <- function(p) {
    sc <- p$scales$scales[vapply(p$scales$scales,
                                 function(x) "fill" %in% x$aesthetics, logical(1))]
    if (!length(sc)) return(NULL)
    sc[[1]]$palette(3)
  }
  fa <- fills(p_npg); fb <- fills(p_okabe)
  testthat::skip_if(is.null(fa) || is.null(fb), "标度取值不可读")
  expect_false(identical(fa, fb))
})

test_that("tcm_sankey 配色可复现（原来 sample() 打乱）", {
  data(xfbdf, package = "tcmnetpharma", envir = environment())
  df <- as.data.frame(xfbdf)[1:12, c("herb", "molecule", "target")]
  fill_of <- function(p) {
    sc <- p$scales$scales[vapply(p$scales$scales,
                                 function(x) "fill" %in% x$aesthetics, logical(1))]
    if (!length(sc)) return(NULL)
    sc[[1]]$palette
  }
  p1 <- tcm_sankey(df)
  p2 <- tcm_sankey(df)
  f1 <- fill_of(p1); f2 <- fill_of(p2)
  testthat::skip_if(is.null(f1) || is.null(f2), "\u586b\u8272\u6807\u5ea6\u4e0d\u53ef\u8bfb")
  expect_identical(f1, f2)
})

test_that("色板 kind 只有 qual/seq/div（前端分组依赖这三个值）", {
  # GUI 按 kind 把下拉分成「分类色 / 连续色 / 发散色」三组
  # （见 gui-app/www/js/app.js 的 PALETTE_GROUPS）。若某个色板写成
  # "diverging" 之类的别名，它会掉出所有分组 -> 下拉里根本看不到，
  # 而界面上不会有任何报错（静默缺失）。
  info <- tcm_palette_names(names_only = FALSE)
  expect_true(all(info$kind %in% c("qual", "seq", "div")),
              info = paste(unique(info$kind), collapse = ","))
  # 三类都要有，否则某个分组标题下永远是空的
  expect_true(all(c("qual", "seq", "div") %in% info$kind))
})

test_that("非法配色值必须报错", {
  data("kegg.filter", package = "tcmnetpharma", envir = environment())
  expect_error(bar_plot(kegg.filter, palette = "no_such_palette"), "unknown palette")
})
