# cir_plot 画布越界回归测试（2026-10-23，boss 反馈「富集环形图图片生成异常」）
#
# 为什么不用 expect_s3_class 就够：
#   cir_plot 是**直绘型**（circlize + ComplexHeatmap，无返回值，函数末尾
#   circos.clear()），「能不能出图」它天天都在过 —— 真正的缺陷是**环画到画布
#   外**（画布跨度不够且不对称），设备上表现为整环偏移、部分轨道被裁。
#   这类缺陷只有检查**渲染出来的像素**才抓得住。
#
# 判据：用与 GUI 完全一致的抓图参数（png 9×9 in @ 96 dpi，即 .canvas_for 的
# square 规格）出图，断言**四条边的中点条带是背景色**（环未出界），同时整图
# **不是空白**（确实画了东西，防止「两次都空」的假通过，铁律 28）。
#
# ⚠️ 判据位置必须是**边中点**而不是四角（2026-10-23 反向证伪时发现）：
#   圆环越界切的是边中段（旧参数成图左缘中点被环体着色），
#   四角在旧成图里也全是白的 —— 拿角当判据等于没测。
#   图例的位置是否合适（是否压环/被裁）不在此测，靠目视评审锁定默认值。

test_that("cir_plot 在 GUI 同款 9x9 画布上不出界（四角留白）", {
  skip_if_not_installed("png")
  # 2026-10-27：夹具由已移除的 kegg.filter 换为合成富集表（只量几何，不依赖通路内容）
  kegg.filter <- make_enrich_fixture()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 9, height = 9, units = "in", res = 96)
  cir_plot(kegg.filter, top = 10)
  invisible(dev.off())

  img <- png::readPNG(tmp)
  h <- dim(img)[1]
  w <- dim(img)[2]
  # GUI 实际产出即 864×864；若设备参数哪天变了，这里要跟着改（守卫性断言）
  expect_equal(c(w, h), c(864L, 864L))

  is_background <- function(block) {
    # png 设备默认白底；容许轻微抗锯齿（全部像素接近白色即视为背景）
    all(block[, , 1] > 0.98) && all(block[, , 2] > 0.98) && all(block[, , 3] > 0.98)
  }
  k <- 24L    # 条带厚度
  m <- 60L    # 条带半长（以边中点为中心）
  edges <- list(
    top_center    = img[1:k, (w %/% 2 - m):(w %/% 2 + m), , drop = FALSE],
    bottom_center = img[(h - k + 1L):h, (w %/% 2 - m):(w %/% 2 + m), , drop = FALSE],
    left_center   = img[(h %/% 2 - m):(h %/% 2 + m), 1:k, , drop = FALSE],
    right_center  = img[(h %/% 2 - m):(h %/% 2 + m), (w - k + 1L):w, , drop = FALSE]
  )
  for (nm in names(edges)) {
    expect_true(is_background(edges[[nm]]),
                info = sprintf("%s 边中点应为背景色（环出界会在此着色）", nm))
  }

  # 整图不能是空白 —— 防「画布越大环越小直到消失」这种反向退化
  center <- img[(h %/% 2 - 100L):(h %/% 2 + 100L),
                (w %/% 2 - 100L):(w %/% 2 + 100L), , drop = FALSE]
  expect_false(all(center[, , 1] > 0.98), info = "中心区域应有内容（环体）")
})
