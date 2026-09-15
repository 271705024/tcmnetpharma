# ===========================================================================
# pathway_ccplot 渲染与输入校验回归测试（2026-09-15，rev1 by p42 lead）
#
# 背景
#   在 P4.2 适配层冒烟测试（36 个导出函数 → run_and_capture）中发现
#   pathway_ccplot 在**任何**输入下 ggsave 都失败：
#
#     Error: Unknown colour name: KEGG
#
#   根因（B1）
#     R/pathway_ccplot.R 中 `ggraph::geom_edge_diagonal(aes(color = node1.node.branch))`
#     使用的是 **edge_colour** 美学（ggraph 的边色阶独立于 ggplot2 的 colour
#     色阶），而图层只声明了 `scale_colour_manual()`。node 的 colour 被正确映射，
#     edge 的取值则保持为**原始字符串**（如 "KEGG"、通路 Description），
#     在渲染期被当作颜色名解析 → `Unknown colour name`。
#   修法：补 `ggraph::scale_edge_colour_manual(values = ...)`。
#
#   第二个缺陷（B2）
#     输入类型非法时原代码执行 `print("The data format must be S4 object or data
#     frame.")` 后**继续往下跑**，用户拿到的是下游 `separate_rows(NULL)` 的
#     晦涩报错，而非可读提示；0 行输入同理。
#   修法：改为显式 `stop()`，并在 drop_na 后增加 nrow == 0 检查。
#
# 为什么测试要**真正渲染**
#   rev3 的既有回归测试（test-smoke-all-exports.R 第 6 节）统一走
#   `with_null_device()`（pdf(NULL)）只验证「构建不报错」。B1 的特殊性在于
#   **对象构建成功、失败发生在图形设备渲染期**，pdf(NULL) 同样会抛错但因
#   测试只断言构造过程…… 反过来说：本缺陷恰恰说明「构造成功」不足以证明可渲染。
#   故本文件对 B1 一律走真实位图设备（png + ggsave），并以**文件字节数**作为
#   「确实画出了内容」的判据（空白 PNG 约 2 KB，真实通路圆图 > 100 KB）。
#
# 注：本文件同时兼容两种执行方式
#   (a) 已安装包：devtools::test() / testthat::test_file()
#   (b) 未安装包：由 logs/_l2_testthat.R 驱动，把包内对象注入测试环境。
# ===========================================================================

# ---------------------------------------------------------------------------
# 工具：错误消息安全取值（不依赖 rlang::`%||%`，保持测试文件自包含）
# ---------------------------------------------------------------------------
.or_none <- function(x) if (is.null(x)) "(none)" else x

# ---------------------------------------------------------------------------
# 工具：渲染到临时 PNG 并回传文件字节数（>0 表示真的画出了东西）
#   使用 png() 而非 ggsave()：ggsave 需要可写的工作目录，而 png() 写 tmpdir。
# ---------------------------------------------------------------------------
render_to_png <- function(expr, width = 1200, height = 800) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  grDevices::png(f, width = width, height = height, res = 96)
  ok <- TRUE
  err <- NULL
  tryCatch(
    {
      # print() 是必要的：ggplot 对象在非交互/脚本上下文不会自动绘制
      print(force(expr))
    },
    error = function(e) {
      ok <<- FALSE
      err <<- conditionMessage(e)
    }
  )
  grDevices::dev.off()
  size <- if (file.exists(f)) file.info(f)$size else 0
  list(ok = ok, size = size, err = err)
}

# 与 test-smoke-all-exports.R 保持一致的合成 fixture（32 行，含 geneID 的 "/" 分隔）
make_cc_fixture <- function() {
  set.seed(42)
  n <- 12L
  data.frame(
    ONTOLOGY    = rep(c("BP", "CC", "MF"), each = 4L),
    ID          = sprintf("hsa%05d", 1:n),
    Description = sprintf("synthetic pathway term number %02d", 1:n),
    GeneRatio   = paste0(3, "/", 20 + (1:n %% 15)),
    BgRatio     = paste0(50 + (1:n %% 30), "/", 2000 + (1:n %% 100)),
    pvalue      = round(stats::runif(n, 1e-6, 0.05), 6),
    p.adjust    = round(stats::runif(n, 1e-5, 0.2), 6),
    qvalue      = round(stats::runif(n, 1e-5, 0.25), 6),
    geneID      = vapply(seq_len(n), function(i) {
      paste(sprintf("GENE%03d", ((i * 2 - 1) %% 29) + c(0, 1, 2)), collapse = "/")
    }, character(1)),
    Count       = rep(c(3L, 4L, 5L), length.out = n),
    stringsAsFactors = FALSE
  )
}

# 空白 PNG 的判别阈值：876x656 的空白位图约 2 KB，真实通路圆图远大于此。
BLANK_MAX_BYTES <- 3000L

# ===========================================================================
# 1. B1 回归：三条参数路径都必须渲染出**非空白**图像
#    修复前：三者均抛 `Unknown colour name`，size = 0。
# ===========================================================================
test_that("pathway_ccplot root=NULL 能渲染出非空白图像", {
  d <- make_cc_fixture()
  r <- render_to_png(pathway_ccplot(d, top = 5))
  expect_true(r$ok, info = paste("render error:", .or_none(r$err)))
  expect_gt(r$size, BLANK_MAX_BYTES)
})

test_that("pathway_ccplot root='KEGG' 能渲染出非空白图像", {
  d <- make_cc_fixture()
  r <- render_to_png(pathway_ccplot(d, top = 5, root = "KEGG"))
  expect_true(r$ok, info = paste("render error:", .or_none(r$err)))
  expect_gt(r$size, BLANK_MAX_BYTES)
})

test_that("pathway_ccplot label.name='ID' 能渲染出非空白图像", {
  d <- make_cc_fixture()
  r <- render_to_png(pathway_ccplot(d, top = 5, label.name = "ID"))
  expect_true(r$ok, info = paste("render error:", .or_none(r$err)))
  expect_gt(r$size, BLANK_MAX_BYTES)
})

test_that("pathway_ccplot 接受内置 enrichResult 对象 KK 并渲染成功", {
  r <- render_to_png(pathway_ccplot(TCMNP::KK, top = 5, root = "KEGG"))
  expect_true(r$ok, info = paste("render error:", .or_none(r$err)))
  expect_gt(r$size, BLANK_MAX_BYTES)
})

# ===========================================================================
# 2. B2 回归：非法输入必须给出**可读的**错误，而不是下游晦涩报错或静默通过
# ===========================================================================
test_that("pathway_ccplot 对 NULL 输入报出明确的类型错误", {
  expect_error(pathway_ccplot(NULL), "must be an S4 object")
})

test_that("pathway_ccplot 对非 data.frame 输入报错并回显实际类型", {
  expect_error(pathway_ccplot("not a data frame"), "must be an S4 object")
  expect_error(pathway_ccplot(1:10), "got: integer")
})

test_that("pathway_ccplot 对 0 行输入报出明确的空数据错误", {
  expect_error(pathway_ccplot(make_cc_fixture()[0, ]), "0 rows")
})

test_that("pathway_ccplot 的 top 超过可用行数时按实际行数渲染（不产生 NA 幽灵节点）", {
  # 12 行 fixture 里把前 5 行 geneID 置 NA，drop_na 后剩 7 行。
  # 历史行为：`data[1:5, ]` 在 top > nrow 时用 NA 补齐 → 生成标签为 "NA"
  # 的幽灵节点。现行为：top 被收敛到 nrow(data)，正常出图。
  d <- make_cc_fixture()
  d$geneID[1:5] <- NA_character_
  expect_no_error(render_to_png(pathway_ccplot(d, top = 5)))
  r <- render_to_png(pathway_ccplot(d, top = 5))
  expect_true(r$ok, info = paste("render error:", .or_none(r$err)))
  expect_gt(r$size, BLANK_MAX_BYTES)
})

test_that("pathway_ccplot 的 top 超过总行数时仍能正常渲染", {
  d <- make_cc_fixture()
  r <- render_to_png(pathway_ccplot(d, top = 999))
  expect_true(r$ok, info = paste("render error:", .or_none(r$err)))
  expect_gt(r$size, BLANK_MAX_BYTES)
})
