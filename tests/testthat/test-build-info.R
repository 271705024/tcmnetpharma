# ===========================================================================
# 构建身份函数测试（tcmnp_build_info / tcmnp_identity）
#
# 背景
#   这两个函数于 2026-09-15 引入（L2 冒烟测试是 2026-09-14 做的），因此
#   不在 `test-smoke-all-exports.R` 覆盖的 36 个导出函数之内 —— 是本包
#   仅有的两个**从未被自动化测试执行过**的导出函数。
#
#   它们又恰是 E8 编码缺陷的**重灾区**：函数体内原本写有裸中文字符串常量，
#   导致包在 UTF-8 locale 下 library() 后调用即报
#   `Error: embedded nul in string`（而 C locale 下完全正常）。
#   详见 logs/errors-utf8-nul-encoding-2026-09-22.md。
#
# 因此本文件有两重职责
#   1. 契约测试 —— 验证两个函数的返回结构（补齐覆盖缺口）
#   2. **E8 回归守卫** —— 在 UTF-8 locale 下强制反序列化这两个函数，
#      确保裸中文字符串常量不会再次混入
#
# 注：locale 相关断言在无法设置 UTF-8 locale 的环境下 skip，而不是失败
#     —— 但 C locale 下的基础契约断言始终执行。
# ===========================================================================

# ---------------------------------------------------------------------------
# 工具：尝试切到 UTF-8 locale，返回是否成功
# ---------------------------------------------------------------------------
.enter_utf8 <- function() {
  for (loc in c("zh_CN.UTF-8", "Chinese_China.utf8",
                "en_US.UTF-8", "English_United States.utf8")) {
    ok <- tryCatch({
      suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
      isTRUE(l10n_info()[["UTF-8"]])
    }, error = function(e) FALSE)
    if (ok) return(TRUE)
  }
  FALSE
}

# ===========================================================================
# 1. tcmnp_build_info —— 返回结构契约
# ===========================================================================
test_that("tcmnp_build_info 返回命名 list 且含关键字段", {
  info <- tcmnp_build_info()
  expect_type(info, "list")
  expect_true(length(info) > 0)
  # 关键字段必须存在（值可能为 NA —— 开发期未打包时是预期的）
  # 注：字段清单以实际返回为准（实测 10 个），勿凭记忆增删
  expect_true(all(c("build_id", "name", "version", "git_commit") %in% names(info)),
              info = paste("实际字段:", paste(names(info), collapse = ", ")))
  expect_equal(info$name, "tcmnetpharma")
})

test_that("tcmnp_build_info(field) 单字段取值与完整列表一致", {
  info <- tcmnp_build_info()
  one <- tcmnp_build_info("build_id")
  expect_length(one, 1)
  expect_identical(one, info$build_id)
})

test_that("tcmnp_build_info 对未知字段返回 NA 而非报错", {
  expect_no_error(v <- tcmnp_build_info("__no_such_field__"))
  expect_true(is.na(v))
})

# ===========================================================================
# 2. tcmnp_identity —— 打印摘要，不应报错
# ===========================================================================
test_that("tcmnp_identity 可无错执行", {
  expect_no_error(tcmnp_identity())
})

test_that("tcmnp_identity 返回可打印的内容（或 NULL 副作用打印）", {
  out <- utils::capture.output(res <- tcmnp_identity())
  # 契约：要么有返回值，要么有打印输出 —— 二者至少有其一
  expect_true(length(out) > 0 || !is.null(res))
})

# ===========================================================================
# 3. ★ E8 回归守卫：UTF-8 locale 下函数体必须可反序列化
#
#    这是本文件的核心价值。E8 的症状是：包在 UTF-8 locale 下 library()
#    之后，含裸中文字符串常量的函数一读函数体就崩（embedded nul in string）。
#    因此这里显式在 UTF-8 下 deparse 两个函数体 —— 只要源码里再混入裸
#    多字节字符串常量，本测试立刻失败。
# ===========================================================================
test_that("★E8 回归：UTF-8 locale 下两个身份函数体可 deparse（无 embedded nul）", {
  entered <- .enter_utf8()
  skip_if_not(entered, "本环境无法设置 UTF-8 locale，跳过 E8 locale 回归守卫")

  for (fn in c("tcmnp_build_info", "tcmnp_identity")) {
    f <- get(fn, envir = asNamespace("tcmnetpharma"))
    # 注：expect_no_error() 不接受 info= 参数，故用 tryCatch 自行捕获再断言
    err <- tryCatch({ deparse(f); NULL }, error = function(e) conditionMessage(e))
    expect_null(
      err,
      info = paste0(fn, " 的函数体在 UTF-8 locale 下不可解码（", err, "）—— ",
                    "疑似源码混入裸中文字符串常量（E8 复发）。",
                    "修复：python data-raw/tools/escape_cn_strings.py --apply ",
                    "--dir TCMNP-main/R")
    )
  }
})

test_that("★E8 回归：UTF-8 locale 下两个身份函数可实际调用", {
  entered <- .enter_utf8()
  skip_if_not(entered, "本环境无法设置 UTF-8 locale，跳过 E8 locale 回归守卫")

  err1 <- tryCatch({ tcmnp_build_info(); NULL }, error = function(e) conditionMessage(e))
  expect_null(err1, info = paste0("tcmnp_build_info() 在 UTF-8 locale 下报错：", err1))

  err2 <- tryCatch({ tcmnp_identity(); NULL }, error = function(e) conditionMessage(e))
  expect_null(err2, info = paste0("tcmnp_identity() 在 UTF-8 locale 下报错：", err2))
})

# ===========================================================================
# 4. ★E8 回归：内部文案助手必须返回合法 UTF-8、且非空
#    .tcmnp_note() 是 E8 修复时抽出的集中管理点，用 \uXXXX 转义书写。
# ===========================================================================
test_that("★E8 回归：内部 .tcmnp_note 返回合法 UTF-8 文本", {
  ns <- asNamespace("tcmnetpharma")
  note_fn <- tryCatch(get(".tcmnp_note", envir = ns), error = function(e) NULL)
  skip_if(is.null(note_fn), ".tcmnp_note 未导出到命名空间")

  for (k in c("PARSE_FAILED", "FALLBACK_USED", "UNPACKAGED")) {
    v <- note_fn(k)
    expect_type(v, "character")
    expect_length(v, 1)
    expect_true(nzchar(v), info = paste0("key=", k, " 返回空串"))
    # 必须是合法 UTF-8（未因 Encoding 误标而在转码中损坏）
    expect_true(
      validUTF8(v),
      info = paste0("key=", k, " 返回值含非法字节 —— 疑似 E8 编码缺陷复发")
    )
  }
})

test_that("★E8 回归：未知 key 返回空串而非报错", {
  ns <- asNamespace("tcmnetpharma")
  note_fn <- tryCatch(get(".tcmnp_note", envir = ns), error = function(e) NULL)
  skip_if(is.null(note_fn), ".tcmnp_note 未导出到命名空间")
  expect_identical(note_fn("__no_such_key__"), "")
})
