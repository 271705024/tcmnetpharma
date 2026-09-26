# ===========================================================================
# 构建身份信息（BUILD IDENTITY） — 2026-09-15
# ---------------------------------------------------------------------------
# 背景
#   本项目多次被「旧包干扰」坑到：验的是旧副本、目标库有残留、同机多份
#   TCMNP 互相串味。仅看路径或时间戳不足以判别新鲜度 —— 必须能**从代码里
#   问出**「我现在加载的这一份到底是哪个构建」。
#
# 设计
#   身份信息由构建流水线在打包时**烧进包内**（inst/BUILD-INFO.json），
#   运行期用 tcmnp_build_info() 读回。若该文件不存在（开发期未打包），
#   则回退到「未注入」占位，绝不报错 —— 身份查询本身不应成为故障源。
#
# 用法
#   tcmnp_build_info()          # 返回命名 list
#   tcmnp_build_info("build_id")# 只取一个字段
#   tcmnp_identity()            # 打印单行摘要，适合日志开头
#
# ===========================================================================
# 【编码铁律】2026-09-22 血泪教训 —— 包 R/ 代码中**禁止出现裸中文字符串常量**
# ---------------------------------------------------------------------------
# 症状：包在 UTF-8 locale 下 library() 后，本文件里的函数一调用就报
#         Error: embedded nul in string: 'inst/BUILD-INFO.json d8\re-...'
#       连 body()/deparse() 都会炸；但同一函数在 C locale 下完全正常。
#
# 定位（详见 tests/xuanfuhua-2026-09-22-p43/ 探针 1-38）：
#   1. 与 DuckDB / system.file / 解析逻辑**无关**；
#   2. 只取决于「library() 时的 LC_CTYPE 是否 UTF-8」：
#        C            -> 一切正常
#        zh_CN.UTF-8  -> 函数体读取即报 embedded nul
#   3. 二分到**本文件里的中文字符串常量**：把这些常量换成 ASCII 后
#      （注释中文保留）问题立刻消失 —— 注释无害，**字符串常量才是元凶**；
#   4. 手写的最小包（4 个文件）可 100% 复现，与 TCMNP 其余代码无关。
#
# 根因：R 4.6.1 (ucrt) 在 UTF-8 locale 下把源文件里的裸多字节字符串常量的
#   Encoding 标记错标为 "ASCII"（安装期警告原文：
#     input string '...' cannot be translated from 'ASCII' to UTF-8,
#     but is valid UTF-8
#   ），该错误标记随字节码写进 R/TCMNP.rdb；运行时按 UTF-8 locale 反序列化
#   这条记录就崩。**安装期写的 rdb 有损，运行时 locale 一变就暴露**。
#
# 修法（本文件采用）：
#   * 包代码里的**每个面向用户的字符串常量**一律写成 \uXXXX 转义 ——
#     转义序列是纯 ASCII，与 locale 完全无关，Encoding 标记永远正确。
#   * 中文**注释**可以保留（已实测无害），但为稳妥起见新代码建议注释也少用。
#   * 生成中文的人手转义太痛苦？用 tools/escape_cn_strings.R 自动转换。
#
# 复现/回归脚本：tests/xuanfuhua-2026-09-22-p43/_probe31.R（最小包复现）
# ===========================================================================

.tcmnp_build_info_cache <- new.env(parent = emptyenv())

#' 读取 TCMNP 构建身份信息
#'
#' 返回当前已加载的 TCMNP 包的构建身份信息（build_id / 版本 / 数据版本 /
#' git commit / 源码指纹）。信息由构建流水线在打包时写入
#' \code{inst/BUILD-INFO.json}；开发期未打包时返回占位值而非报错。
#'
#' @param field 可选。只取某一个字段（如 \code{"build_id"}）。
#'              缺省则返回完整命名 list。
#' @return 命名 list（或单个字段值）。若 \code{field} 不存在则返回 \code{NA}。
#' @examples
#' \dontrun{
#' tcmnp_build_info("build_id")
#' }
#' @export
tcmnp_build_info <- function(field = NULL) {
  if (is.null(.tcmnp_build_info_cache$data)) {
    f <- system.file("BUILD-INFO.json", package = "TCMNP")
    if (nzchar(f) && file.exists(f)) {
      # 刻意**不**依赖 jsonlite：本函数纯信息性，不应为一个「读一行身份」
      # 的功能给包增加硬依赖。内部 JSON 由我们自己的生成器写出、结构固定，
      # 自带的解析器足够且可控。
      #
      # 坑（2026-09-15 踩过）：早期版本写的是 `jsonlite::fromJSON(...)`，
      # 但 jsonlite 既不在 Imports 也不在 NAMESPACE。包内命名空间用 `::`
      # 引用未声明的包，会让 **lazy loading 整体失败**，而 R 只回一句
      # 无信息量的 `ERROR: lazy loading failed for package 'TCMNP'` ——
      # 极难定位（安装日志里连行号都不给）。
      info <- .tcmnp_parse_build_info_fallback(f)
      if (is.null(info) || length(info) == 0L) {
        info <- list(build_id = "unknown",
                     note = .tcmnp_note("PARSE_FAILED"))
      }
      .tcmnp_build_info_cache$data <- info
    } else {
      .tcmnp_build_info_cache$data <- list(
        build_id = "unpackaged",
        note = .tcmnp_note("UNPACKAGED")
      )
    }
  }
  info <- .tcmnp_build_info_cache$data
  if (is.null(field)) {
    return(info)
  }
  if (is.null(info[[field]])) NA_character_ else info[[field]]
}

#' 内部：本地化的提示文案（\uXXXX 转义，保证 locale 无关）
#'
#' ## ★ E8 修复（2026-09-22，commit c7bcb2c）：裸中文字符串常量 → \uXXXX 转义
#' 根因 —— 本文件原以裸中文字符串常量书写这几条提示文案。R 4.6.1 (ucrt) 在
#'   安装期把其 `Encoding` 标记错标为 "ASCII"，随字节码写进 `R/TCMNP.rdb`；
#'   运行期在 UTF-8 locale 下反序列化该记录即抛 `embedded nul in string`。
#'   **是「安装期写坏、运行期按 locale 才暴露」的延迟缺陷** —— C locale 下完全正常。
#' 方案 —— 全部文案改 `\uXXXX` 转义（纯 ASCII，Encoding 标记恒正确），
#'   并抽出本助手集中管理；文案**字节内容与转义前逐字节一致**（87/37/52）。
#' 回归锚点 —— `logs/errors-utf8-nul-encoding-2026-09-22.md`；
#'   闸门：`build_app.py::_preflight_source_encoding()`（静态预扫）+
#'   `stage_verify()` locale 矩阵（C + zh_CN.UTF-8）；复现：`_probe31.R`。
#' 工具 —— `python data-raw/tools/escape_cn_strings.py --apply --dir TCMNP-main/R`
#'
#' @keywords internal
#' @noRd
.tcmnp_note <- function(key) {
  switch(key,
    PARSE_FAILED  = "\u0042\u0055\u0049\u004c\u0044\u002d\u0049\u004e\u0046\u004f\u002e\u006a\u0073\u006f\u006e\u0020\u5b58\u5728\u4f46\u89e3\u6790\u5931\u8d25",
    FALLBACK_USED = paste0(
      "\u7531\u0020\u0066\u0061\u006c\u006c\u0062\u0061\u0063\u006b\u0020",
      "\u89e3\u6790\u5668\u8bfb\u53d6\uff08\u006a\u0073\u006f\u006e\u006c\u0069\u0074\u0065\u0020",
      "\u4e0d\u53ef\u7528\uff09"
    ),
    UNPACKAGED    = .tcmnp_note_unpackaged(),
    ""
  )
}

#' 内部：「开发期未打包」提示文案
#' @keywords internal
#' @noRd
.tcmnp_note_unpackaged <- function() {
  paste0(
    "\u0069\u006e\u0073\u0074\u002f\u0042\u0055\u0049\u004c\u0044\u002d\u0049\u004e\u0046\u004f\u002e\u006a\u0073\u006f\u006e\u0020",
    "\u4e0d\u5b58\u5728\uff1a\u5f53\u524d\u4e3a\u5f00\u53d1\u671f\u52a0\u8f7d\uff0c",
    "\u975e\u6d41\u6c34\u7ebf\u6253\u5305\u4ea7\u7269\u3002"
  )
}

#' @keywords internal
#' @noRd
.tcmnp_parse_build_info_fallback <- function(path) {
  txt <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
                  error = function(e) "")
  # 坑一（2026-09-15 实测）：生成器在 Windows 上写出 CRLF，
  # R 读进来后每行尾部残留 \r，行首锚定的正则全部失配 → 「字段全 NA」的静默失败。
  txt <- gsub("\r\n", "\n", txt, fixed = TRUE)
  txt <- gsub("\r", "\n", txt, fixed = TRUE)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]

  # 坑二（2026-09-15 实测）：**不要用 `(?m)^...` 内联修饰符** ——
  # R 的 regexpr(perl = TRUE) 在本机对这种写法**静默返回 0 个匹配**
  # （实测：带 (?m) 匹配数 0；去掉后正常匹配 1）。
  # 这类「不报错、只是匹配不到」的失败最难查，故这里改成**逐行处理**。
  #
  # 坑三（同日实测）：`json.dumps(indent=2)` 写出的 JSON **所有键都带缩进**——
  # 顶层键缩进 **恰好 2 空格**，嵌套键（package{} 内）缩进 >= 4。
  # 一开始误以为「顶层键顶格」，据此写的 `^\\s` 跳过规则把**每一行都跳过了**，
  # 结果仍然是「字段全 NA」。故判据改为**按缩进层数**：只接受恰好 2 空格缩进的行。
  #
  # 坑四（2026-09-22）：**注意 `\\r` `\\n` 必须用 fixed = TRUE 传字面量**，
  # 否则在 locale 变化时转义序列可能被二次解释，导致整段文本被压成一行。
  indent_of <- function(ln) {
    m <- regmatches(ln, regexpr("^ *", ln))
    nchar(m)
  }
  pick <- function(key) {
    needle <- paste0('"', key, '"')
    for (ln in lines) {
      if (indent_of(ln) != 2L) next          # 只认顶层（恰好 2 空格）
      if (!startsWith(trimws(ln), needle)) next
      m <- regmatches(ln, regexpr(':\\s*"([^"]*)"', ln))
      if (length(m) == 1L && nzchar(m)) {
        return(sub('^:\\s*"([^"]*)"$', "\\1", m))
      }
    }
    NA_character_
  }
  pick_lgl <- function(key) {
    needle <- paste0('"', key, '"')
    for (ln in lines) {
      if (indent_of(ln) != 2L) next
      if (!startsWith(trimws(ln), needle)) next
      return(grepl("true", ln, fixed = TRUE))
    }
    FALSE
  }
  keys <- c("build_id", "generated_at", "stage", "name", "version",
            "data_version", "source_md5", "git_commit")
  out <- lapply(keys, pick)
  names(out) <- keys
  out$git_dirty <- pick_lgl("git_dirty")
  out$note <- .tcmnp_note("FALLBACK_USED")
  out
}

#' @keywords internal
#' @noRd
.tcmnp_or <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a

#' 打印 TCMNP 构建身份单行摘要
#'
#' 适合放在日志/会话开头，快速确认「我加载的是哪一份」。
#'
#' @return 不可见地返回该摘要字符串。
#' @examples
#' \dontrun{
#' tcmnp_identity()
#' }
#' @export
tcmnp_identity <- function() {
  i <- tcmnp_build_info()
  line <- sprintf(
    "TCMNP %s | build %s | data %s | git %s%s",
    .tcmnp_or(i$version, "?"),
    .tcmnp_or(i$build_id, "?"),
    .tcmnp_or(i$data_version, "?"),
    .tcmnp_or(i$git_commit, "?"),
    if (isTRUE(i$git_dirty)) " (DIRTY)" else ""
  )
  message(line)
  invisible(line)
}
