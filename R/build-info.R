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
                     note = "BUILD-INFO.json 存在但解析失败")
      }
      .tcmnp_build_info_cache$data <- info
    } else {
      .tcmnp_build_info_cache$data <- list(
        build_id = "unpackaged",
        note = paste(
          "inst/BUILD-INFO.json 不存在：当前为开发期加载，",
          "非流水线打包产物。"
        )
      )
    }
  }
  info <- .tcmnp_build_info_cache$data
  if (is.null(field)) {
    return(info)
  }
  if (is.null(info[[field]])) NA_character_ else info[[field]]
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
  out$note <- "由 fallback 解析器读取（jsonlite 不可用）"
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
