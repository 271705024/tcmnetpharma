# ---------------------------------------------------------------------------
# 富集条目的分组与排序（2026-10-24，R1）
#
# 核心原则：**分组（group）与排序（order）是两个正交维度**，必须显式分开。
#
# 反例（2026-10-24 之前的 go_barplot "classic" 分支）：
#   factor(Description, levels = unique(Description[order(Count, ONTOLOGY)]))
# 这里 ONTOLOGY 只是**第二排序键**（仅当 Count 相等才起作用），所以 BP/CC/MF
# 是按 Count 大小交错出现的 —— 这正是 boss 截图里"三色块交替、无法按本体阅读"
# 的形态。
#
# 正确范式在包内早已存在（R/degree_plot.R:86-89）：
#   group_by(type) %>% slice_max(degree, n = top) %>% arrange(type, desc(degree))
# 本文件把它一般化成可复用的 .tcm_pick_top()。
# ---------------------------------------------------------------------------

#' 分组取 top + 组内排序
#'
#' @param df data.frame
#' @param n 每组保留条数
#' @param by 排序依据列（默认 \code{p.adjust}）
#' @param group 分组列名向量；\code{NULL} 表示不分组
#' @param decreasing \code{by} 是否降序。默认 \code{FALSE}（p 值越小越靠前）；
#'   \code{by = "Count"} 时应传 \code{TRUE}（基因数越多越靠前）。
#' @param strict \code{TRUE}（默认）按**位置**硬截前 n 条；\code{FALSE} 为宽松模式
#'   （保留与第 n 名并列的全部条目）。
#'   \strong{为什么默认 strict}：全并列数据下宽松过滤会完全失效 —— 桑基图那次
#'   实测已证明（\code{max_nodes = 60} 与 \code{Inf} 的节点数都是 300）。
#'   富集结果里 p.adjust 常出现大量相同的极小值（如 1e-300），宽松模式会
#'   保留远超 n 条。
#' @return 重排后的 df，附加两列：\code{.grp}（分组键）、\code{.rank}（组内名次）。
#'   组间顺序按分组键的字符序（GO 本体正好得到 BP -> CC -> MF）。
#' @noRd
.tcm_pick_top <- function(df, n, by = "p.adjust", group = NULL,
                          decreasing = FALSE, strict = TRUE) {
  if (!is.data.frame(df) || !nrow(df)) {
    if (is.data.frame(df)) {
      df$.grp <- character(0)
      df$.rank <- integer(0)
    }
    return(df)
  }

  if (is.null(by) || !length(by) || !by[[1L]] %in% names(df)) by <- "p.adjust"
  by <- by[[1L]]

  n <- suppressWarnings(as.integer(n)[1L])
  if (is.na(n) || n < 1L) n <- nrow(df)

  # ---- 分组键 --------------------------------------------------------------
  gcols <- if (is.null(group)) character(0) else
    intersect(as.character(group), names(df))
  df$.grp <- if (length(gcols)) {
    do.call(paste, c(lapply(gcols, function(g) as.character(df[[g]])), sep = "\u0001"))
  } else {
    rep("", nrow(df))
  }
  # 分段分隔符是控制字符，展示时不该露出来；保留一个可读版本供调试
  df$.grp <- gsub("\u0001", " / ", df$.grp, fixed = TRUE)

  # ---- 组内排序（NA 一律排到最后）------------------------------------------
  key <- if (decreasing) -xtfrm(df[[by]]) else xtfrm(df[[by]])
  key[is.na(key)] <- Inf
  ord <- order(df$.grp, key, xtfrm(seq_len(nrow(df))))
  df <- df[ord, , drop = FALSE]

  # ---- 截断 ----------------------------------------------------------------
  df$.rank <- stats::ave(seq_len(nrow(df)), df$.grp, FUN = seq_along)
  if (isTRUE(strict)) {
    df <- df[df$.rank <= n, , drop = FALSE]
  } else if (any(df$.rank > n)) {
    # 宽松：取第 n 名的 by 值作阈值，>= 该值（升序时 <=）的全保留
    cut_vals <- tapply(df[[by]], df$.grp, function(v) {
      v <- v[!is.na(v)]
      if (length(v) < n) NA_real_ else sort(v)[[n]]
    })
    keep <- vapply(seq_len(nrow(df)), function(i) {
      g <- df$.grp[[i]]
      cv <- cut_vals[[g]]
      if (is.null(cv) || is.na(cv)) return(TRUE)
      v <- df[[by]][[i]]
      if (is.na(v)) return(FALSE)
      if (decreasing) v >= cv else v <= cv
    }, logical(1))
    df <- df[keep, , drop = FALSE]
  }

  rownames(df) <- NULL
  df
}

#' 解析 top 预设
#'
#' 对应 Enrichr「默认 top 10、要更多进 Table 视图选 10/25/50/100」的分层做法：
#' 给用户一组有语义的名字，而不是让他猜数字。
#'
#' @param top 数字，或 "compact"=5 / "standard"=10 / "full"=20
#' @return 单个正整数
#' @noRd
.tcm_top_n <- function(top) {
  if (is.null(top) || !length(top)) return(10L)
  if (is.numeric(top)) {
    n <- suppressWarnings(as.integer(top)[1L])
    return(if (is.na(n) || n < 1L) 10L else n)
  }
  key <- tolower(trimws(as.character(unlist(top, use.names = FALSE))[[1L]]))
  tab <- c(compact = 5L, standard = 10L, full = 20L)
  if (key %in% names(tab)) return(unname(tab[[key]]))
  n <- suppressWarnings(as.integer(key))
  if (is.na(n) || n < 1L) 10L else n
}
