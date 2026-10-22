# ---------------------------------------------------------------------------
# 配色体系（2026-10-22）
#
# 由来：boss 反馈「配色问题需要考虑，从高分文献或配色网站拔几套好用的配色
# 体系放设置选项」。本文件是**配色的单一事实源**，每个色值都标了出处。
#
# 三条硬约束：
#   1. **色值必须有出处**，不凭记忆造色：
#        · 期刊风  = Nature(NPG) / Science(AAAS) / Lancet / JAMA / NEJM / JCO
#                    的期刊官方色板，经 ggsci 收录（github.com/nanxstats/ggsci,
#                    R/palettes.R 的 pal_npg / pal_aaas / pal_lancet / pal_jama /
#                    pal_nejm / pal_jco）
#        · 色盲友好 = Okabe & Ito (2008) Color Universal Design；
#                    普及文献 Wong B. (2011) Nature Methods 8:441
#                    doi:10.1038/nmeth.1618
#        · 连续型   = ColorBrewer 2.0 (colorbrewer2.org)；viridis (viridisLite)
#                    柔和分类 Set2 / Tableau 10 亦标注来源
#   2. **一套配色要同时服务三类用途** —— 少分类（3 类：药/成分/靶点或 BP/CC/MF）、
#      多分类（通路/模块/网络节点）、连续型（p 值/度数渐变）。否则用户换一张图
#      就得重选一次，反而更麻烦。
#   3. **只改颜色，不改数据**（与 style 同口径；tests 有硬断言）。
#
# 用法：
#   tcm_palette("npg", 3)                    # 3 个分类色
#   tcm_palette("viridis", 8)                # 8 个连续色阶
#   tcm_palette("okabe")                     # 该色板全部颜色
# ---------------------------------------------------------------------------

# 色板注册表。
#   kind:  qual = 分类（定性）色板；seq / div = 连续/发散色板
#   colors: 色值序列。**连续型色板的顺序 = 数值从小到大**（渐变映射靠它）
#   n_max: 该色板"原生"颜色数（超过就插值补足，见 .tcm_palette_n）
.tcm_palette_db <- list(
  # ---------------- 期刊 / 机构官方色板（ggsci 收录）----------------
  npg = list(
    label = "NPG \u81EA\u7136\u98CE", short = "NPG",
    source = "Nature \u7CFB\u5217\u671F\u520A\u8272\u677F\uFF08ggsci: pal_npg\uFF09",
    kind = "qual",
    colors = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F",
               "#8491B4", "#91D1C2", "#DC0000", "#7E6148", "#B09C85")
  ),
  aaas = list(
    label = "Science \u98CE", short = "AAAS",
    source = "Science/AAAS \u671F\u520A\u8272\u677F\uFF08ggsci: pal_aaas\uFF09",
    kind = "qual",
    colors = c("#3B4992", "#EE0000", "#008B45", "#631879", "#008280",
               "#BB0021", "#5F559B", "#A20056", "#808180", "#1B1919")
  ),
  lancet = list(
    label = "Lancet \u98CE", short = "Lancet",
    source = "The Lancet Oncology \u8272\u677F\uFF08ggsci: pal_lancet\uFF09",
    kind = "qual",
    colors = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F",
               "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919")
  ),
  jama = list(
    label = "JAMA \u98CE", short = "JAMA",
    source = "JAMA \u8272\u677F\uFF08ggsci: pal_jama\uFF09",
    kind = "qual",
    colors = c("#374E55", "#DF8F44", "#00A1D5", "#B24745", "#79AF97",
               "#6A6599", "#80796B")
  ),
  nejm = list(
    label = "NEJM \u98CE", short = "NEJM",
    source = "New England Journal of Medicine \u8272\u677F\uFF08ggsci: pal_nejm\uFF09",
    kind = "qual",
    colors = c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1",
               "#6F99AD", "#FFDC91", "#EE4C97")
  ),
  jco = list(
    label = "JCO \u98CE", short = "JCO",
    source = "Journal of Clinical Oncology \u8272\u677F\uFF08ggsci: pal_jco\uFF09",
    kind = "qual",
    colors = c("#0073C2", "#EFC000", "#868686", "#CD534C", "#7AA6DC",
               "#003C67", "#8F7700", "#3B3B3B", "#A73030", "#4A6990")
  ),

  # ---------------- 色盲友好（三色觉缺陷下仍可区分）----------------
  okabe = list(
    label = "\u8272\u76F2\u53CB\u597D Okabe-Ito", short = "Okabe",
    source = "Okabe & Ito 2008 Color Universal Design\uFF1BWong 2011 Nat Methods 8:441",
    kind = "qual",
    # 顺序按原作者给的「首次相遇对比度」排序，不要随意打乱
    colors = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")
  ),

  # ---------------- 通用分类色板 ----------------
  set2 = list(
    label = "\u67D4\u548C\u5206\u7C7B Set2", short = "Set2",
    source = "ColorBrewer 2.0\uFF08colorbrewer2.org\uFF09Set2",
    kind = "qual",
    colors = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
               "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
  ),
  tableau = list(
    label = "Tableau 10", short = "Tableau",
    source = "Tableau \u9ED8\u8BA4\u8272\u677F\uFF08matplotlib 2.0+ \u4EA6\u4EE5\u6B64\u4E3A\u9ED8\u8BA4\uFF09",
    kind = "qual",
    colors = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
               "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")
  ),

  # ---------------- 连续 / 发散（p 值、度数渐变）----------------
  # 顺序 = 数值从小到大：p.adjust 小（更显著）落在序列前端
  viridis = list(
    label = "viridis \u8FDE\u7EED\uFF08\u8272\u76F2\u53CB\u597D\uFF09", short = "viridis",
    source = "viridis\uFF08viridisLite::viridis\uFF09\uFF1B\u9ED8\u8BA4\u8272\u76F2\u53CB\u597D\u4E14\u53EF\u7070\u5EA6\u8FD8\u539F",
    kind = "seq",
    # viridisLite 返回 8 位（#RRGGBBFF，alpha 恒为 FF），裁成 6 位与其他色板
    # 统一 —— 否则下游插值/比对会因 alpha 通道差异出现假不等。
    colors = substr(viridisLite::viridis(8), 1L, 7L)
  ),
  rdbu = list(
    label = "\u7EA2\u84DD\u53D1\u6563 RdBu", short = "RdBu",
    source = "ColorBrewer 2.0 RdBu\uFF1A\u4F4E\u503C\u504F\u7EA2\uFF08\u663E\u8457\uFF09\u3001\u9AD8\u503C\u504F\u84DD",
    kind = "div",
    colors = RColorBrewer::brewer.pal(9, "RdBu")
  ),
  spectral = list(
    label = "\u5F69\u8679 Spectral", short = "Spectral",
    source = "ColorBrewer 2.0 Spectral\uFF1A\u4F4E\u503C\u504F\u7EA2\u3001\u9AD8\u503C\u504F\u84DD",
    kind = "div",
    colors = RColorBrewer::brewer.pal(11, "Spectral")
  ),
  ylgnbu = list(
    label = "\u84DD\u7EFF\u6E10\u53D8 YlGnBu", short = "YlGnBu",
    source = "ColorBrewer 2.0 YlGnBu\uFF1A\u4F4E\u503C\u6D45\u3001\u9AD8\u503C\u6DF1\u84DD",
    kind = "seq",
    colors = RColorBrewer::brewer.pal(9, "YlGnBu")
  )
)


#' Available colour palettes
#'
#' Lists the colour schemes shipped with the package. Every scheme records its
#' provenance (journal brand palette, colour-blind-safe set, or ColorBrewer /
#' viridis). They are used by the `palette` argument of the plotting functions
#' and by the GUI style panel.
#'
#' @param names_only return only the palette keys (default `TRUE`);
#'   `FALSE` returns a data.frame with label / kind / source / n.
#'
#' @return character vector of palette keys, or a data.frame describing them.
#' @export
#'
#' @examples
#' tcm_palette_names()
#' tcm_palette_names(names_only = FALSE)
tcm_palette_names <- function(names_only = TRUE) {
  if (isTRUE(names_only)) return(names(.tcm_palette_db))
  data.frame(
    name = names(.tcm_palette_db),
    label = vapply(.tcm_palette_db, function(x) x$label, ""),
    kind = vapply(.tcm_palette_db, function(x) x$kind, ""),
    n = vapply(.tcm_palette_db, function(x) length(x$colors), 0L),
    source = vapply(.tcm_palette_db, function(x) x$source, ""),
    stringsAsFactors = FALSE, row.names = NULL
  )
}


#' Colours from a built-in palette
#'
#' @param name palette key, see [tcm_palette_names()].
#' @param n number of colours. `NULL` (default) returns the palette as shipped.
#'   When `n` exceeds the shipped length, colours are interpolated
#'   (a message reports it — silently recycling colours would let two
#'   categories collide, which is worse than a smooth transition).
#' @param type `"discrete"` (default) or `"continuous"`. Both interpolate;
#'   the difference is only in the message and in how callers use the result.
#'
#' @return character vector of hex colours.
#' @export
#'
#' @importFrom viridisLite viridis
#'
#' @examples
#' tcm_palette("npg", 3)
#' tcm_palette("okabe")
#' tcm_palette("viridis", 8, type = "continuous")
tcm_palette <- function(name = "npg", n = NULL,
                        type = c("discrete", "continuous")) {
  type <- match.arg(type)
  if (is.null(name)) return(NULL)
  name <- as.character(unlist(name, use.names = FALSE))[[1]]
  if (!name %in% names(.tcm_palette_db)) {
    stop(sprintf("unknown palette: \"%s\" (available: %s)",
                 name, paste(names(.tcm_palette_db), collapse = ", ")),
         call. = FALSE)
  }
  cols <- .tcm_palette_db[[name]]$colors
  if (is.null(n)) return(cols)
  n <- suppressWarnings(as.integer(unlist(n, use.names = FALSE))[[1]])
  if (!is.finite(n) || n < 1L) stop("n must be a positive integer", call. = FALSE)
  .tcm_palette_n(cols, n, name, type)
}


# 取 n 个颜色：够就切片，不够就插值补足并告知（不静默复用同色）
.tcm_palette_n <- function(cols, n, name = "", type = "discrete") {
  n <- as.integer(n)[[1]]
  if (n <= length(cols)) return(cols[seq_len(n)])
  message(sprintf(
    "palette \"%s\": \u539f\u751f %d \u8272\uff0c\u8bf7\u6c42 %d \u8272\uff0c\u5df2\u63d2\u503c\u8865\u8db3\u3002",
    name, length(cols), n))
  grDevices::colorRampPalette(cols)(n)
}


# 连续型色阶（渐变用）：任意色板都插值出 n 级
.tcm_palette_ramp <- function(name, n = 8L) {
  if (is.null(name)) return(NULL)
  cols <- tcm_palette(name)
  grDevices::colorRampPalette(cols)(as.integer(n)[[1]])
}


# 校验 palette 取值（供各绘图函数入口与 GUI 后端防御共用）：
# NULL / 空 -> NULL（走各函数原有配色，向后兼容）
.tcm_palette_check <- function(palette) {
  if (is.null(palette)) return(NULL)
  p <- as.character(unlist(palette, use.names = FALSE))
  if (!length(p) || !nzchar(p[[1]])) return(NULL)
  p <- p[[1]]
  if (!p %in% names(.tcm_palette_db)) {
    stop(sprintf("unknown palette: \"%s\" (available: %s)",
                 p, paste(names(.tcm_palette_db), collapse = ", ")),
         call. = FALSE)
  }
  p
}
