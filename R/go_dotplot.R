#' Dot plot showed the results of enrichment analysis
#'
#' @description
#' 2026-10-24 重构：不再依赖 \code{.tcm_inject_group_key()} 把 \code{source}
#' 回填进 \code{ONTOLOGY}，改用 \code{\link{tcm_enrich_table}()} 的
#' \code{source} / \code{ontology} 双列模型；\code{title} 默认改为从数据派生
#' （旧版是字面量 \code{"GO enrichment of cluster"}，凡非 GO 数据都身份错标）。
#'
#' @param go.diff clusterProfiler 的 enrichGO 结果（S4）、\code{tcm_enrich()} 的
#'   返回，或 data.frame。
#' @param top **每组**保留的条目数（按 \code{Count} 降序取）。
#' @param color 连续色阶名，见 \code{RColorBrewer::display.brewer.all()}。
#' @param text.size 字号。
#' @param text.width 条目名换行宽度。
#' @param title 图标题。\code{NULL}（默认）时从数据派生。
#' @param legacy_title 设为 \code{TRUE} 时标题强制为旧版字面量
#'   \code{"GO enrichment of cluster"}。默认 \code{FALSE}。
#' @param style 形制/外观预设：\code{"default"}（x = Rich Factor）、
#'   \code{"genratio"}（x = \code{GeneRatio}，clusterProfiler 口径）、
#'   \code{"journal"} / \code{"presentation"}（主题）。
#'   \strong{只改视觉形制，选中的条目与统计量完全一致}。
#' @param palette 配色方案键，见 \code{\link{tcm_palette_names}()}。
#' @param ... 未识别的参数会触发警告（不静默忽略）。
#'
#' @return ggplot 对象
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 facet_grid
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom tidyr drop_na
#' @importFrom stringr str_wrap
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \dontrun{
#' data(xfbdf, package = "tcmnetpharma")
#' res <- tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human",
#'                   pvalueCutoff = 0.01, qvalueCutoff = 0.05)
#' go_dotplot(res$go)
#' go_dotplot(res$go, top = 10, style = "genratio")   # 文献标准口径
#' go_dotplot(res$reactome, style = "genratio")       # 标题自动写 Reactome
#' }
go_dotplot <- function(go.diff,
                       top = 5,
                       color = "RdBu",
                       text.size = 12,
                       text.width = 35,
                       title = NULL,
                       legacy_title = FALSE,
                       style = c("default", "genratio", "journal", "presentation"),
                       palette = NULL,
                       ...) {
  style <- match.arg(style)
  palette <- .tcm_palette_check(palette)

  dots <- list(...)
  if (length(dots)) {
    warning("go_dotplot(): ignoring unrecognised argument(s): ",
            paste(names(dots), collapse = ", "), call. = FALSE)
  }

  # 连续型配色：点色映射的是 p.adjust，故取色阶（不是分类色）
  grad_cols <- if (is.null(palette)) {
    RColorBrewer::brewer.pal(8, color)
  } else {
    .tcm_palette_ramp(palette, 8L)
  }

  # ---- 1. 标准化：来源与本体分列，绝不互相回填 ----------------------------
  tb <- tcm_enrich_table(go.diff)
  if (!is.data.frame(tb) || !nrow(tb))
    stop("No enrichment rows found in `go.diff`.", call. = FALSE)
  if (!"Count" %in% names(tb))
    stop("`go.diff` is missing required column: Count", call. = FALSE)

  # ---- 2. 分组（来源 / 本体，由数据决定）----------------------------------
  grp <- .tcm_resolve_group(tb)
  tb$.grp <- as.character(grp$key)

  # Rich Factor = 命中数 / 该条目注释基因总数
  bg <- suppressWarnings(as.numeric(sub("/.*$", "", as.character(tb$BgRatio))))
  bg[!is.finite(bg) | bg <= 0] <- NA_real_
  tb$richFactor <- suppressWarnings(as.numeric(tb$Count)) / bg
  tb$richFactor[!is.finite(tb$richFactor)] <- 0

  # ---- 3. 分组取 top（strict 硬截）+ 组内排序 ----------------------------
  d <- .tcm_pick_top(tb, n = top, by = "Count", group = ".grp",
                     decreasing = TRUE, strict = TRUE)
  if (!nrow(d)) stop("No terms left after grouping/slicing.", call. = FALSE)

  # ---- 4. 标题：身份信息绝不来自字面量默认值 ------------------------------
  ttl <- if (isTRUE(legacy_title)) {
    "GO enrichment of cluster"
  } else if (!is.null(title) && length(title) && nzchar(as.character(title)[[1L]])) {
    as.character(title)[[1L]]
  } else {
    .tcm_enrich_title(d)
  }

  # ---- 5. x 轴口径 --------------------------------------------------------
  vs <- if (style %in% c("journal", "presentation")) style else "default"

  if (identical(style, "genratio")) {
    d$x_value <- .dot_generatio(d)
    xl <- "GeneRatio"
    if (all(!is.finite(d$x_value))) {
      # 不做「部分行静默丢掉」：气泡图少几个点比 x 轴口径说错更危险
      message("go_dotplot(style = \"genratio\"): \u6570\u636e\u65e0\u53ef\u7528 GeneRatio\uff0c\u5df2\u9000\u56de Rich Factor\u3002")
      d$x_value <- d$richFactor
      xl <- "Rich Factor"
    }
  } else {
    d$x_value <- d$richFactor
    xl <- "Rich Factor"
  }

  # 组内按 x 升序 -> prep_y(desc = FALSE) 让最大者在顶部
  d <- d[order(d$.grp, d$x_value), , drop = FALSE]
  py <- .enrich_bar_prep_y(d, text.width, desc = FALSE)
  d <- py$d
  ylab_fun <- .enrich_bar_ylab_fun(py$labs)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x_value, y = .data$.ykey)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$p.adjust, size = .data$Count)) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$.grp),
      scales = "free_y", space = "free_y"
    ) +
    ggplot2::scale_color_gradientn(
      colours = grad_cols,
      trans = "log10",
      guide = ggplot2::guide_colorbar(reverse = TRUE, order = 1)
    ) +
    ggplot2::scale_size_continuous(range = c(2, 8)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.10))) +
    # drop 由 .enrich_bar_prep_y() 决定（单轴形制为 FALSE，保住 placeholder 槽位）
    ggplot2::scale_y_discrete(breaks = py$breaks, labels = ylab_fun,
                                drop = py$drop) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = text.size, colour = "black"),
      axis.title.x = ggplot2::element_text(size = text.size),
      strip.text = ggplot2::element_text(face = "bold", size = text.size),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = xl, y = NULL, title = ttl)

  .tcm_style_apply(p, vs)
}

#' GeneRatio 数值化（k/n -> k/n）
#'
#' 缺失或全不可解析时返回全 NA，由调用方决定退回口径（不静默丢行）。
#' @noRd
.dot_generatio <- function(d) {
  out <- rep(NA_real_, nrow(d))
  if (!"GeneRatio" %in% names(d)) return(out)
  gr <- as.character(d$GeneRatio)
  k <- suppressWarnings(as.numeric(sub("/.*$", "", gr)))
  n <- suppressWarnings(as.numeric(sub("^.*/", "", gr)))
  ok <- is.finite(k) & is.finite(n) & n > 0
  out[ok] <- k[ok] / n[ok]
  out
}
