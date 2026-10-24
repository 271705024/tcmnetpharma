#' Lollipop chart showed the results of enrichment analysis
#'
#' @description
#' 2026-10-24 重构。原实现有四个问题：
#' \enumerate{
#'   \item 直接 \code{group_by(ONTOLOGY)}，但只有 GO 结果有该列 —— 换成
#'         KEGG / Reactome / WikiPathways 数据即在 \code{group_by} 处报错；
#'   \item \code{title} 是字面量 \code{"GO enrichment of cluster"}，非 GO 数据身份错标；
#'   \item \code{tidyr::drop_na()} 会把 \code{ONTOLOGY} 为 \code{NA} 的非 GO 行整批删掉；
#'   \item \code{factor(Description, levels = go_enrich$Description)} 在跨来源撞名时
#'         直接报 "duplicated levels"。
#' }
#' 现改为 \code{\link{tcm_enrich_table}()} 的 \code{source} / \code{ontology}
#' 双列模型，形制与 \code{\link{go_barplot}()} 一致。
#'
#' @param go.diff clusterProfiler 的 enrichGO 结果（S4）、\code{tcm_enrich()} 的
#'   返回，或 data.frame。
#' @param top **每组**保留的条目数（按 \code{p.adjust} 升序取）。
#' @param color 连续色阶名，见 \code{RColorBrewer::display.brewer.all()}。
#' @param text.size 字号。
#' @param text.width 条目名换行宽度。
#' @param title 图标题。\code{NULL}（默认）时从数据派生。
#' @param legacy_title 设为 \code{TRUE} 时标题强制为旧版字面量
#'   \code{"GO enrichment of cluster"}。默认 \code{FALSE}。
#' @param palette 配色方案键，见 \code{\link{tcm_palette_names}()}。
#' @param ... 未识别的参数会触发警告（不静默忽略）。
#'
#' @return ggplot 对象
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 margin
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom stringr str_wrap
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \dontrun{
#' data(xfbdf, package = "tcmnetpharma")
#' res <- tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human",
#'                   pvalueCutoff = 0.01, qvalueCutoff = 0.05)
#' go_lollipop(res$go)
#' go_lollipop(res$reactome)   # 以前这里会崩（无 ONTOLOGY 列）
#' }
go_lollipop <- function(go.diff,
                        top = 5,
                        color = "RdBu",
                        text.size = 12,
                        text.width = 35,
                        title = NULL,
                        legacy_title = FALSE,
                        palette = NULL,
                        ...) {
  palette <- .tcm_palette_check(palette)
  dots <- list(...)
  if (length(dots)) {
    warning("go_lollipop(): ignoring unrecognised argument(s): ",
            paste(names(dots), collapse = ", "), call. = FALSE)
  }

  grad_cols <- if (is.null(palette)) {
    RColorBrewer::brewer.pal(8, color)
  } else {
    .tcm_palette_ramp(palette, 8L)
  }

  # ---- 1. 标准化（来源与本体分列）-----------------------------------------
  tb <- tcm_enrich_table(go.diff)
  if (!is.data.frame(tb) || !nrow(tb))
    stop("No enrichment rows found in `go.diff`.", call. = FALSE)
  if (!"Count" %in% names(tb))
    stop("`go.diff` is missing required column: Count", call. = FALSE)

  # ---- 2. 分组 ------------------------------------------------------------
  grp <- .tcm_resolve_group(tb)
  tb$.grp <- as.character(grp$key)

  bg <- suppressWarnings(as.numeric(sub("/.*$", "", as.character(tb$BgRatio))))
  bg[!is.finite(bg) | bg <= 0] <- NA_real_
  tb$richFactor <- suppressWarnings(as.numeric(tb$Count)) / bg
  tb$richFactor[!is.finite(tb$richFactor)] <- 0

  # ---- 3. 分组取 top + 组内排序（原实现 slice(1:top) 取的是原始行序前 n 条，
  #         与"最显著"无关，属实质缺陷）-------------------------------------
  d <- .tcm_pick_top(tb, n = top, by = "p.adjust", group = ".grp",
                     decreasing = FALSE, strict = TRUE)
  if (!nrow(d)) stop("No terms left after grouping/slicing.", call. = FALSE)

  # ---- 4. 标题派生 --------------------------------------------------------
  ttl <- if (isTRUE(legacy_title)) {
    "GO enrichment of cluster"
  } else if (!is.null(title) && length(title) && nzchar(as.character(title)[[1L]])) {
    as.character(title)[[1L]]
  } else {
    .tcm_enrich_title(d)
  }

  # 组内按 richFactor 升序 -> prep_y(desc = FALSE) 让最大者在顶部
  d <- d[order(d$.grp, d$richFactor), , drop = FALSE]
  d <- .enrich_bar_prep_y(d, text.width, desc = FALSE)
  ylab_fun <- .enrich_bar_ylab_fun(d)

  ggplot2::ggplot(d, ggplot2::aes(x = .data$richFactor, y = .data$.ykey)) +
    ggplot2::geom_segment(ggplot2::aes(xend = 0, yend = .data$.ykey)) +
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
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_y_discrete(labels = ylab_fun) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = text.size, colour = "black"),
      axis.title = ggplot2::element_text(
        margin = ggplot2::margin(10, 5, 0, 0),
        color = "black", size = text.size
      ),
      strip.text = ggplot2::element_text(face = "bold", size = text.size)
    ) +
    ggplot2::labs(x = "Rich Factor", y = NULL, title = ttl)
}
