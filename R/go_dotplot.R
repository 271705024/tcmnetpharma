#' Dot plot showed the results of GO results
#'
#' @param go.diff R clusterprofiler package for enrichGO results
#' @param top According to the order of p adjust value from small to large
#' the number of categories to show
#' @param color color see "RColorBrewer::display.brewer.all()"
#' @param text.size text size
#' @param text.width text width
#' @param title title
#' @param style visual preset. \code{"default"} keeps the original look
#'   (x = Rich Factor). \code{"genratio"} uses the clusterProfiler convention
#'   most papers follow: x = \code{GeneRatio} (k/n), dot size = \code{Count},
#'   colour = \code{p.adjust}. \code{"journal"} / \code{"presentation"}
#'   apply a publication / slide theme on top of the default form.
#'   **Only the visual form changes** — the selected terms and their
#'   statistics are identical across presets.
#' @param palette colour scheme key, see [tcm_palette_names()]. `NULL`
#'   (default) keeps the ColorBrewer ramp named by \code{color}; a palette
#'   key replaces that ramp (interpolated to a continuous scale).
#'   Colours only — data and statistics are untouched.
#' @param ... additional parameters
#'
#' @return dotplot
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
#' # 富集结果：GO 离线即可，无需 OrgDb 包（等价 enrichGO(ont = "all")）
#' data(xfbdf, package = "tcmnetpharma")
#' res <- tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human",
#'                   pvalueCutoff = 0.01, qvalueCutoff = 0.05)
#' go.diff <- res$go
#' go_dotplot(go.diff, color = "Spectral")
#' # 文献标准口径（2026-10-22 新增预设）：横轴 GeneRatio，点大小 = 基因数
#' go_dotplot(go.diff, top = 10, style = "genratio")
#' }
go_dotplot <- function(go.diff,
                       top = 5,
                       color = "RdBu",
                       text.size = 12,
                       text.width = 35,
                       title = "GO enrichment of cluster",
                       style = c("default", "genratio", "journal", "presentation"),
                       palette = NULL,
                       ...) {
  style <- match.arg(style)
  palette <- .tcm_palette_check(palette)
  # 连续型配色：点色映射的是 p.adjust，故取色阶（不是分类色）
  grad_cols <- if (is.null(palette)) RColorBrewer::brewer.pal(8, color) else
    .tcm_palette_ramp(palette, 8L)
  # data processing
  if (isS4(go.diff)) {
    go.diff <- go.diff@result
  } else if (is.data.frame(go.diff)) {
    go.diff <- go.diff
  } else {
    print("The data format must be S4 object or data frame.")
  }

  go_enrich <- go.diff %>%
    tidyr::drop_na() %>%
    dplyr::group_by(ONTOLOGY) %>%
    dplyr::slice_max(Count, n = top) %>%
    dplyr::mutate(richFactor = Count / as.numeric(sub("/\\d+", "", BgRatio)))
  go_enrich <- go_enrich[order(go_enrich$richFactor,
    decreasing = FALSE
  ), ]
  go_enrich$Description <- factor(go_enrich$Description,
    levels = go_enrich$Description
  )

  # ---- 文献形制预设（2026-10-22）-------------------------------------------
  # genratio：clusterProfiler dotplot 的经典口径 —— 横轴 GeneRatio（k/n），
  # 点大小 = Count，颜色 = p.adjust。文献里绝大多数 GO 气泡图都是这个形态，
  # 而本函数原实现用 Rich Factor（Count / 该条目注释基因总数），两者语义不同：
  #   GeneRatio = 命中基因数 / 输入的已注释基因数（衡量"我这份基因里占比"）
  #   RichFactor = 命中基因数 / 该条目注释基因总数（衡量"条目被覆盖比例"）
  # 二者都是有效口径，故做成预设而不是替换默认（不改变 default 的历史输出）。
  if (identical(style, "genratio")) {
    return(.go_dotplot_genratio(go_enrich, grad_cols, text.size, text.width, title))
  }

  # ggplot2 plotting
  p <- ggplot(go_enrich, aes(richFactor, Description)) +
    geom_point(aes(color = p.adjust, size = Count)) +
    facet_grid(ONTOLOGY ~ ., scales = "free_y", space = "free_y") +
    scale_color_gradientn(
      colours = grad_cols,
      trans = "log10",
      guide = guide_colorbar(reverse = TRUE, order = 1)
    ) +
    scale_size_continuous(range = c(2, 8)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = text.size,
        colour = "black",
        vjust = 1
      ),
      axis.text.y = element_text(
        size = text.size,
        colour = "black",
        hjust = 1
      )
    ) +
    xlab("Rich Factor") +
    ylab(NULL) +
    ggtitle(title) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = text.width)) +
    theme(strip.text = element_text(face = "bold", size = text.size))
  # 通用主题型预设（journal / presentation）后置套用；default 原样返回
  return(.tcm_style_apply(p, style))
}


# ---------------------------------------------------------------------------
# genratio 形制（2026-10-22）
#
# 口径说明见调用点的注释。GeneRatio 缺失（或存在非数值）时**整体退回
# Rich Factor 并给出 message** —— 不做「部分行静默丢掉」：
#   气泡图少几个点比多点一个假的 x 轴更危险（读者无从发现）。
# ---------------------------------------------------------------------------
.go_dotplot_genratio <- function(go_enrich, grad_cols, text.size, text.width, title) {
  xl <- "GeneRatio"
  gr <- rep(NA_real_, nrow(go_enrich))
  if ("GeneRatio" %in% names(go_enrich)) {
    k <- suppressWarnings(as.numeric(sub("/.*$", "", as.character(go_enrich$GeneRatio))))
    n <- suppressWarnings(as.numeric(sub("^.*/", "", as.character(go_enrich$GeneRatio))))
    ok <- is.finite(k) & is.finite(n) & n > 0
    gr[ok] <- k[ok] / n[ok]
  }
  if (all(!is.finite(gr))) {
    message("go_dotplot(style = \"genratio\"): \u6570\u636e\u65e0\u53ef\u7528 GeneRatio\uff0c\u5df2\u9000\u56de Rich Factor\u3002")
    gr <- go_enrich$richFactor
    xl <- "Rich Factor"
  }
  go_enrich$x_value <- gr
  go_enrich <- go_enrich[order(go_enrich$x_value), ]
  go_enrich$Description <- factor(go_enrich$Description,
                                  levels = go_enrich$Description)

  ggplot(go_enrich, aes(x_value, Description)) +
    geom_point(aes(color = p.adjust, size = Count)) +
    facet_grid(ONTOLOGY ~ ., scales = "free_y", space = "free_y") +
    scale_color_gradientn(
      colours = grad_cols,
      trans = "log10",
      guide = guide_colorbar(reverse = TRUE, order = 1)
    ) +
    scale_size_continuous(range = c(2, 8)) +
    scale_x_continuous(expand = expansion(mult = c(0.04, 0.10))) +
    theme_bw() +
    theme(
      axis.text = element_text(size = text.size, colour = "black"),
      axis.title.x = element_text(size = text.size),
      strip.text = element_text(face = "bold", size = text.size),
      panel.grid.minor = element_blank()
    ) +
    xlab(xl) +
    ylab(NULL) +
    ggtitle(title) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = text.width))
}
