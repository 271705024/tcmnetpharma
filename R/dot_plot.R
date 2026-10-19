#'  Bubble graphs showed the results of GO and KEGG analysis
#'
#' @param data R clusterprofiler package for KEGG or GO results
#' @param color color see "RColorBrewer::display.brewer.all()"
#' @param top number of categories to show
#' @param text.size text size
#' @param text.width y-axis label length
#' @param title  title
#' @param ... additional parameters
#'
#' @return dotplot
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 guide_colorbar
#' @importFrom dplyr mutate
#' @importFrom stats reorder
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringr str_wrap
#' @examples
#' \dontrun{
#' data(xfbdf, package = "TCMNP")
#' # 富集结果：全部离线，无需 OrgDb 包
#' res <- tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human",
#'                   pvalueCutoff = 0.05, qvalueCutoff = 1)
#' BP <- res$go            # 等价 enrichGO(ont = "ALL")
#' KK <- res$wikipathways  # 通路维度（KEGG 的离线替代）
#' # 可选：KEGG 需联网，数据不随包分发（见 inst/extdata/DATA-README.md）
#' # KK <- clusterProfiler::enrichKEGG(entrez, organism = "hsa")
#' dot_plot(KK, title = "WikiPathways")
#' dot_plot(BP, title = "biological process")
#' }
dot_plot <- function(data,
                     color = "RdBu",
                     top = 15,
                     text.size = 10,
                     text.width = 35,
                     title = NULL, ...) {
  # Slice to the requested number of categories.
  #
  # ## ★ E2 修复（2026-09-21）：`top` 参数被静默忽略 → 图表退化
  # 根因 —— `top` 曾以 `ggplot(data2, showCategory = top, ...)` 形式转发，
  #   但 `showCategory` 并非 ggplot2 的合法参数。ggplot2 对未知参数**静默忽略**，
  #   因此切片从未发生、全部行次照画（实测：2,214 条 GO term 把 y 轴糊成黑块）。
  # 方案 —— 显式取前 `top` 行；与 `bubble_plot()` 既有的 `[1:top, ]` 行为对齐。
  # 回归锚点 —— `logs/errors-xuanfuhua-e2e-2026-09-21.md` §E2；
  #   验证见 `tests/xuanfuhua-2026-09-21/output`（4 张 top 驱动图均 rows=15）。
  if (isS4(data)) data <- data@result
  data <- as.data.frame(data)
  if (!is.null(top) && is.finite(top) && top > 0 && nrow(data) > top) {
    data <- data[seq_len(top), , drop = FALSE]
  }
  # Rich Factor
  data2 <- dplyr::mutate(data,
                         richFactor = Count / as.numeric(sub(
                           "/\\d+", "",
                           BgRatio
                         ))
  )
  # ggplot2 plotting
  p <- ggplot(data2,
              aes(richFactor, stats::reorder(Description, richFactor))
  ) +
    geom_point(aes(color = p.adjust, size = Count)) +
    scale_color_gradientn(
      colours = RColorBrewer::brewer.pal(8, color),
      trans = "log10",
      guide = guide_colorbar(reverse = TRUE, order = 1)
    ) +
    scale_size_continuous(range = c(2, 10)) +
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
    theme(axis.title = element_text(
      margin = margin(10, 5, 0, 0),
      color = "black",
      size = text.size
    )) +
    ggtitle(title) +
    xlab("Rich Factor") +
    ylab(NULL) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = text.width))
  return(p)
}
