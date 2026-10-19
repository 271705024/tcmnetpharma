#' Bar graphs showed the results of GO and KEGG analysis
#'
#' @param data R clusterprofiler package for KEGG and GO results
#' @param top according to the order of p adjust value from small to large
#' the number of categories to show
#' @param color color see "RColorBrewer::display.brewer.all()"
#' @param text.size text size
#' @param text.width y-axis label length
#' @param text.bar The size of the text at the top of the bar graph
#' @param title  title
#' @param sort.by order the bars by "count" (default, largest first) or by
#' "p.adjust" (most significant first). Use "p.adjust" when you want the most
#' meaningful terms on top -- sorting by count tends to surface broad umbrella
#' terms that are significant in any gene list.
#' Kyoto Encyclopedia of Genes and Genomes, KEGG
#' cellular component, CC
#' molecular function, MF
#' biological process, BP
#' @param ... additional parameters
#'
#' @return barplot
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 guide_colorbar
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom dplyr mutate
#' @importFrom dplyr slice
#' @importFrom stats reorder
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringr str_wrap
#'
#' @examples
#' \dontrun{
#' data(xfbdf, package = "TCMNP")
#' # 富集结果：GO / Reactome / WikiPathways 全部离线，无需 OrgDb 包
#' res <- tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human",
#'                   pvalueCutoff = 0.05, qvalueCutoff = 1)
#' BP <- res$go            # 等价 enrichGO(ont = "ALL")，含 ONTOLOGY 列
#' KK <- res$wikipathways  # 通路维度（KEGG 的离线替代）
#' # 可选：KEGG 需联网，且 KEGG 数据不随包分发（见 inst/extdata/DATA-README.md）
#' # KK <- clusterProfiler::enrichKEGG(entrez, organism = "hsa")
#' bar_plot(KK)
#' bar_plot(head(KK@result, 10), title = "WikiPathways", color = "Spectral")
#' bar_plot(BP@result[2:11,], title = "biological process", color = "Spectral")
#' }
bar_plot <- function(data,
                     top = 10,
                     color = "RdBu",
                     text.size = 10,
                     text.width = 35,
                     text.bar = 4,
                     title = NULL,
                     sort.by = c("count", "p.adjust"), ...) {
  # data processing
  sort.by <- match.arg(sort.by)
  if (isS4(data)) {
    data <- data@result %>% tidyr::drop_na()
  } else if (is.data.frame(data)) {
    data <- data %>% tidyr::drop_na()
  } else {
    print("The data format must be S4 object or data frame.")
  }
  # Ordering.
  #
  # ## ★ E4 修复（2026-09-21）：排序硬编码为 Count → 无法突出最显著条目
  # 根因 —— 柱条恒按 `reorder(Description, Count)` 排序，调用方无法按显著性出图。
  #   按 Count 排序会把**泛化伞状词条**顶到最前：如 GO:0005515「protein binding」
  #   （1,276 个基因）在任何人类基因列表里都排第一，却不携带任何特异性生物学信号。
  # 方案 —— 新增 `sort.by` 参数，`"p.adjust"` 时按显著性排序（次键 -Count）。
  # 回归锚点 —— `logs/errors-xuanfuhua-e2e-2026-09-21.md` §E4；
  #   验证见 `tests/xuanfuhua-2026-09-21`（bar_plot 含 sort.by 即通过）。
  if (sort.by == "p.adjust") {
    data <- data[order(data$p.adjust, -data$Count), , drop = FALSE]
  } else {
    data <- data[order(-data$Count), , drop = FALSE]
  }
  # Rich Factor
  data2 <- dplyr::mutate(data,
    richFactor = Count / as.numeric(sub(
      "/\\d+", "",
      BgRatio
    ))
  ) %>%
    dplyr::slice(1:top)
    data2$richFactor <- data2$richFactor %>% round(digits = 2)
  # 绘图顺序。
  #
  # ## ★ E4 附带修复（2026-09-21）：`reorder()` 写在 `aes()` 内 → 双 y 标度
  # 根因 —— 在 `aes()` 里调 `reorder()` 会创建第二个 y 标度，触发
  #   `Scale for y is already present` 警告，并**静默覆盖**下方的 `scale_y_discrete()`。
  # 方案 —— 改为显式设定 `factor(levels=...)`，保持单一标度，顺序无歧义。
  ord <- order(if (sort.by == "p.adjust") data2$p.adjust else -data2$Count)
  data2$Description <- factor(data2$Description,
                              levels = rev(data2$Description[ord]))
  # ggplot2 plotting
  p <- ggplot(data2) +
    geom_col(aes(x = Count, y = Description, fill = p.adjust),
      color = "black",
      width = 0.6
    ) +
    geom_text(aes(x = Count, y = Description, label = Count),
      hjust = -0.05,
      vjust = 0.5,
      size = text.bar
    ) +
    scale_fill_gradientn(
      colours = RColorBrewer::brewer.pal(8, color),
      trans = "log10",
      guide = guide_colorbar(reverse = TRUE, order = 1)
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        size = text.size,
        colour = "black"
      ),
      axis.text.y = element_text(
        angle = 0,
        size = text.size,
        face = "plain",
        colour = "black"
      )
    ) +
    scale_y_discrete(
      expand = c(0, -1),
      labels = function(x) str_wrap(x, width = text.width)
    ) +
    theme(legend.position = "right") +
    ylab(NULL) +
    ggtitle(title)
  return(p)
}
