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
#' library(clusterProfiler)
#' library(org.Hs.eg.db)
#' library(DOSE)
#' eg <- bitr(unique(xfbdf$target),
#'   fromType = "SYMBOL",
#'   toType = "ENTREZID",
#'   OrgDb = "org.Hs.eg.db"
#' )
#' KK <- enrichKEGG(
#'   gene = eg$ENTREZID,
#'   organism = "hsa",
#'   pvalueCutoff = 0.05
#' )
#' KK <- setReadable(KK, "org.Hs.eg.db", keyType = "ENTREZID")
#' BP <- enrichGO(
#'   gene = eg$ENTREZID,
#'   "org.Hs.eg.db",
#'   ont = "BP",
#'   pvalueCutoff = 0.05,
#'   readable = TRUE
#' )
#' bar_plot(KK)
#' bar_plot(KK@result[c( 11, 15, 17, 33, 41, 42, 47, 48,52, 53), ], title = "KEGG", color = "Spectral")
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
  # DEFECT FIXED 2026-09-21: the bars were always drawn in Count order
  # (via reorder(Description, Count)), so a caller could not surface the most
  # SIGNIFICANT terms. Sorting by Count puts uninformative umbrella terms on
  # top -- e.g. GO:0005515 "protein binding" (1,276 genes) ranks first in any
  # human gene list while carrying no specific biological signal.
  # `sort.by = "p.adjust"` now ranks by significance instead.
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
  # Set the drawing order explicitly by fixing factor levels, rather than by
  # calling reorder() inside aes(). Doing it in aes() creates a second y scale
  # and triggers "Scale for y is already present" while silently overriding
  # the scale_y_discrete() below; fixing the levels keeps a single scale and
  # makes the order unambiguous.
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
