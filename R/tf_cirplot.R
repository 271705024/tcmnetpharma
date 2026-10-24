#' Display the relationship between transcription factors and target genes
#'
#' @param data data.frame
#' @param start.degree the angle at which the circle plot starts
#' @param text.size the font size of each layer circle
#' @param color color see "RColorBrewer::display.brewer.all()"
#' @param tf_col,target_col \strong{显式指定列名}（2026-10-24 新增）。
#'   不传时按列名精确匹配（\code{TF} / \code{Target} 及常见别名）；
#'   匹配不到\strong{直接报错并列出候选列名}，不再按「两列的基数谁大」猜。
#' @param ... additional parameters
#'
#' @return chord diagram
#' @export
#'
#' @importFrom circlize circos.par
#' @importFrom circlize chordDiagram
#' @importFrom circlize circos.track
#' @importFrom circlize get.cell.meta.data
#' @importFrom circlize circos.text
#' @importFrom circlize colorRamp2
#' @importFrom circlize get.all.sector.index
#' @importFrom circlize circos.clear
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \dontrun{
#' data(xfbdf, package = "tcmnetpharma")
#' tf_data <- tf_filter(xfbdf$target)
#' set.seed(1234)
#' data <- tf_data[, 1:2] %>%
#'   distinct() %>%
#'   dplyr::slice_sample(n = 100)
#' tf_cirplot(data, color = "Spectral")
#' }
tf_cirplot <- function(data,
                       start.degree = 270,
                       text.size = 0.6,
                       color = "RdBu",
                       tf_col = NULL,
                       target_col = NULL, ...) {
  # data processing
  if (is.data.frame(data)) {
    data <- as.data.frame(data)
    # ★ 列解析（2026-10-24）：显式参数 > 列名精确匹配 > 报错。绝不按基数猜。
    rc <- .tcm_resolve_cols(
      data,
      explicit = list(TF = tf_col, Target = target_col),
      aliases = list(
        TF = c("TF", "tf", "transcription_factor"),
        Target = c("Target", "target", "gene", "target_gene")
      ),
      what = "TF \u5217\u4e0e\u9776\u70b9\u5217\uff08TF / Target\uff09",
      fn = "tf_cirplot"
    )
    if (rc[["TF"]] == rc[["Target"]]) {
      stop("tf_cirplot: TF \u5217\u4e0e Target \u5217\u6307\u5411\u540c\u4e00\u5217\uff08",
           names(data)[rc[["TF"]]], "\uff09\uff0c\u65e0\u6cd5\u753b\u5f26\u56fe\u3002",
           call. = FALSE)
    }
    data <- data.frame(
      TF = as.character(data[[rc[["TF"]]]]),
      Target = as.character(data[[rc[["Target"]]]]),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::distinct()

    cols_number <- length(unique(data$TF))
    # ⚠️ 不要把调色结果命名为 tf_col：那是本函数的**形参名**（列名参数），
    #    同名会覆盖用户传进来的列名，且报错信息会指向错误的东西。
    cols <- brewer.pal(min(8L, max(3L, cols_number)), color)
    tf_pal <- colorRampPalette(cols)(length(unique(data$TF)))
    names(tf_pal) <- unique(data$TF)

    Target_col <- rep("lightgrey", length(unique(data$Target)))
    names(Target_col) <- unique(data$Target)
    grid.col <- c(tf_pal, Target_col)

    # drawing
    circos.clear()
    circlize::circos.par(
      canvas.xlim = c(-1, 1),
      canvas.ylim = c(-1, 1),
      start.degree = start.degree
    )

    chordDiagram(data,
      grid.col = grid.col,
      link.decreasing = TRUE,
      transparency = 0.1,
      big.gap = 1,
      annotationTrack = "grid",
      preAllocateTracks = list(track.height = .1)
    )

    for (si in get.all.sector.index()) {
      xlim <- get.cell.meta.data("xlim",
        sector.index = si,
        track.index = 1
      )
      ylim <- get.cell.meta.data("ylim",
        sector.index = si,
        track.index = 1
      )
      circos.text(mean(xlim),
        ylim[1],
        labels = si,
        sector.index = si,
        track.index = 1,
        facing = "clockwise",
        cex = text.size,
        adj = c(0, 0.5),
        niceFacing = TRUE
      )
    }
    circos.clear()
  } else {
    print("The data must be a data frame with two columns.")
  }
}
