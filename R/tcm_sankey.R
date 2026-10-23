#' Sankey diagram
#'
#' @param data_sankey data.frame
#' @param text.size text size
#' @param text.position text position 0, 0.5, 1
#' @param x.axis.text.size Location of x-axis labels in sankey plot
#' @param style visual preset: \code{"default"} keeps the original look, \code{"journal"} applies a publication theme (white background, no grid, thin axis lines), \code{"presentation"} enlarges text and axis lines for slides. Only visuals change; data and statistics are identical across presets.
#' @param palette colour scheme key, see [tcm_palette_names()]. `NULL`
#'   (default) keeps the built-in rainbow palette. Colours only.
#' @param max_per_level 每层（列）保留的出现频率最高的条目数。按完整路径过滤
#'   （不截行），节点数 = 过滤后路径端点数。`Inf` 表示不过滤。默认 10。
#' @param ... additional parameters
#'
#' @return figure
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 position_nudge
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom dplyr mutate
#' @importFrom ggsankey make_long
#' @importFrom ggsankey geom_sankey
#' @importFrom ggsankey geom_sankey_text
#' @importFrom ggsankey theme_sankey
#' @importFrom cols4all c4a
#'
#' @examples
#' data("xfbdf", package = "tcmnetpharma")
#' data <- xfbdf %>% dplyr::slice_sample(n = 30)
#' tcm_sankey(data, text.position = 1)

tcm_sankey <- function(data_sankey,
                       text.size = 3,
                       text.position = 0,
                       x.axis.text.size = 12,
                       style = c("default", "journal", "presentation"),
                       palette = NULL,
                       max_per_level = 10,
                       ...){
  style <- match.arg(style, .tcm_style_generic)
  palette <- .tcm_palette_check(palette)

  # ---- 分层 top-N 过滤（2026-10-23，P2）----
  # 原实现无任何过滤，全量 make_long；大数据量时节点数爆炸。GUI 侧曾按行截断
  # （head），会把「药-成分-靶点」路径截成半截、产生大量孤节点。
  # 现改为**分层过滤**：每一层（列）取出现频率最高的 top-N 个值，保留「整行
  # 路径的所有层值都在各自 top-N 集合内」的行 —— 按完整路径过滤，不截行，
  # 节点数 = 过滤后路径的端点数（可控且连线完整）。
  # max_per_level = Inf 回退为全量（不过滤）。
  if (is.data.frame(data_sankey) && nrow(data_sankey) > 0L) {
    mpl <- suppressWarnings(as.numeric(max_per_level)[[1]])
    if (is.na(mpl)) mpl <- Inf
    if (is.finite(mpl) && mpl > 0L) {
      data_sankey <- .sankey_filter_levels(data_sankey, as.integer(mpl))
    }
  }

  # color settings
  df <- data_sankey %>%
        as.data.frame()%>%
        make_long(colnames(data_sankey))
  n_node <- length(unique(df$node))
  mycol2 <- if (!is.null(palette)) {
    # 配色体系指定时用它（节点数超过色板原生色数会插值补足并提示）
    tcm_palette(palette, n_node)
  } else {
    # ⚠️ 2026-10-22 修：原实现是 `sample(mycol, ...)` —— 用全局 RNG 打乱配色，
    # 同一份数据两次出图**颜色不同**，投稿复现直接失败。改为按节点出现顺序
    # 确定性分配：可复现，且与 style 的「可复现」承诺一致（同 ppi_plot 的
    # set.seed 处理）。
    cols4all::c4a("rainbow_wh_rd", n_node)
  }
  # Sankey diagram
  p <- do.call(rbind, apply(data_sankey, 1, function(x) {
    data.frame(
      x = names(x), node = x,
      next_x = dplyr::lead(names(x)),
      next_node = dplyr::lead(x), row.names = NULL
    )
  })) %>%
    dplyr::mutate(
      x = factor(x, names(data_sankey)),
      next_x = factor(next_x, names(data_sankey))
    ) %>%
    ggplot(aes(
      x = x,
      next_x = next_x,
      node = node,
      next_node = next_node,
      fill = node,
      label = node
    )) +
    ggsankey::geom_sankey(
      flow.alpha = 0.5,
      node.color = NA,
      show.legend = FALSE
    ) +
    ggsankey::geom_sankey_text(
      size = text.size, color = "black",
      hjust = text.position,
      position = position_nudge(x = -0.05)
    ) +
    ggsankey::theme_sankey(base_size = 20) +
    scale_fill_manual(values = mycol2) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(
      size = x.axis.text.size,
      hjust = 0.5, vjust = 10,
      colour = "black"
    ))
  return(.tcm_style_apply(p, style))
}


# ---------------------------------------------------------------------------
# 桑基图分层过滤（2026-10-23，P2）
#
# 每一层（列）按出现频率取 top-N 个值，然后保留「整行路径的所有层值都在各自
# top-N 集合内」的行。关键：**按完整路径过滤**（不截行）—— 旧的按行 head()
# 会把「药-成分-靶点」路径截成半截，产生大量孤节点（boss 反馈「桑基图爆炸」）。
# 层名与层数保持数据本来的列（不硬编码 herb/molecule/target）。
# ---------------------------------------------------------------------------
.sankey_filter_levels <- function(df, top_n) {
  keep_mask <- rep(TRUE, nrow(df))
  for (cn in names(df)) {
    vals <- as.character(df[[cn]])
    vals[is.na(vals)] <- ""
    # 每层按频率取 top_n（并列第 top_n 名的都保留，避免随机丢弃）
    tb <- sort(table(vals[vals != ""]), decreasing = TRUE)
    if (length(tb) > top_n) {
      cutoff <- as.integer(tb[[top_n]])
      top_vals <- names(tb)[as.integer(tb) >= cutoff]
      keep_mask <- keep_mask & (vals %in% top_vals)
    }
  }
  df[keep_mask, , drop = FALSE]
}
