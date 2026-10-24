#' Suggest a canvas size for an enrichment plot
#'
#' 富集图「能出但难看」的最常见原因是**画布尺寸与内容不匹配**：24 条目、
#' 标签折成 3 行，硬塞进 7 英寸高的画布必然叠字。本函数按**实际槽位数**
#' （已含多行标签预留的空槽）与最长标签宽度，算出建议的 `ggsave()` 尺寸。
#'
#' 设计取舍：包层**不知道**用户最终画布多大，所以无法在绘图阶段自适应；
#' 与其猜一个默认值，不如把「该用多大」算出来交还给导出环节。
#'
#' @param p 由 [enrich_barplot()]、[go_barplot()]、[go_dotplot()] 或
#'   [go_lollipop()] 返回的 ggplot 对象
#' @param width_per_char 每个字符占的英寸宽（默认 0.075，约合 10pt 无衬线）
#' @param height_per_line 每行标签占的英寸高（默认 0.26）
#' @param label_share 纵轴标签区占画布宽度的比例（默认 0.55）
#' @param chrome 标题 / 坐标轴 / 边距占用的固定高度（英寸，默认 1.4）
#' @param min_width,min_height 下限（英寸）
#' @return 命名数值向量 `c(width = , height = )`，可直接喂给 `ggsave()`
#' @export
#' @examples
#' \dontrun{
#' p <- enrich_barplot(tcm_enrich_table(res), top = "full")
#' sz <- tcm_plot_size(p)
#' ggplot2::ggsave("out.png", p, width = sz[["width"]], height = sz[["height"]],
#'                 dpi = 300)
#' }
tcm_plot_size <- function(p, width_per_char = 0.075, height_per_line = 0.26,
                          label_share = 0.55, chrome = 1.4,
                          min_width = 6, min_height = 3) {
  if (!inherits(p, "ggplot")) {
    stop("`p` \u5fc5\u987b\u662f ggplot \u5bf9\u8c61\u3002", call. = FALSE)
  }
  d <- p$data
  if (!is.data.frame(d) || !".ykey" %in% names(d)) {
    stop(
      "tcm_plot_size(): \u65e0\u6cd5\u4ece\u8fd9\u5f20\u56fe\u63a8\u65ad\u5c3a\u5bf8\u3002\n",
      "  \u4ec5\u652f\u6301 enrich_barplot() / go_barplot() / go_dotplot() / ",
      "go_lollipop() \u8fd4\u56de\u7684\u56fe\uff08\u5b83\u4eec\u7684\u6570\u636e\u91cc\u5e26 .ykey \u5217\uff09\u3002",
      call. = FALSE
    )
  }

  # 槽位数（含为多行标签预留的空槽）—— 这是高度真正的驱动量
  n_slots <- nlevels(d$.ykey)
  if (!is.finite(n_slots) || n_slots < 1L) n_slots <- nrow(d)

  # 最长的一行标签（不是最长的一条标签：折行后按「单行」算宽度）
  txt <- if (".ylab_txt" %in% names(d)) as.character(d$.ylab_txt) else
    as.character(d$Description)
  # ⚠️ 按「折行后单行最长」算宽度，不能用整条标签的 nchar()：
  #    一条 90 字的标签折成 3×30 字，宽度需求是 30 字，不是 90 字。
  lines <- strsplit(txt, "\n", fixed = TRUE)
  max_chars <- max(c(1L, vapply(lines, function(z) max(nchar(z)), integer(1))),
                   na.rm = TRUE)
  if (!is.finite(max_chars) || max_chars < 1L) max_chars <- 10L

  width <- max_chars * width_per_char / max(0.15, min(0.9, label_share))
  height <- n_slots * height_per_line + chrome

  c(width = max(min_width, round(width, 2)),
    height = max(min_height, round(height, 2)))
}
