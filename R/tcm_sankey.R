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
#' @param max_nodes 桑基图节点总数硬上限。分层过滤后若端点总数仍超过该值，
#'   则逐层收紧 `max_per_level`（每次减半，下限 1）直到达标。`Inf` 表示不限。
#'   默认 60。用于防止「桑基图爆炸」（节点过多导致标签重叠、渲染卡死）。
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
                       max_nodes = 60,
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

  # ---- 节点总数硬闸（2026-10-24）----
  # 分层 top-N 过滤后，端点总数仍可能超限（例如某层取值极分散、top-N 内仍有
  # 大量低频值，或 max_per_level 被显式调大）。此时逐层收紧 max_per_level
  # （每次减半，下限 1），直到过滤后端点总数 <= max_nodes。这是「桑基图爆炸」
  # 的最后一道硬闸：只靠前端滑条/预设不够，包层必须兜底。
  #
  # ⚠️ 2026-10-24 实测修正：收紧必须用 **strict 硬截**。宽松模式在「某层频率
  #   全部相同」的数据上（典型：MOL 编号列每行唯一）`tb >= cutoff` 会保留全部
  #   取值 → 收紧到 1 也仍是全量，硬闸形同虚设（实测 300 端点纹丝不动）。
  #   另外每次**从同一份基准重算**，避免层层叠加把行数削到 0。
  if (is.data.frame(data_sankey) && nrow(data_sankey) > 0L) {
    mn <- suppressWarnings(as.numeric(max_nodes)[[1]])
    if (is.na(mn)) mn <- Inf
    if (is.finite(mn) && mn > 0L) {
      base_df <- data_sankey          # 基准（已按 max_per_level 过滤）
      cur_mpl <- suppressWarnings(as.numeric(max_per_level)[[1]])
      if (is.na(cur_mpl) || !is.finite(cur_mpl)) cur_mpl <- nrow(base_df)
      cur_mpl <- max(1L, as.integer(cur_mpl))
      n_endpoints <- .sankey_endpoint_count(base_df)
      while (n_endpoints > mn && cur_mpl > 1L) {
        cur_mpl <- max(1L, floor(cur_mpl / 2L))
        cand <- .sankey_filter_levels(base_df, cur_mpl, strict = TRUE)
        if (nrow(cand) == 0L) break   # 再紧就无完整路径：停在上一个可行解
        n_cand <- .sankey_endpoint_count(cand)
        if (n_cand == 0L) break
        data_sankey <- cand
        n_endpoints <- n_cand
      }
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
# 桑基图分层过滤（2026-10-23，P2；2026-10-24 加 strict 硬截模式）
#
# 每一层（列）按出现频率取 top-N 个值，然后保留「整行路径的所有层值都在各自
# top-N 集合内」的行。关键：**按完整路径过滤**（不截行）—— 旧的按行 head()
# 会把「药-成分-靶点」路径截成半截，产生大量孤节点（boss 反馈「桑基图爆炸」）。
# 层名与层数保持数据本来的列（不硬编码 herb/molecule/target）。
#
# strict 两个模式的差别（2026-10-24 实测踩到，必须区分）：
#   strict = FALSE（默认，日常 max_per_level 用）：**并列第 top_n 名的都保留**，
#     避免随机丢弃并列项。副作用：当某层所有取值频率相同（如全为 1，典型是
#     MOL 编号列），`tb >= cutoff` 会把**全部**取值放进 top_vals → 过滤完全失效。
#   strict = TRUE（仅 max_nodes 硬闸收紧循环用）：**按位置硬截前 top_n 个**，
#     保证层内值个数真的 <= top_n。没有它，max_nodes 在「全并列」数据上
#     逐层收紧到 1 也仍然是全量，硬闸形同虚设（实测 300 端点纹丝不动）。
# ---------------------------------------------------------------------------
.sankey_filter_levels <- function(df, top_n, strict = FALSE) {
  keep_mask <- rep(TRUE, nrow(df))
  for (cn in names(df)) {
    vals <- as.character(df[[cn]])
    vals[is.na(vals)] <- ""
    # 每层按频率取 top_n
    tb <- sort(table(vals[vals != ""]), decreasing = TRUE)
    if (length(tb) > top_n) {
      if (isTRUE(strict)) {
        # 硬截：按频率降序取前 top_n 个（同名频率时 sort 的次序稳定可复现）
        top_vals <- names(tb)[seq_len(as.integer(top_n))]
      } else {
        # 并列第 top_n 名的都保留，避免随机丢弃（原 P2 语义）
        cutoff <- as.integer(tb[[top_n]])
        top_vals <- names(tb)[as.integer(tb) >= cutoff]
      }
      keep_mask <- keep_mask & (vals %in% top_vals)
    }
  }
  df[keep_mask, , drop = FALSE]
}

# ---------------------------------------------------------------------------
# 桑基图端点总数（2026-10-24）
#
# 节点数 = 各层（列）取值去重后的总数。注意：同一取值出现在不同层（例如
# 「成分名」既是中间层又是末层）会被当作不同节点各计一次，与 ggsankey 的
# make_long + unique(node) 语义一致（node 由 layer 位置决定）。
# ---------------------------------------------------------------------------
.sankey_endpoint_count <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(0L)
  sum(vapply(df, function(col) {
    v <- as.character(col)
    v <- v[!is.na(v) & v != ""]
    length(unique(v))
  }, integer(1)))
}
