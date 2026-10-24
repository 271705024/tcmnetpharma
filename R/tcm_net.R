#' Network interaction diagram of herbs, ingredients, and targets
#'
#' @param network.data data frame
#' must contain herb, molecule, target three columns of data
#' @param view \strong{数据/形状轴}（2026-10-24 新增，与 \code{style} 正交）：
#'   \describe{
#'     \item{\code{"overview"}}{（默认）节点裁到 \code{max_nodes}，颜色只区分
#'       节点类型（herb / molecule / target），只标度数最高的 15 个名字。看整体形态。}
#'     \item{\code{"cluster"}}{按 walktrap 社区着色，\strong{只标簇名、不标节点名}，
#'       质心位置放 \code{M1}/\code{M2} 标签。看结构分组。}
#'     \item{\code{"hub"}}{按度数分档着色 + 保留核心节点及其一跳邻居
#'       （\code{keep = "ego"}），标签只给 top 10（即 hub 本身）。
#'       看关键节点。}
#'   }
#'   三者共同点：\strong{只画一层边}、\strong{degree 只控点大小}、
#'   \strong{只出一个图例}。
#' @param node.color node color
#' see "RColorBrewer::display.brewer.all()"
#' @param node.size  node size
#' @param label.size label size
#' @param label.degree 度数阈值：度数 \eqn{\ge} 该值的节点显示标签。
#'   \code{NULL}（默认）时由 \code{view} 决定（overview 取 top 15）。
#'   显式给数值时\strong{覆盖} view 的取法 —— 与旧版语义一致（旧版就是绝对阈值）。
#' @param edge.color edge color。单层边，此参数直接生效。
#' @param edge.width 已废弃：新实现每层边宽度恒定（由 \code{style} 决定），
#'   边不再承载统计信息。保留形参只为向后兼容；显式传入时给出提示。
#' @param graph.layout 布局名。\code{"auto"}（默认）表示用该 \code{style} 的
#'   推荐布局（fr），固定随机种子保证可复现。
#' @param rem.dis.inter remove single free unconnected nodes
#' @param style \strong{外观轴}：视觉预设，与 \code{view} 正交 ——
#'   \code{"default"} / \code{"journal"} / \code{"cytoscape"} / \code{"presentation"}。
#'   只改边透明度、边宽、标签字号，不改节点集合与颜色通道。
#' @param palette colour scheme key, see [tcm_palette_names()]. `NULL`
#'   (default) keeps the ColorBrewer colours named by \code{node.color};
#'   a palette key replaces that base colour set.
#' @param max_nodes 节点数上限。\code{NULL}（默认）表示用 \code{view} 自带上限
#'   （overview / cluster 50，hub 30）；显式给数值则覆盖；\code{Inf} 不裁剪。
#' @param min_degree 度数下限（仅在触发裁剪时生效，见 \code{tcm_filter_network}）。
#' @param keep 超限选点策略。\code{NULL}（默认）由 \code{view} 决定
#'   （hub 用 \code{"ego"}，其余 \code{"top_degree"}）。
#' @param label.repel label repel
#'
#' @return Network Diagram（ggplot 对象）。裁剪信息挂在
#'   \code{attr(p, "cap_report")} 上：\code{list(shown_nodes=, total_nodes=,
#'   shown_edges=, total_edges=, capped=)}，供 GUI 显示
#'   「已显示 118 / 2,340 个节点」。
#' @export
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 aes
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr count
#' @importFrom ggraph ggraph
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph scale_edge_width
#' @importFrom ggraph geom_edge_link
#' @importFrom ggraph theme_graph
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph graph_from_data_frame
#' @examples
#' \dontrun{
#' data("xfbdf", package = "tcmnetpharma")
#' network.data <- xfbdf %>%
#'   dplyr::select(herb, molecule, target) %>%
#'   dplyr::slice_sample(n = 100) %>%
#'   as.data.frame()
#' tcm_net(network.data, node.color = "Spectral", label.degree = 1)
#' tcm_net(network.data, view = "cluster", style = "journal")
#' }
tcm_net <- function(network.data,
                    view = c("overview", "cluster", "hub"),
                    node.color = "RdBu",
                    node.size = c(2, 8),
                    label.size = 4,
                    label.degree = NULL,
                    label.repel = TRUE,
                    edge.color = "lightgrey",
                    edge.width = c(0.2, 2),
                    graph.layout = "auto",
                    rem.dis.inter = FALSE,
                    style = c("default", "journal", "cytoscape", "presentation"),
                    palette = NULL,
                    max_nodes = NULL,
                    min_degree = 1,
                    keep = NULL) {
  view <- match.arg(view)
  style <- match.arg(style)
  if (!missing(edge.width)) {
    message(
      "tcm_net: `edge.width` \u5df2\u5e9f\u5f03\u2014\u2014R2 \u8d77\u6bcf\u5c42\u8fb9\u5bbd\u6052\u5b9a\uff08\u7531 style \u51b3\u5b9a\uff09\uff0c",
      "\u56e0\u4e3a\u8fb9\u4e0d\u518d\u627f\u8f7d\u7edf\u8ba1\u4fe1\u606f\u3002\u5df2\u5ffd\u7565\u3002"
    )
  }
  palette <- .tcm_palette_check(palette)

  # 配色体系（2026-10-22）：统一成一组基准色，各分支都从它插值，
  # 保证「同一张图换风格不改配色逻辑」。
  node_base <- if (is.null(palette)) {
    RColorBrewer::brewer.pal(8, name = node.color)
  } else {
    .tcm_palette_ramp(palette, 8L)
  }
  network.data <- as.data.frame(network.data)

  # ★ E5 修复（2026-09-28）——「三元组列」按名取，并按节点名赋 class
  #
  # 根因（GUI 实测定位，logs/operations-2026-09-28.md 第五轮）：
  #   本函数下方用 `dplyr::select(herb, molecule, target)` **按列名**构图，
  #   却用 `network.data[, 2]`/`[, 3]` **按列位置**给 `V(net)$class` 取值。
  #   当入参含多余列（如 GUI 查询结果的 `molecule_id`）时两者错位：
  #   `[, 2]` 取到 `molecule_id`，`molecule` 列从未参与计数，
  #   `c(rep(...), rep(...), rep(...))` 的长度 ≠ 顶点数 →
  #   igraph 报 `Length of new attribute value must be 1 or 888, ... not 887.`
  #   （GUI 中四种查询结果全部触发；内置 network_data 恰好三列，故旧测试全绿 ——
  #    典型的「测试没覆盖到真实输入形状」缺陷。）
  # 方案：① 先按名取出三元组（缺列则明确报错，不静默按位置猜）；
  #       ② class 按**节点名**逐个判定（长度恒等于顶点数），角色优先级
  #          herb > molecule > target。
  need_cols <- c("herb", "molecule", "target")
  if (!all(need_cols %in% names(network.data))) {
    stop(
      "`network.data` \u5fc5\u987b\u5305\u542b\u5217\uff1a",
      paste(need_cols, collapse = ", "),
      "\uff1b\u5f53\u524d\u5217\u4e3a\uff1a", paste(names(network.data), collapse = ", ")
    )
  }
  network.data <- network.data[, need_cols, drop = FALSE]

  links <- rbind(
    network.data %>%
      dplyr::select(herb, molecule) %>%
      dplyr::rename(from = herb, to = molecule) %>%
      dplyr::mutate(weight = 1),
    network.data %>%
      dplyr::select(molecule, target) %>%
      dplyr::rename(from = molecule, to = target) %>%
      dplyr::mutate(weight = 1)
  ) %>%
    dplyr::distinct()

  # ---- rem.dis.inter：度数 1 的两端互连边（孤立对）先剔除 ----------------
  if (isTRUE(rem.dis.inter)) {
    links <- links %>%
      dplyr::mutate(from_c = dplyr::count(., from)$n[match(from, dplyr::count(., from)$from)]) %>%
      dplyr::mutate(to_c = dplyr::count(., to)$n[match(to, dplyr::count(., to)$to)]) %>%
      dplyr::filter(!(from_c == 1 & to_c == 1)) %>%
      dplyr::select(1, 2, 3)
  }

  # ---- 构图（唯一路径：旧版 rem.dis.inter 的 if/else 两个分支已合并）------
  nodes <- data.frame(node = unique(c(links$from, links$to)))
  net <- igraph::graph_from_data_frame(
    d = links, vertices = nodes, directed = FALSE
  )
  igraph::V(net)$degree <- igraph::degree(net)
  igraph::V(net)$class <- .tcm_node_class(igraph::V(net)$name, network.data)

  # ---- view 感知的容量裁剪（2026-10-24 起 view 决定默认上限）------------
  # 原实现完全没有容量控制时，数百/上千节点在力导向布局下糊成一团
  # （boss 反馈「药物-靶点图爆炸」）。现走统一管线 tcm_filter_network。
  # 顶点属性（class）由 .tcm_net_cap 统一保留，这里不再手工重挂。
  capt <- .tcm_net_cap(net, view = view, max_nodes = max_nodes,
                       min_degree = min_degree, keep = keep)
  net <- capt$net
  # degree 必须按**裁剪后的子图**重算：裁剪会重建图并丢掉拓扑相关属性，
  # 若沿用全图度数，点大小会与标签阈值口径不一致。
  igraph::V(net)$degree <- igraph::degree(net)
  if (isTRUE(capt$report$capped)) {
    message(sprintf(
      "tcm_net (view = \"%s\"): %d \u4e2a\u8282\u70b9\u8d85\u8fc7\u663e\u793a\u4e0a\u9650\uff0c\u5df2\u4fdd\u7559 %d \u4e2a\u6838\u5fc3\u8282\u70b9\uff08%d \u6761\u8fb9\uff09\u3002",
      view, capt$report$total_nodes, capt$report$shown_nodes,
      capt$report$shown_edges
    ))
  }

  p <- .tcm_net_plot(
    net, view = view, style = style,
    node.size = node.size, label.size = label.size,
    graph.layout = graph.layout, node_base = node_base, palette = palette,
    label_degree = label.degree, label_repel = label.repel,
    edge_color = edge.color
  )
  attr(p, "cap_report") <- capt$report
  p
}
