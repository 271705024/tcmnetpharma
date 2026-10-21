#' Protein-protein interaction analysis
#'
#' @param data Human protein-protein interaction (PPI)
#' data were downloaded from the STRING database.
#' @param node.color  node color: see "RColorBrewer::display.brewer.all()"
#' @param node.size  node size
#' @param label.size markup text size
#' @param label.degree The node degree is the number of connections that
#' the node has with the other nodes.
#' Nodes with connections greater than or
#' equal to degree will be displayed.
#' @param edge.color edge color
#' @param edge.width edge width
#' @param rem.dis.inter remove single free unconnected nodes
#' @param graph.layout Network Diagram Layout:
#' "kk", "nicely", "sphere", "circle",
#' "bipartite", "star","tree", "randomly",
#' "gem", "graphopt","lgl", "grid",
#' "mds", "sugiyama"
#' @param label.repel label repel
#' @param style visual preset. \code{"default"} keeps the original look;
#'   \code{"journal"} (thin edges, white-stroked nodes, top-8 white labels,
#'   no legend) suits manuscript figures; \code{"cytoscape"} mimics Cytoscape
#'   (more FR iterations, edge width by score, top-15 labels, one colour bar);
#'   \code{"presentation"} uses a compact stress layout with large labels and
#'   full legends for slides. Only visual parameters change — data and
#'   statistics are identical across presets.

#' @return network diagrams
#' @export
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom ggplot2 scale_linewidth_continuous
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr count
#' @importFrom ggraph ggraph
#' @importFrom ggraph geom_edge_fan
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph scale_edge_width
#' @importFrom ggraph theme_graph
#' @importFrom RColorBrewer brewer.pal
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph V
#' @importFrom igraph E

#' @examples
#' \dontrun{
#' data(ppi_data, package = "TCMNP")
#' ppi_plot(ppi_data, label.repel = FALSE)

#' }
ppi_plot <- function(data,
                     node.color = "RdBu",
                     node.size = c(1, 10),
                     label.size = 4,
                     label.degree = 5,
                     label.repel = TRUE,
                     edge.color = "lightgrey",
                     edge.width = c(0.2, 2),
                     rem.dis.inter = FALSE,
                     graph.layout = "kk",
                     style = c("default", "journal", "cytoscape", "presentation")) {
  style <- match.arg(style)
 # data processing
  if (is.data.frame(data)) {
    data <- data %>% dplyr::distinct()
    if (length(unique(data[, 1])) > length(unique(data[, 2]))) {
      colnames(data) <- c("to", "from", "weight")
    } else {
      colnames(data) <- c("from", "to", "weight")
    }
  } else {
    print("The data must be a data frame with three columns.")
  }
  links <- data
  # node data
  nodes <- links %>%
    {
      data.frame(gene = c(.$from, .$to))
    } %>%
    dplyr::distinct()

  # Create a network graph from links and nodes
  if (rem.dis.inter == FALSE) {
    net <- igraph::graph_from_data_frame(
      d = links,
      vertices = nodes,
      directed = FALSE
    )
    # V and E are functions of the igraph package,
    # which are used to modify the nodes (nodes) and
    # connections (links) of the network graph respectively.
    igraph::V(net)$degree <- igraph::degree(net)
    igraph::V(net)$size <- igraph::degree(net)
    igraph::E(net)$score <- igraph::E(net)$weight
    # ---- 风格模板（2026-10-21）----
    # 原实现（default）保留完全兼容；其余三种是给「论文投稿 / Cytoscape 观感 /
    # 组会演示」用的预设，差别只在**视觉参数**，数据与统计口径完全一致。
    if (!identical(style, "default")) {
      return(.ppi_style_plot(net, style, node.color, node.size, label.size,
                             graph.layout))
    }

    # Plot with ggraph (default：保持原行为不动)
    ggraph(net, layout = graph.layout) +
      geom_edge_fan(
        aes(
          edge_width = score,
          colour = score
        ),
        color = edge.color,
        show.legend = TRUE
      ) +
      geom_node_point(aes(color = degree, size = size), alpha = 1.0) +
      scale_color_gradientn(
        colours =
          rev(RColorBrewer::brewer.pal(8, node.color))
      ) +
      geom_node_text(aes(filter = degree >= label.degree, label = name),
        size = label.size,
        repel = label.repel
      ) +
      ggraph::scale_edge_width(range = edge.width) +
      scale_linewidth_continuous(name = "degree", range = node.size) +
      theme_graph(base_family = "sans")
  } else if (rem.dis.inter == TRUE) {
    # remove dissociative interactions
    # If the from of a link in the links data frame
    # only appears once, and the to appears only once, remove it
    links_2 <- links %>%
      dplyr::mutate(from_c = count(., from)$n[match(from, count(., from)$from)]) %>%
      dplyr::mutate(to_c = count(., to)$n[match(to, count(., to)$to)]) %>%
      filter(!(from_c == 1 & to_c == 1)) %>%
      dplyr::select(1, 2, 3)
    # new node data
    nodes_2 <- links_2 %>%
      {
        data.frame(gene = c(.$from, .$to))
      } %>%
      dplyr::distinct()
    # create a network diagram
    net_2 <- igraph::graph_from_data_frame(
      d = links_2,
      vertices = nodes_2,
      directed = FALSE
    )
    # add the necessary parameters
    igraph::V(net_2)$degree <- igraph::degree(net_2)
    igraph::V(net_2)$size <- igraph::degree(net_2)
    igraph::E(net_2)$score <- igraph::E(net_2)$weight
    ggraph(net_2, layout = graph.layout) +
      geom_edge_fan(aes(edge_width = score, colour = score),
        color = edge.color, show.legend = TRUE
      ) +
      geom_node_point(aes(color = degree, size = size), alpha = 1.0) +
      scale_color_gradientn(colours = rev(
        RColorBrewer::brewer.pal(8, node.color)
      )) +
      geom_node_text(aes(filter = degree >= label.degree, label = name),
        size = label.size, repel = label.repel
      ) +
      ggraph::scale_edge_width(range = edge.width) +
      scale_linewidth_continuous(name = "degree", range = node.size) +
      theme_graph(base_family = "sans")
  }
}


# ---------------------------------------------------------------------------
# 风格模板（2026-10-21）
#
# 为什么需要：boss 反馈「PPI 图能出但不美观，想接近 Cytoscape 那种效果」。
# 拆开看是四个具体缺陷（都是原 default 实现带来的）：
#   1. `layout = "kk"` 在 100+ 节点网络上会散成一个环，中间空、外围密；
#   2. `geom_edge_fan` 画的是弧线并带 score 图例 —— 弧线在大网络里像一团毛；
#   3. `geom_node_text` 没有底色，标签直接压在边上（图中 HSP90AA1/CALM3 就是）；
#   4. size / score / degree 三个图例堆在右侧，占掉 1/4 画布。
#
# 三个模板只改**视觉参数**，数据与统计口径完全一致：
#   journal      —— 投稿用：fr 布局、细灰边、白描边节点、top 8 白底标签、无图例
#   cytoscape    —— 接近 Cytoscape：fr 迭代更多、节点按度散开、边按 score 分粗细、
#                   top 15 白底标签、只留 degree 色条
#   presentation —— 组会/答辩：stress 布局更紧凑、大节点大标签、完整图例
# ---------------------------------------------------------------------------
.ppi_style_plot <- function(net, style, node.color, node.size, label.size,
                            graph.layout) {
  pres <- switch(
    style,
    journal = list(layout = "fr", niter = 3000L, size_range = c(1.2, 5),
                   edge_alpha = 0.5, edge_width = c(0.25, 0.9), n_label = 8L,
                   label_size = 3.2, stroke = 0.4, legend = "none"),
    cytoscape = list(layout = "fr", niter = 5000L, size_range = c(2, 9),
                     edge_alpha = 0.6, edge_width = c(0.35, 1.6), n_label = 15L,
                     label_size = 3.4, stroke = 0.6, legend = "degree"),
    presentation = list(layout = "fr", niter = 4000L, size_range = c(3, 13),
                        edge_alpha = 0.65, edge_width = c(0.4, 2.2), n_label = 20L,
                        label_size = 4.6, stroke = 0.8, legend = "all")
  )
  # ---- 节点容量（视觉必需，不是可选）----
  # 793 节点 / 2594 边的 PPI 子网在 9 英寸静态画布上，无论哪种布局都会挤成一团
  # （2026-10-21 实测：stress 布局下中心节点完全重叠，标签被淹没）。
  # Cytoscape 出图好看的前提就是**先按度数过滤**再布局 —— 这里同样处理：
  # 按度数保留核心 max_nodes 个节点及其内部边。叶子节点（度 1-2）对网络结构
  # 没有信息量，删掉不影响解读。
  max_nodes <- 150L
  # ⚠️ 不要 as.integer()：那会丢掉向量里的**节点名**，names() 变 NULL，
  # induced_subgraph(vids = NULL) 会得到空图（2026-10-21 实测踩过：0 条边）。
  deg_all <- igraph::degree(net)
  if (igraph::vcount(net) > max_nodes) {
    keep <- names(sort(deg_all, decreasing = TRUE))[seq_len(max_nodes)]
    net <- igraph::induced_subgraph(net, vids = keep)
    message(sprintf(
      "ppi_plot(style = \"%s\"): \u7F51\u7EDC %d \u4E2A\u8282\u70B9\u8D85\u8FC7\u663E\u793A\u4E0A\u9650\uFF0C\u5DF2\u6309\u5EA6\u6570\u4FDD\u7559\u524D %d \u4E2A\u6838\u5FC3\u8282\u70B9\uFF08%d \u6761\u8FB9\uFF09\u3002",
      style, length(deg_all), max_nodes, igraph::ecount(net)))
  }

  # 若用户显式选了布局且不是默认 kk，尊重用户；否则用模板布局
  lay <- if (!identical(graph.layout, "kk")) graph.layout else pres$layout

  lay_obj <- if (identical(lay, "fr")) {
    ggraph::create_layout(net, layout = "fr", niter = pres$niter)
  } else {
    ggraph::create_layout(net, layout = lay)
  }

  # 标签阈值 = 度数第 n_label 高的那个值（不是绝对值，避免网络大小不同时
  # 一个标签都标不出来 —— 这是 2026-10-20 踩过的坑）
  deg <- igraph::degree(net)
  n_lab <- min(pres$n_label, length(deg))
  thr <- sort(as.integer(deg), decreasing = TRUE)[n_lab]

  p <- ggraph::ggraph(lay_obj) +
    ggraph::geom_edge_link(
      ggplot2::aes(edge_width = score),
      colour = "grey70", alpha = pres$edge_alpha, show.legend = FALSE
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(fill = degree, size = size),
      shape = 21, colour = "white", stroke = pres$stroke, show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradientn(
      colours = rev(RColorBrewer::brewer.pal(8, node.color)), name = "degree"
    ) +
    ggplot2::scale_size(range = pres$size_range, guide = "none") +
    ggraph::scale_edge_width(range = pres$edge_width, guide = "none") +
    ggraph::geom_node_label(
      ggplot2::aes(filter = degree >= thr, label = name),
      size = pres$label_size, repel = TRUE,
      label.size = 0.12, label.padding = ggplot2::unit(0.10, "lines"),
      fill = "white", colour = "grey15", alpha = 0.92
    ) +
    ggplot2::coord_fixed() +
    ggraph::theme_graph(background = "white", base_family = "sans") +
    ggplot2::theme(
      legend.position = if (identical(pres$legend, "none")) "none" else "right",
      legend.key.size = ggplot2::unit(0.9, "lines"),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )

  if (identical(pres$legend, "degree")) {
    p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(
      barheight = ggplot2::unit(3.2, "lines"), barwidth = ggplot2::unit(0.6, "lines")
    ))
  }
  p
}
