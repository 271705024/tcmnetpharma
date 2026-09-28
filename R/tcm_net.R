#' Network interaction diagram of herbs, ingredients, and targets
#'
#' @param network.data data frame
#' must contain herb, molecule, target three columns of data
#' @param node.color node color
#' see "RColorBrewer::display.brewer.all()"
#' @param node.size  node size
#' @param label.size label size
#' @param label.degree
#' the node degree is the number of connections that
#' the node has with the other nodes.
#' Nodes with connections greater than or
#' equal to degree will be displayed.
#' @param edge.color edge color
#' @param edge.width edge width
#' @param graph.layout etwork Diagram Layout:
#' @param graph.layout Network Diagram Layout:
#' "kk", "nicely", "circle", "sphere",
#' "bipartite", "star", "tree", "randomly",
#' "gem", "graphopt","lgl", "grid",
#' "mds", "sugiyama","fr"
#' @param rem.dis.inter remove single free unconnected nodes
#' @param label.repel label repel
#'
#' @return Network Diagram
#' @export
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 aes
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr count
#' @importFrom ggraph ggraph
#' @importFrom ggraph geom_edge_fan
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph scale_edge_width
#' @importFrom ggraph geom_edge_link0
#' @importFrom ggraph theme_graph
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph graph_from_data_frame
#' @examples
#' \dontrun{
#' data("xfbdf", package = "TCMNP")
#' network.data <- xfbdf %>%
#'   dplyr::select(herb, molecule, target) %>%
#'   dplyr::slice_sample(n = 100) %>%
#'   as.data.frame()
#' tcm_net(network.data, node.color= "Spectral",
#' label.degree = 1, rem.dis.inter = TRUE,graph.layout = "fr",
#' label.size = 3)
#' }
tcm_net <- function(network.data,
                    node.color = "RdBu",
                    node.size = c(2, 8),
                    label.size = 4,
                    label.degree = 3,
                    label.repel = TRUE,
                    edge.color = "lightgrey",
                    edge.width = c(0.2, 2),
                    graph.layout = "kk",
                    rem.dis.inter = FALSE) {
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
  #       ② class 按**节点名**逐个判定（长度恒等于顶点数），角色优先级 herb > molecule > target。
  need_cols <- c("herb", "molecule", "target")
  if (!all(need_cols %in% names(network.data))) {
    stop(
      "`network.data` \u5fc5\u987b\u5305\u542b\u5217\uff1a",
      paste(need_cols, collapse = ", "),
      "\uff1b\u5f53\u524d\u5217\u4e3a\uff1a", paste(names(network.data), collapse = ", ")
    )
  }
  network.data <- network.data[, need_cols, drop = FALSE]

  # 按节点名给 class（恒与顶点数等长；一个名字同时出现在多列时取最先匹配的角色）
  node_class <- function(vnames, nd) {
    out <- rep(NA_character_, length(vnames))
    out[vnames %in% as.character(nd[[3]])] <- colnames(nd)[3]
    out[vnames %in% as.character(nd[[2]])] <- colnames(nd)[2]
    out[vnames %in% as.character(nd[[1]])] <- colnames(nd)[1]
    out[is.na(out)] <- "other"
    out
  }

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
  # Create a network graph from links and nodes
  if (rem.dis.inter == FALSE) {
    nodes <- links %>%
      {
        data.frame(node = c(.$from, .$to))
      } %>%
      dplyr::distinct()
    net <- igraph::graph_from_data_frame(
      d = links,
      vertices = nodes,
      directed = FALSE
    )
    # V and E are functions of the igraph package,
    # which are used to modify the nodes (nodes) and
    # connections (links) of the network graph respectively.
    igraph::V(net)$degree <- igraph::degree(net)
    igraph::V(net)$class <- node_class(igraph::V(net)$name, network.data)
    igraph::V(net)$size <- igraph::degree(net)
    igraph::E(net)$score <- igraph::E(net)$weight

    # Plot with ggraph
    # ggraph is a package based on ggplot2,
    # the syntax is similar to regular ggplot2
    col <- RColorBrewer::brewer.pal(8, name = node.color)
    colorN <- grDevices::colorRampPalette(colors = col)(nrow(nodes))
    names(colorN) <- nodes$node

    ggraph(net, layout = graph.layout) +
      geom_edge_link0(aes(edge_linewidth = weight), edge_colour = edge.color) +
      geom_edge_fan(aes(edge_width = score, colour = score),
        color = edge.color, show.legend = TRUE
      ) +
      ggraph::geom_node_point(aes(
        color = degree,
        size = degree,
        shape = class
      ), alpha = 1.0) +
      scale_color_gradientn(colours = rev(colorN)) +
      geom_node_text(aes(
        filter = degree >= label.degree,
        label = name
      ), size = label.size, repel = label.repel) +
      scale_edge_width(range = edge.width) +
      scale_size_continuous(name = "degree", range = node.size) +
      ggraph::theme_graph(base_family = "sans")
  } else if (rem.dis.inter == TRUE) {
    links <- links %>%
      dplyr::mutate(from_c = count(., from)$n[match(from, count(., from)$from)]) %>%
      dplyr::mutate(to_c = count(., to)$n[match(to, count(., to)$to)]) %>%
      filter(!(from_c == 1 & to_c == 1)) %>%
      dplyr::select(1, 2, 3)
    nodes <- links %>%
      {
        data.frame(node = c(.$from, .$to))
      } %>%
      dplyr::distinct()
    col <- RColorBrewer::brewer.pal(8, name = node.color)
    colorN <- grDevices::colorRampPalette(colors = col)(nrow(nodes))
    names(colorN) <- nodes$node

    # Create a network graph from links and nodes
    net <- igraph::graph_from_data_frame(
      d = links,
      vertices = nodes,
      directed = FALSE
    )
    # V and E are functions of the igraph package,
    # which are used to modify the nodes (nodes) and
    # connections (links) of the network graph respectively.
    igraph::V(net)$degree <- igraph::degree(net)
    igraph::V(net)$class <- node_class(igraph::V(net)$name, network.data)
    igraph::V(net)$size <- igraph::degree(net)
    igraph::E(net)$score <- igraph::E(net)$weight

    # Plot with ggraph
    ggraph(net, layout = graph.layout) +
      geom_edge_link0(aes(edge_linewidth = weight),
        edge_colour = edge.color
      ) +
      geom_edge_fan(aes(edge_width = score, colour = score),
        color = edge.color, show.legend = TRUE
      ) +
      ggraph::geom_node_point(aes(
        color = degree, size = degree,
        shape = class
      ), alpha = 1.0) +
      scale_color_gradientn(colours = rev(colorN)) +
      geom_node_text(
        aes(
          filter = degree >= label.degree,
          label = name
        ),
        size = label.size,
        repel = label.repel
      ) +
      scale_edge_width(range = edge.width) +
      scale_size_continuous(
        name = "degree",
        range = node.size
      ) +
      ggraph::theme_graph(base_family = "sans")
  }
}
