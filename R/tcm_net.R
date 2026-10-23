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
#' @param max_nodes 节点数上限。超过时按度数保留核心节点及其连边（`Inf` 表示
#'   不裁剪）。默认 150。
#' @param min_degree 度数下限：度数低于此值的节点（及其孤立边）在构图前剔除。
#'   默认 1（等价于「移除孤立节点」）。
#' @param keep 超限选点策略：\code{"top_degree"}（保留度数最高者）或
#'   \code{"ego"}（保留核心节点及其一跳邻居，子网更密）。
#' @param label.repel label repel
#' @param style visual preset, same names as \code{\link{ppi_plot}}:
#'   \code{"default"} keeps the original look (arc edges + three legends);
#'   \code{"journal"} / \code{"cytoscape"} / \code{"presentation"} draw a
#'   cleaner single-layer edge set with white-backed labels.
#' @param palette colour scheme key, see [tcm_palette_names()]. `NULL`
#'   (default) keeps the ColorBrewer colours named by \code{node.color};
#'   a palette key replaces that base colour set. Colours only — the node
#'   set, edges and layout are unaffected.
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
#' data("xfbdf", package = "tcmnetpharma")
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
                    rem.dis.inter = FALSE,
                    style = c("default", "journal", "cytoscape", "presentation"),
                    palette = NULL,
                    max_nodes = 150,
                    min_degree = 1,
                    keep = c("top_degree", "ego")) {
  style <- match.arg(style)
  keep <- match.arg(keep)
  palette <- .tcm_palette_check(palette)
  # 配色体系（2026-10-22）：统一成一组基准色，各分支（含风格模板）都从它插值，
  # 保证「同一张图换风格不改配色逻辑」。
  node_base <- if (is.null(palette)) RColorBrewer::brewer.pal(8, name = node.color) else
    .tcm_palette_ramp(palette, 8L)
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

  # ---- 统一节点容量控制（2026-10-23，P2）----
  # 原实现**没有任何容量控制**：节点数 = 全量 links 端点并集，数百/上千节点
  # 时力导向布局糊成一团、标签互相压（boss 反馈「药物-靶点图爆炸」）。
  # 现接入统一管线 tcm_filter_network（边规则先行 → 节点=边端点 → 过滤后算度数
  # → 超限 induced subgraph），节点/边口径与 ppi_plot、拓扑页一致（单一事实源）。
  # max_nodes = Inf 可回退为「不裁剪」（仅供调试/全量导出）。
  flt <- tcm_filter_network(links, max_nodes = max_nodes,
                            min_degree = min_degree, keep = keep)
  links <- flt$links
  rm(flt)
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
    col <- node_base
    colorN <- grDevices::colorRampPalette(colors = col)(nrow(nodes))
    names(colorN) <- nodes$node

    if (!identical(style, "default")) {
      return(.tcm_net_style_plot(net, style, node.color, node.size, label.size,
                                 graph.layout, node_base))
    }

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
    col <- node_base
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


# ---------------------------------------------------------------------------
# 三层网络（药材-成分-靶点）风格模板（2026-10-21）
#
# 原实现（default）的三个具体缺陷，boss 的截图里都能看到：
#   1. **画了两层边**：`geom_edge_link0`（直线，按 weight）+ `geom_edge_fan`
#      （弧线，按 score）—— 同一批边画两遍，视觉上糊成一团；
#   2. **三个图例**（degree 色条 / size / edge width）挤在右侧，吃掉 1/4 画布；
#   3. 标签用 `geom_node_text`，没有底色，长分子名直接横穿图面。
#
# 注意：三层网络**天然是放射状**（每个成分挂几十个靶点，靶点度数为 1，
# 布局算法必然把它们推到外圈）。模板只能改善"干净程度"，改变不了骨相。
# 想要密集的靶点网络，应该走「靶点 → PPI 子网 → ppi_plot」那条路。
# ---------------------------------------------------------------------------
.tcm_net_style_plot <- function(net, style, node.color, node.size, label.size,
                                graph.layout, node_base = NULL) {
  if (is.null(node_base)) node_base <- RColorBrewer::brewer.pal(8, name = node.color)
  pres <- switch(
    style,
    journal = list(layout = "fr", niter = 3000L, edge_alpha = 0.45,
                   edge_width = c(0.2, 0.9), n_label = 10L, label_size = 3.0,
                   legend = "none"),
    cytoscape = list(layout = "fr", niter = 4000L, edge_alpha = 0.55,
                     edge_width = c(0.3, 1.4), n_label = 15L, label_size = 3.3,
                     legend = "class"),
    presentation = list(layout = "fr", niter = 3500L, edge_alpha = 0.6,
                        edge_width = c(0.4, 1.8), n_label = 20L, label_size = 4.0,
                        legend = "all")
  )
  lay <- if (!identical(graph.layout, "kk")) graph.layout else pres$layout

  old_seed <- if (exists(".Random.seed", envir = globalenv()))
    get(".Random.seed", envir = globalenv()) else NULL
  set.seed(20261021L)
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv()))
        rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  lay_obj <- ggraph::create_layout(net, layout = lay, niter = pres$niter)

  deg <- igraph::degree(net)
  n_lab <- min(pres$n_label, length(deg))
  thr <- if (n_lab > 0L) sort(as.integer(deg), decreasing = TRUE)[n_lab] else 0L

  col <- node_base
  colorN <- grDevices::colorRampPalette(colors = col)(max(2L, igraph::vcount(net)))

  # 形状区分三类节点（herb / molecule / target）—— 这是三层网络唯一有信息量的
  # 视觉编码，任何模板都保留。
  p <- ggraph::ggraph(lay_obj) +
    ggraph::geom_edge_link0(
      ggplot2::aes(edge_linewidth = weight),
      colour = "grey70", alpha = pres$edge_alpha, show.legend = FALSE
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(colour = degree, size = degree, shape = class),
      alpha = 1, show.legend = c(size = FALSE, colour = FALSE, shape = TRUE)
    ) +
    ggplot2::scale_colour_gradientn(colours = rev(colorN)) +
    ggraph::scale_edge_width(range = pres$edge_width, guide = "none") +
    ggplot2::scale_size_continuous(range = node.size, guide = "none") +
    (if (n_lab > 0L) ggraph::geom_node_label(
      # 长 IUPAC 分子名（如 5,7-dihydroxy-3-(4-methoxyphenyl)-8-...）会把
      # 版面横向劈成两半，截到 24 字符。完整名可导出边表查看。
      ggplot2::aes(filter = degree >= thr,
                   label = ifelse(nchar(name) > 24,
                                  paste0(substr(name, 1, 23), "\u2026"), name)),
      size = pres$label_size, repel = TRUE,
      label.size = 0.12, label.padding = ggplot2::unit(0.10, "lines"),
      fill = "white", colour = "grey15", alpha = 0.92
    ) else NULL) +
    ggraph::theme_graph(background = "white", base_family = "sans") +
    ggplot2::theme(
      legend.position = if (identical(pres$legend, "none")) "none" else "right",
      legend.key.size = ggplot2::unit(0.9, "lines"),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )

  if (identical(pres$legend, "class")) {
    p <- p + ggplot2::guides(shape = ggplot2::guide_legend(
      title = "type", override.aes = list(size = 3.2)))
  }
  p
}
