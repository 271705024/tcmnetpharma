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
                     style = c("default", "journal", "cytoscape", "presentation",
                            "hub30", "module", "minimal")) {
  style <- match.arg(style)
 # data processing
  if (is.data.frame(data)) {
    data <- data %>% dplyr::distinct()
    # 只有两列（node1/node2，没有 combined_score）也要能画：
    # 补一列 weight = NA，后续 has_score 兜底会走固定线宽。
    # （2026-10-21 实测：2 列表直接进 `colnames(data) <- c("from","to","weight")`
    #   会报 'names' attribute [3] must be the same length as the vector [2]）
    if (ncol(data) == 2L) {
      data$weight <- NA_real_
    }
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
    # 原实现（default）保留输出形态；其余预设是给「论文投稿 / Cytoscape 观感 /
    # 组会演示」用的，差别只在**视觉参数**，数据与统计口径完全一致。
    if (!identical(style, "default")) {
      return(.ppi_style_plot(net, style, node.color, node.size, label.size,
                             graph.layout))
    }

    # ⚠️ default 也要做节点筛选（2026-10-21 补）：
    # 之前 default 是全量出图，737 节点在 kk 布局下必然散成"中心一团 + 外围孤点"，
    # 用户看到的就是「PPI 结果有问题」。可读性上限对所有风格都成立，
    # 不该因为"保持原始行为"就放行。
    net <- .ppi_cap_nodes(net, 150L, style)

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
                        label_size = 4.6, stroke = 0.8, legend = "all"),
    # 只留 30 个核心节点且全部带标签 —— 期刊 Graphical Abstract / 摘要图用
    hub30 = list(layout = "fr", niter = 3000L, size_range = c(3, 13),
                 edge_alpha = 0.6, edge_width = c(0.35, 1.4), n_label = 30L,
                 label_size = 4.2, stroke = 0.7, legend = "none", max_nodes = 30L),
    # 按社区（walktrap）着色 —— 看模块结构，而不是看单个基因
    module = list(layout = "fr", niter = 4000L, size_range = c(2, 10),
                  edge_alpha = 0.45, edge_width = c(0.25, 1.0), n_label = 10L,
                  label_size = 3.3, stroke = 0.6, legend = "none",
                  by_module = TRUE),
    # 极简：只有点和线，无标签无图例 —— 大图缩略 / 补充材料
    minimal = list(layout = "fr", niter = 3000L, size_range = c(1.5, 6),
                   edge_alpha = 0.35, edge_width = c(0.2, 0.8), n_label = 0L,
                   label_size = 3, stroke = 0.3, legend = "none")
  )
  # ---- 节点容量（视觉必需，不是可选）----
  # 793 节点 / 2594 边的 PPI 子网在 9 英寸静态画布上，无论哪种布局都会挤成一团
  # （2026-10-21 实测：stress 布局下中心节点完全重叠，标签被淹没）。
  # Cytoscape 出图好看的前提就是**先按度数过滤**再布局 —— 这里同样处理：
  # 按度数保留核心 max_nodes 个节点及其内部边。叶子节点（度 1-2）对网络结构
  # 没有信息量，删掉不影响解读。
  max_nodes <- if (!is.null(pres$max_nodes)) as.integer(pres$max_nodes) else 150L
  net <- .ppi_cap_nodes(net, max_nodes, style)

  # module 预设：walktrap 社区检测后按模块着色（看模块结构而不是单个基因）
  if (isTRUE(pres$by_module)) {
    cl <- igraph::cluster_walktrap(net)
    igraph::V(net)$module <- factor(igraph::membership(cl))
  }

  # 若用户显式选了布局且不是默认 kk，尊重用户；否则用模板布局
  lay <- if (!identical(graph.layout, "kk")) graph.layout else pres$layout

  # fr 布局的初始位置是随机的：同一份数据、同样的代码，两次出图可能一次密集
  # 一次散开（2026-10-21 boss 那张"孤点图"即由此而来，而同一数据在我这里却是
  # 密集网络）。固定种子保证**可复现** —— 科研出图必须能重跑。
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

  fill_aes <- if (isTRUE(pres$by_module)) {
    ggplot2::aes(fill = module, size = size)
  } else {
    ggplot2::aes(fill = degree, size = size)
  }
  fill_scale <- if (isTRUE(pres$by_module)) {
    ggplot2::scale_fill_brewer(palette = "Set2", name = "module")
  } else {
    ggplot2::scale_fill_gradientn(
      colours = rev(RColorBrewer::brewer.pal(8, node.color)), name = "degree"
    )
  }

  # 边宽兜底：边表若没有 combined_score（或全 NA），aes(edge_width = score)
  # 会映射成 NA -> **整片边消失**，只剩一堆散点 —— 这是「PPI 结果看起来没边」
  # 的第二种可能成因。没有分数就用固定线宽，至少保证网络结构可见。
  has_score <- !is.null(igraph::E(net)$score) && any(!is.na(igraph::E(net)$score))
  edge_layer <- if (has_score) {
    ggraph::geom_edge_link(
      ggplot2::aes(edge_width = score),
      colour = "grey70", alpha = pres$edge_alpha, show.legend = FALSE
    )
  } else {
    ggraph::geom_edge_link(
      colour = "grey70", alpha = pres$edge_alpha, linewidth = 0.45,
      show.legend = FALSE
    )
  }

  p <- ggraph::ggraph(lay_obj) +
    edge_layer +
    ggraph::geom_node_point(
      fill_aes,
      shape = 21, colour = "white", stroke = pres$stroke, show.legend = FALSE
    ) +
    fill_scale +
    ggplot2::scale_size(range = pres$size_range, guide = "none") +
    ggraph::scale_edge_width(range = pres$edge_width, guide = "none") +
    (if (pres$n_label > 0L) ggraph::geom_node_label(
      ggplot2::aes(filter = degree >= thr,
                   label = ifelse(nchar(name) > 24,
                                  paste0(substr(name, 1, 23), "\u2026"), name)),
      size = pres$label_size, repel = TRUE,
      label.size = 0.12, label.padding = ggplot2::unit(0.10, "lines"),
      fill = "white", colour = "grey15", alpha = 0.92
    ) else NULL) +
    ggplot2::coord_fixed() +
    ggraph::theme_graph(background = "white", base_family = "sans") +
    ggplot2::theme(
      legend.position = if (identical(pres$legend, "none")) "none" else "right",
      legend.key.size = ggplot2::unit(0.9, "lines"),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )

  if (identical(pres$legend, "degree") && !isTRUE(pres$by_module)) {
    p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(
      barheight = ggplot2::unit(3.2, "lines"), barwidth = ggplot2::unit(0.6, "lines")
    ))
  }
  p
}


# ---------------------------------------------------------------------------
# 节点容量：**default 与所有预设共用这一份实现**（2026-10-21）
#
# 为什么必须统一：default 曾被"保持原始行为"放行全量出图，结果 737 节点在 kk
# 布局下散成「中心一团 + 外围孤点」，用户直接判定为「PPI 结果有问题」。
# 可读性上限不是风格选择，是所有风格共同的前提。
#
# ⚠️ 不要写成 `as.integer(igraph::degree(net))`：那会丢掉向量里的节点名，
# `names()` 变 NULL，`induced_subgraph(vids = NULL)` 得到**空图（0 条边）**，
# 而日志还照样打印"已保留 N 个节点"（2026-10-21 踩过）。
# ---------------------------------------------------------------------------
.ppi_cap_nodes <- function(net, max_nodes = 150L, style_label = "") {
  if (!is.finite(max_nodes) || igraph::vcount(net) <= max_nodes) return(net)
  deg <- igraph::degree(net)
  keep <- names(sort(deg, decreasing = TRUE))[seq_len(as.integer(max_nodes))]
  out <- igraph::induced_subgraph(net, vids = keep)
  message(sprintf(
    "ppi_plot%s: %d \u4e2a\u8282\u70b9\u8d85\u8fc7\u663e\u793a\u4e0a\u9650\uff0c\u5df2\u6309\u5ea6\u6570\u4fdd\u7559\u524d %d \u4e2a\u6838\u5fc3\u8282\u70b9\uff08%d \u6761\u8fb9\uff09\u3002",
    if (nzchar(style_label)) paste0(" (style = \"", style_label, "\")") else "",
    igraph::vcount(net), as.integer(max_nodes), igraph::ecount(out)))
  out
}
