#' Protein-protein interaction analysis
#'
#' @param data Human protein-protein interaction (PPI) data downloaded from the
#'   STRING database. 三列（node1 / node2 / combined_score）或两列（无分数，
#'   退化为固定线宽）。
#' @param node.color node color: see "RColorBrewer::display.brewer.all()"。
#'   \strong{仅在 `view` 按度数着色（连续色阶）时生效}。
#' @param node.size 节点大小范围（长度 2）。`NULL`（默认）= 由 `style` 决定；
#'   显式传入则覆盖 style。
#' @param label.size 标签字号。`NULL`（默认）= 由 `style` 决定。
#' @param label.degree 标签的\strong{绝对度数阈值}（度数 ≥ 该值的节点出标签）。
#'   默认 `NULL` = 由 `view` / `style` 决定。旧版默认 `5`。
#' @param label.repel 标签是否 repel
#' @param edge.color 边颜色
#' @param edge.width 边线宽范围（长度 2）。`NULL`（默认）= 由 `style` 决定。
#' @param rem.dis.inter 去掉「两端都只出现一次」的游离互作（数据层过滤）
#' @param graph.layout 布局名。"auto"（默认）= 由 `style` 决定；
#'   显式给名字则覆盖 style。
#' @param style \strong{外观轴}（与 `view` 正交）：
#'   `"default"` / `"journal"` / `"cytoscape"` / `"presentation"` / `"minimal"`。
#'   只改视觉参数，不改数据与统计口径。为向后兼容仍接受旧名
#'   `"hub30"` / `"module"`（会被拆成对应的 `view` × `style` 组合并给出提示，
#'   见下方“旧名映射”）。
#' @param view \strong{数据/形状轴}（与 `style` 正交）：
#'   \describe{
#'     \item{`"degree"`}{按度数连续着色（默认）}
#'     \item{`"module"`}{按 walktrap 社区着色 —— 看模块结构，而不是单个基因}
#'     \item{`"hub"`}{只留 30 个核心节点并\strong{全部}带标签（摘要图用）}
#'   }
#' @param palette colour scheme key, see [tcm_palette_names()]. `NULL`
#'   (default) keeps the ColorBrewer palette named by \code{node.color};
#'   a palette key replaces that gradient. Colours only — the node set,
#'   layout and statistics are identical.
#' @param max_nodes node count upper bound. When the network exceeds this,
#'   the highest-degree nodes (and their edges) are kept via the shared
#'   `tcm_filter_network` pipeline. `Inf` disables capping.
#' @param min_degree degree floor applied by `tcm_filter_network` (default 0
#'   keeps everything up to `max_nodes`; raise to drop low-degree fringe nodes
#'   first). Only matters when `max_nodes` is finite.
#' @param keep selection strategy when capping: `"top_degree"` keeps the
#'   highest-degree nodes; `"ego"` keeps them plus their one-hop neighbours
#'   (denser subnet, more nodes). See `tcm_filter_network()`.
#'
#' @section 旧名映射（`style` 的向后兼容）:
#' 旧版把「选哪些节点 / 颜色表什么」（数据轴）与「长什么样」（外观轴）塞进了
#' 一个 7 值枚举。现已拆成 `view` × `style` 两个正交轴（3 × 5 = 15 种组合）。
#'
#' 其中 5 个旧名本身就是新的 `style` 名，配 `view = "degree"` 即旧行为、无需转换：
#' `"default"` / `"journal"` / `"cytoscape"` / `"presentation"` / `"minimal"`。
#' 只有下面两个旧名跨越了两个轴，会被自动拆分并给出提示：
#'
#' | 旧 `style` | 等价于 | 与旧行为的差异 |
#' |---|---|---|
#' | `"hub30"` | `view="hub"` + `style="presentation"` | 30 节点仍全标；\strong{会多出图例}（旧版无）。要无图例可再传 `style="journal"` |
#' | `"module"` | `view="module"` + `style="default"` | 仍按社区着色；标签数 10 → 8、节点大小范围 2–10 → 1–10 |
#'
#' @return network diagrams
#' @export
#' @importFrom ggplot2 scale_color_gradientn
#' @importFrom ggplot2 scale_size_continuous
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

#' @examples
#' \dontrun{
#' data(ppi_data, package = "tcmnetpharma")
#' ppi_plot(ppi_data, label.repel = FALSE)
#' ppi_plot(ppi_data, view = "module", style = "journal")
#' ppi_plot(ppi_data, view = "hub", style = "presentation")
#' }
ppi_plot <- function(data,
                     node.color = "RdBu",
                     node.size = NULL,
                     label.size = NULL,
                     label.degree = NULL,
                     label.repel = TRUE,
                     edge.color = "lightgrey",
                     edge.width = NULL,
                     rem.dis.inter = FALSE,
                     graph.layout = "auto",
                     style = c("default", "journal", "cytoscape", "presentation",
                               "minimal", "hub30", "module"),
                     view = c("degree", "module", "hub"),
                     palette = NULL,
                     max_nodes = 150,
                     min_degree = 0,
                     keep = c("top_degree", "ego")) {
  # ⚠️ `match.arg()` 的实参必须是变量名，不能是表达式（铁律 80）
  style_in <- as.character(style)[1L]
  view_in <- as.character(view)[1L]
  keep <- match.arg(keep)
  palette <- .tcm_palette_check(palette)

  # ---- 0. 两轴解析：旧名自动拆成 view × style ---------------------------
  # 旧版 7 值枚举把「选点/着色」和「外观」混在一起（铁律：一个枚举不该承载
  # 两个正交维度）。这里把 legacy 名映射成组合，老调用零改动仍然可用。
  legacy <- .ppi_legacy_style(style_in)
  if (!is.null(legacy)) {
    if (missing(view)) {
      view_in <- legacy$view
    }
    message(sprintf(
      "ppi_plot: style = \"%s\" \u5df2\u62c6\u4e3a\u4e24\u4e2a\u6b63\u4ea4\u8f74\uff0c\u7b49\u4ef7\u4e8e view = \"%s\" + style = \"%s\"\u3002\n  \u8bf7\u6539\u7528\u65b0\u5199\u6cd5\uff1appi_plot(..., view = \"%s\", style = \"%s\")",
      style_in, view_in, legacy$style, view_in, legacy$style))
    style_in <- legacy$style
  }
  if (is.null(legacy) && !style_in %in% .ppi_styles()) {
    stop(sprintf(
      "style \u5fc5\u987b\u662f\u4ee5\u4e0b\u4e4b\u4e00\uff1a%s\n  \uff08\u65e7\u540d\uff1a%s\uff09",
      paste(.ppi_styles(), collapse = ", "),
      paste(names(.ppi_legacy_map()), collapse = ", ")
    ), call. = FALSE)
  }
  if (!view_in %in% .ppi_views()) {
    stop(sprintf("view \u5fc5\u987b\u662f\u4ee5\u4e0b\u4e4b\u4e00\uff1a%s",
                 paste(.ppi_views(), collapse = ", ")), call. = FALSE)
  }

  node_base <- if (is.null(palette)) {
    rev(RColorBrewer::brewer.pal(8, node.color))
  } else {
    .tcm_palette_ramp(palette, 8L)
  }

  # ---- 1. 数据标准化 -----------------------------------------------------
  if (!is.data.frame(data)) {
    stop(sprintf(
      "`data` \u5fc5\u987b\u662f data.frame\uff08\u4e24\u5217\u6216\u4e09\u5217\uff09\uff0c\u5b9e\u9645\u4e3a\uff1a%s",
      paste(class(data), collapse = " / ")
    ), call. = FALSE)
  }
  links <- data %>% dplyr::distinct()
  if (ncol(links) < 2L) {
    stop("\u8bc1\u636e\u4e0d\u8db3\uff1a`data` \u81f3\u5c11\u9700\u8981\u4e24\u5217\uff08\u4e24\u4e2a\u4e92\u4f5c\u86cb\u767d\uff09\u3002",
         call. = FALSE)
  }
  # 列角色：前三列依次视为 node1 / node2 / score。
  # ⚠️ 旧实现用 `length(unique(data[,1])) > length(unique(data[,2]))` 猜方向，
  #    该判据在 tibble 与 data.frame 上取值不同（1 列 tibble 的 length() 是
  #    列数）—— venn_net 的同类缺陷已实测证伪（见 venn_net.R 注释）。
  #    这里方向对无向图**不影响结果**，只需保证列名固定，故直接按位置命名。
  if (ncol(links) == 2L) links$weight <- NA_real_
  links <- links[, 1:3, drop = FALSE]
  colnames(links) <- c("from", "to", "weight")

  # ---- 2. 数据层过滤：去掉游离互作 --------------------------------------
  if (isTRUE(rem.dis.inter)) {
    links <- .ppi_drop_isolated(links)
    if (!nrow(links)) {
      stop("\u53bb\u6389\u6e38\u79bb\u4e92\u4f5c\u540e\u6ca1\u6709\u5269\u4e0b\u4efb\u4f55\u8fb9\u3002", call. = FALSE)
    }
  }

  net <- igraph::graph_from_data_frame(links, directed = FALSE)
  igraph::V(net)$degree <- igraph::degree(net)
  igraph::V(net)$size <- igraph::degree(net)
  igraph::E(net)$score <- igraph::E(net)$weight

  # ---- 3. view 自带的选点意图 -------------------------------------------
  vp <- .ppi_view_presets()[[view_in]]
  cap_nodes <- if (!is.null(vp$max_nodes)) vp$max_nodes else max_nodes
  net <- .ppi_cap_nodes(net, cap_nodes, min_degree, keep, style_in)

  .ppi_plot_core(net, view = view_in, style = style_in,
                 node.size = node.size, label.size = label.size,
                 label.degree = label.degree, label.repel = label.repel,
                 edge.color = edge.color, edge.width = edge.width,
                 graph.layout = graph.layout, node_base = node_base,
                 palette = palette)
}


# ---------------------------------------------------------------------------
# 两个正交轴：view（数据/形状）× style（外观）
#
# 为什么拆：旧版是 7 值大枚举，其中
#   · hub30 偷偷改了 max_nodes（数据轴 —— 选哪些节点）
#   · module 偷偷改了 colour 通道（数据轴 —— 颜色表什么）
#   · 其余 5 个只改外观（外观轴）
# 于是「module + 大字号」「hub + 无图例」这类组合无法表达。拆开后 3 × 5 = 15 种。
# ---------------------------------------------------------------------------

#' view 的合法取值
#' @noRd
.ppi_views <- function() c("degree", "module", "hub")

#' view 预设（数据/形状轴）
#'
#' 每个 view 只决定两件事：**颜色表什么**、**选多少节点**。
#' 标签策略由 view 的 `label_all` 与 style 的 `n_label` 共同决定（见核心函数）。
#' @noRd
.ppi_view_presets <- function() {
  list(
    # 按度数连续着色：PPI 的常规读法（哪个蛋白是枢纽）
    degree = list(colour = "degree", max_nodes = NULL, label_all = FALSE),
    # 按 walktrap 社区着色：看模块结构，而不是单个基因
    module = list(colour = "module", max_nodes = NULL, label_all = FALSE),
    # 只留 30 个核心节点且全部带标签：期刊 Graphical Abstract / 摘要图
    hub = list(colour = "degree", max_nodes = 30L, label_all = TRUE)
  )
}

#' style 的合法取值
#' @noRd
.ppi_styles <- function() {
  c("default", "journal", "cytoscape", "presentation", "minimal")
}

#' style 预设（外观轴）
#'
#' 只含视觉参数，**不含**任何选点 / 着色通道的决定（那是 view 的职责）。
#' `n_label` 是「默认标几条标签」，会被显式的 `label.degree` 覆盖。
#' @noRd
.ppi_style_presets <- function() {
  list(
    default      = list(layout = "kk", niter = NULL, size_range = c(1, 10),
                        edge_alpha = 0.50, edge_width = c(0.2, 2.0),
                        n_label = 8L, label_size = 4.0, stroke = 0.5,
                        legend = "degree"),
    journal      = list(layout = "fr", niter = 3000L, size_range = c(1.2, 5),
                        edge_alpha = 0.50, edge_width = c(0.25, 0.9),
                        n_label = 8L, label_size = 3.2, stroke = 0.4,
                        legend = "none"),
    cytoscape    = list(layout = "fr", niter = 5000L, size_range = c(2, 9),
                        edge_alpha = 0.60, edge_width = c(0.35, 1.6),
                        n_label = 15L, label_size = 3.4, stroke = 0.6,
                        legend = "degree"),
    presentation = list(layout = "fr", niter = 4000L, size_range = c(3, 13),
                        edge_alpha = 0.65, edge_width = c(0.4, 2.2),
                        n_label = 20L, label_size = 4.6, stroke = 0.8,
                        legend = "all"),
    minimal      = list(layout = "fr", niter = 3000L, size_range = c(1.5, 6),
                        edge_alpha = 0.35, edge_width = c(0.2, 0.8),
                        n_label = 0L, label_size = 3.0, stroke = 0.3,
                        legend = "none")
  )
}

#' 旧 7 值 style 名 → view × style 映射表
#' @noRd
.ppi_legacy_map <- function() {
  # ⚠️ 只收「旧名与新模型不等价」的两个：
  #    default / journal / cytoscape / presentation / minimal 本身就是新的
  #    style 名（配 view="degree" 即旧行为），放进来会让每次默认调用都打提示。
  list(
    # hub30 的数据意图（30 节点 + 全标）属于 view=hub；
    # 外观取 presentation（size_range c(3,13) 与旧 hub30 一致）
    hub30  = list(view = "hub", style = "presentation"),
    # module 的着色意图属于 view=module；外观回落到 default
    module = list(view = "module", style = "default")
  )
}

#' 查旧名（非旧名返回 NULL）
#' @noRd
.ppi_legacy_style <- function(style) {
  m <- .ppi_legacy_map()
  if (is.character(style) && length(style) == 1L && style %in% names(m)) {
    m[[style]]
  } else {
    NULL
  }
}


# ---------------------------------------------------------------------------
# 渲染核心：**ppi_plot 只有这一份绘制实现**
#
# 旧实现有三份基本同构的绘制代码（`default` 内联一份、`rem.dis.inter=TRUE`
# 内联又一份、`.ppi_style_plot` 一份）—— 改一处必漏两处（铁律 54）。
# ---------------------------------------------------------------------------
.ppi_plot_core <- function(net, view, style, node.size, label.size,
                           label.degree, label.repel, edge.color, edge.width,
                           graph.layout, node_base, palette) {
  vp <- .ppi_view_presets()[[view]]
  pres <- .ppi_style_presets()[[style]]
  if (is.null(pres)) pres <- .ppi_style_presets()[["default"]]

  # 用户显式给的外观参数优先于 style 预设
  size_range <- if (is.null(node.size)) pres$size_range else node.size
  lab_size <- if (is.null(label.size)) pres$label_size else label.size
  ew <- if (is.null(edge.width)) pres$edge_width else edge.width

  # ---- 着色通道（由 view 决定，只此一个通道）-----------------------------
  if (identical(vp$colour, "module")) {
    igraph::V(net)$module <- factor(
      igraph::membership(igraph::cluster_walktrap(net))
    )
    n_mod <- nlevels(igraph::V(net)$module)
    fill_aes <- ggplot2::aes(fill = module, size = size)
    fill_scale <- if (is.null(palette)) {
      ggplot2::scale_fill_brewer(palette = "Set2", name = "module")
    } else {
      ggplot2::scale_fill_manual(
        values = tcm_palette(palette, max(2L, n_mod)), name = "module"
      )
    }
  } else {
    fill_aes <- ggplot2::aes(fill = degree, size = size)
    fill_scale <- ggplot2::scale_fill_gradientn(
      colours = node_base, name = "degree"
    )
  }

  # ---- 布局（固定种子，可复现）-----------------------------------------
  lay <- if (identical(graph.layout, "auto")) pres$layout else graph.layout
  old_seed <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  set.seed(20261021L)
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv())) {
        rm(".Random.seed", envir = globalenv())
      }
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  # ⚠️ niter 只对力导向布局有意义（fr / graphopt）。写死会废掉 kk/circle/grid
  #    等布局（2026-10-25 实测 10 种布局仅 fr / graphopt 通过）。
  lay_obj <- if (!is.null(pres$niter) && identical(lay, "fr")) {
    ggraph::create_layout(net, layout = lay, niter = pres$niter)
  } else {
    ggraph::create_layout(net, layout = lay)
  }

  # ---- 标签策略 ---------------------------------------------------------
  # 优先级：显式绝对阈值 > view 的「全标」> style 的默认条数
  # ⚠️ 「前 N 个」必须按**名字集合**取，不能按度数阈值取：度数打平时
  #    （PPI 里大量节点 degree 相同）阈值会把同分节点全放进来（铁律 97）。
  deg <- igraph::V(net)$degree
  vname <- igraph::V(net)$name
  # 优先级（从高到低）：style 显式「0 条」> 显式绝对阈值 > view 的全标 > style 默认条数。
  # 「0 条」必须最高：`minimal` 的语义就是"不要标签"，不该被 view=hub 的全标推翻。
  lab_layer <- if (identical(as.integer(pres$n_label), 0L) &&
                  is.null(label.degree)) {
    NULL
  } else if (!is.null(label.degree) && length(label.degree) &&
             is.finite(as.numeric(label.degree)[1L])) {
    ggraph::geom_node_label(
      ggplot2::aes(filter = degree >= as.numeric(label.degree)[1L],
                   label = .tcm_net_trunc(name)),
      size = lab_size, repel = isTRUE(label.repel),
      linewidth = 0.12, label.padding = ggplot2::unit(0.10, "lines"),
      fill = "white", colour = "grey15", alpha = 0.92
    )
  } else if (isTRUE(vp$label_all)) {
    ggraph::geom_node_label(
      ggplot2::aes(label = .tcm_net_trunc(name)),
      size = lab_size, repel = isTRUE(label.repel),
      linewidth = 0.12, label.padding = ggplot2::unit(0.10, "lines"),
      fill = "white", colour = "grey15", alpha = 0.92
    )
  } else {
    n_lab <- max(1L, min(as.integer(pres$n_label), length(deg)))
    ord <- order(-as.numeric(deg), vname)
    keep_names <- vname[ord[seq_len(n_lab)]]
    ggraph::geom_node_label(
      ggplot2::aes(filter = name %in% keep_names,
                   label = .tcm_net_trunc(name)),
      size = lab_size, repel = isTRUE(label.repel),
      linewidth = 0.12, label.padding = ggplot2::unit(0.10, "lines"),
      fill = "white", colour = "grey15", alpha = 0.92
    )
  }

  # ---- 边：单层，宽度按 score（有分数时）--------------------------------
  # 边宽兜底：边表若没有 combined_score（或全 NA），`aes(edge_width = score)`
  # 会映射成 NA → **整片边消失**，只剩一堆散点。没有分数就用固定线宽。
  has_score <- !is.null(igraph::E(net)$score) &&
    any(!is.na(igraph::E(net)$score))
  edge_layer <- if (has_score) {
    ggraph::geom_edge_link(
      ggplot2::aes(edge_width = score),
      colour = edge.color, alpha = pres$edge_alpha, show.legend = FALSE
    )
  } else {
    ggraph::geom_edge_link(
      colour = edge.color, alpha = pres$edge_alpha, linewidth = 0.45,
      show.legend = FALSE
    )
  }

  show_node_lg <- !identical(pres$legend, "none")
  p <- ggraph::ggraph(lay_obj) +
    edge_layer +
    ggraph::geom_node_point(
      fill_aes, shape = 21, colour = "white", stroke = pres$stroke,
      show.legend = show_node_lg
    ) +
    fill_scale +
    ggplot2::scale_size(
      range = size_range, name = "degree",
      guide = if (identical(pres$legend, "all")) "legend" else "none"
    ) +
    ggraph::scale_edge_width(range = ew, guide = "none") +
    lab_layer +
    ggplot2::coord_fixed() +
    ggraph::theme_graph(background = "white", base_family = "sans") +
    ggplot2::theme(
      legend.position = if (identical(pres$legend, "none")) "none" else "right",
      legend.key.size = ggplot2::unit(0.9, "lines"),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )

  if (identical(pres$legend, "degree") && !identical(vp$colour, "module")) {
    p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(
      barheight = ggplot2::unit(3.2, "lines"),
      barwidth = ggplot2::unit(0.6, "lines")
    ))
  }
  p
}


# ---------------------------------------------------------------------------
# 数据层过滤：去掉「两端都只出现一次」的游离互作
# ---------------------------------------------------------------------------
.ppi_drop_isolated <- function(links) {
  n_from <- table(links$from)
  n_to <- table(links$to)
  fc <- as.integer(n_from[links$from])
  tc <- as.integer(n_to[links$to])
  links[!(fc == 1L & tc == 1L), , drop = FALSE]
}


# ---------------------------------------------------------------------------
# 节点容量：**所有 view × style 共用这一份实现**
#
# 为什么必须统一：default 曾被"保持原始行为"放行全量出图，结果 737 节点在 kk
# 布局下散成「中心一团 + 外围孤点」，用户直接判定为「PPI 结果有问题」。
# 可读性上限不是风格选择，是所有风格共同的前提。
#
# ⚠️ 2026-10-26 修 P0：旧实现调 `.tcm_filter_igraph` 后**没有重算顶点属性**。
#   该函数从边表重建图 → `degree` / `size` 全部丢失 → 下游 `aes(size = size)`
#   报 `object 'size' not found`，**任何 max_nodes 小于节点数的调用都无法渲染**
#   （实测 `style="hub30"` 100% 崩；ppi_data 72 节点，只有 max_nodes=150 的
#   默认值因为「不触发裁剪」而侥幸正常）。同一份图 `E(net)$score` 也会丢，
#   让「边宽按 score」静默退化成固定线宽。
# ---------------------------------------------------------------------------
.ppi_cap_nodes <- function(net, max_nodes = 150L, min_degree = 0L,
                           keep = "top_degree", style_label = "") {
  capt <- .tcm_net_cap(net, view = "overview", max_nodes = max_nodes,
                       min_degree = min_degree, keep = keep)
  if (isTRUE(capt$report$capped)) {
    message(sprintf(
      "ppi_plot%s: %d \u4e2a\u8282\u70b9\u8d85\u8fc7\u663e\u793a\u4e0a\u9650\uff0c\u5df2\u6309\u5ea6\u6570\u4fdd\u7559\u524d %d \u4e2a\u6838\u5fc3\u8282\u70b9\uff08%d \u6761\u8fb9\uff09\u3002",
      if (nzchar(style_label)) paste0(" (style = \"", style_label, "\")") else "",
      capt$report$total_nodes, as.integer(max_nodes),
      capt$report$shown_edges))
  }
  out <- capt$net
  # degree / size 必须按**裁剪后的子图**重算（铁律：拓扑相关属性不能沿用全图）
  igraph::V(out)$degree <- igraph::degree(out)
  igraph::V(out)$size <- igraph::degree(out)
  out
}
