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
#'     \item{`"hub"`}{只留 30 个核心节点并\strong{全部}带标签（摘要图用）；
#'       该 30 是\emph{预设默认值}，可被显式 `max_nodes` 覆盖}
#'   }
#' @param palette colour scheme key, see [tcm_palette_names()]. `NULL`
#'   (default) keeps the ColorBrewer palette named by \code{node.color};
#'   a palette key replaces that gradient. Colours only — the node set,
#'   layout and statistics are identical.
#' @param max_nodes 节点数上限。网络超过该值时，按度数保留最高的若干节点
#'   （及其边），走共享管线 `tcm_filter_network()`；`Inf` 关闭裁剪。
#'   \strong{参数优先级：显式传入值 优先于 `view` 预设 优先于 函数默认 150}。
#'   `view = "hub"` 自带的 30 只是预设默认值；显式传值总会生效 ——
#'   `ppi_plot(..., view = "hub", max_nodes = 1000)` 的上限是 1000，不是 30。
#' @param min_degree \strong{严格度数下限}：`degree < min_degree` 的节点整体
#'   剔除（连同其边）。与 `max_nodes` \strong{正交}，\emph{任何} `max_nodes`
#'   取值下都生效 —— 包括 `max_nodes = Inf` / 大于节点数（「不截断、只按度数
#'   筛」正是本参数的主用法）。默认 0 = 不过滤。
#'   被剔除的节点数会以 message 报出（不静默减点）；下限高于全图最大度数时
#'   直接报错。
#'   过滤后「自身够度数、但邻居全被筛光」的节点会被一并剔除（只清一次，不级联），
#'   所以图上不会留孤零零一个点。
#' @param min_degree_iterate 是否把 `min_degree` 迭代到稳定（k-core，2026-10-28 新增）。
#'   默认 `FALSE` = 单次过滤（与 Cytoscape 的 Degree Filter 同口径）。
#'   `TRUE` 会**级联**：**食管炎实测 113 → 27 个节点（10 轮，−76%）**，
#'   因为度≥6 的节点之间大量靠低度节点搭桥。除非明确要 k-core 骨架，否则不要开。
#' @param keep selection strategy when capping: `"top_degree"` keeps the
#'   highest-degree nodes; `"ego"` keeps them plus their one-hop neighbours
#'   (denser subnet, more nodes). See `tcm_filter_network()`.
#' @param legend 图例档位（2026-10-28 新增）。`NULL`（默认）= 跟随 `style`
#'   预设；显式传值覆盖：
#'   \describe{
#'     \item{`"auto"`}{只出\strong{颜色}图例（一份色条）—— 发表图默认}
#'     \item{`"all"`}{颜色 + \strong{节点大小}两份图例（size 图例标题
#'       `degree (size)`，避免与色条同名）}
#'     \item{`"none"`}{全关}
#'   }
#'   注意：`style = "presentation"` 自 2026-10-28 起默认 `"auto"`（原本是
#'   `"all"`），需要老的"两份图例"请显式 `legend = "all"`。
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
#' | `"hub30"` | `view="hub"` + `style="presentation"` | 30 节点仍全标；\strong{会多出 1 份颜色图例}（旧版无）。要无图例可再传 `style="journal"` |
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
                     min_degree_iterate = FALSE,
                     keep = c("top_degree", "ego"),
                     legend = NULL) {
  # ⚠️ `match.arg()` 的实参必须是变量名，不能是表达式（铁律 80）
  style_in <- as.character(style)[1L]
  view_in <- as.character(view)[1L]
  keep <- match.arg(keep)
  # legend 用 NULL 作哨兵（铁律 99）：`NULL` = 跟随 style 预设，显式值才覆盖。
  # 不能写 `legend = c("auto","all","none")` 当默认 —— 那会让 journal/minimal
  # 的「无图例」被默认值 "auto" 顶掉。
  if (!is.null(legend)) {
    legend <- match.arg(legend, c("auto", "all", "none"))
  }
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
  # ⚠️ 参数优先级（2026-10-28 修 D3）：**用户显式值 > view 预设 > 函数默认**。
  #    旧实现无条件用 `vp$max_nodes` 覆盖 max_nodes，导致
  #    `ppi_plot(..., view = "hub", max_nodes = 1000)` 仍是 30 个节点，
  #    只在 message 里轻描淡写地说「超过上限」——用户传的参数被静默丢弃。
  #    `missing()` 才是「没传 vs 传了」的判据（不能用 == 默认值判断：
  #    用户显式传 150 与不传，语义不同）。
  user_cap <- if (missing(max_nodes)) NULL else max_nodes
  vp <- .ppi_view_presets()[[view_in]]
  cap_nodes <- if (!is.null(user_cap)) {
    user_cap
  } else if (!is.null(vp$max_nodes)) {
    vp$max_nodes
  } else {
    150
  }
  net <- .ppi_cap_nodes(net, cap_nodes, min_degree,
                        min_degree_iterate = min_degree_iterate,
                        keep = keep, style_label = style_in)

  .ppi_plot_core(net, view = view_in, style = style_in,
                 node.size = node.size, label.size = label.size,
                 label.degree = label.degree, label.repel = label.repel,
                 edge.color = edge.color, edge.width = edge.width,
                 graph.layout = graph.layout, node_base = node_base,
                 palette = palette, legend = legend)
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
                        # 2026-10-28：由 "all" 改 "auto"。发表图只要一份色条；
                        # size 图例（`"all"`）改为**显式** `ppi_plot(legend = "all")`
                        # 才出 —— 默认两幅同名 "degree" 图例读者分不清。
                        legend = "auto"),
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
# ---------------------------------------------------------------------------
# 渲染核心：**薄适配器**（2026-10-26 第二轮）
#
# 第一轮把 ppi_plot 内部三份同构绘制代码收敛成一份 `.ppi_plot_core()`，
# 但它仍是**独立渲染器**：`net-style-layer.R` 里还有另一份。两套渲染器的
# 代价是真实的 —— 上一轮修的 `niter` 写死、`label.size` 静默失效、图例档位
# 失效，都是「同一个坑要在两个地方各修一次」。
#
# 本轮给共享层补了三项能力，ppi 不再需要自己的渲染器：
#   ① `node_style = "ringed"`（fill + 白描边，Cytoscape 观感）
#   ② `edge_width_by`（按边属性定宽 —— PPI 的 combined_score 是真实统计量）
#   ③ `colour_by = "degree"`（连续 degree 作为 colour/fill 通道）
# 外加 `label_top` / `legend` / `niter` / `palette_colours` 四个外观旋钮，
# 让「ppi 的 style 表」仍由 ppi 自己拥有，共享层只负责画。
#
# 语义归属：ppi 的 5 个 style 名与它们的 layout/niter/size/edge/label/legend
# **仍然只此一份**（`.ppi_style_presets()`），共享层不认识 ppi 的档位名。
# ---------------------------------------------------------------------------
.ppi_plot_core <- function(net, view, style, node.size, label.size,
                           label.degree, label.repel, edge.color, edge.width,
                           graph.layout, node_base, palette, legend = NULL) {
  vp <- .ppi_view_presets()[[view]]
  pres <- .ppi_style_presets()[[style]]
  if (is.null(pres)) pres <- .ppi_style_presets()[["default"]]

  # 用户显式给的外观参数优先于 style 预设（旧版对非 default 风格静默无效）
  size_range <- if (is.null(node.size)) pres$size_range else node.size
  lab_size <- if (is.null(label.size)) pres$label_size else label.size
  ew <- if (is.null(edge.width)) pres$edge_width else edge.width

  # ---- 标签条数 -----------------------------------------------------------
  # 优先级：**style 的 0 条** > 显式 `label_degree` > view 全标 > style 的 N。
  # ⚠️ 「0 条」必须最高：`minimal` 的语义就是"不要标签"，不该被 `view="hub"`
  #    的全标推翻（2026-10-26 第一轮已定，合并进共享层时差点丢掉）。
  #    `label_degree` 由共享层内部判优先级（阈值最高），这里不用管。
  lab_top <- if (identical(as.integer(pres$n_label), 0L)) {
    0
  } else if (isTRUE(vp$label_all)) {
    Inf
  } else {
    as.numeric(pres$n_label)
  }

  # ---- 边宽：有 score 就按 score 映射，否则固定线宽 ------------------------
  has_score <- !is.null(igraph::E(net)$score) &&
    any(!is.na(igraph::E(net)$score))

  # ---- 名义色板：ppi 的 module 视图沿用 ColorBrewer Set2 -------------------
  mod_colours <- NULL
  if (identical(vp$colour, "module")) {
    n_mod <- nlevels(factor(igraph::membership(
      igraph::cluster_walktrap(net)
    )))
    mod_colours <- if (is.null(palette)) {
      RColorBrewer::brewer.pal(8, "Set2")
    } else {
      suppressMessages(tcm_palette(palette, max(2L, n_mod), "discrete"))
    }
  }

  # 布局："auto" = 用 style 的推荐布局；只有力导向布局才带 niter
  # （共享层会按报错文本回退，但能提前判掉就不该让它报一次错）
  lay <- if (identical(graph.layout, "auto")) pres$layout else graph.layout
  nit <- if (identical(lay, "fr")) pres$niter else NULL

  .tcm_net_plot(
    net, view = "overview", style = "default",
    node.size = size_range, label.size = lab_size,
    graph.layout = lay,
    node_base = node_base,
    label_degree = label.degree, label_repel = label.repel,
    label_top = lab_top,
    colour_by = vp$colour,
    palette_colours = mod_colours,
    node_style = "ringed", node_stroke = pres$stroke,
    edge_color = edge.color,
    edge_width_by = if (has_score) "score" else NULL,
    edge_width_range = ew,
    edge_alpha = pres$edge_alpha,
    edge_lw = if (has_score) NULL else 0.45,
    legend = .ppi_legend_mode(legend, pres$legend),
    niter = nit
  )
}

#' 解析图例档位：显式值 > style 预设
#'
#' 三态语义（与 `.tcm_net_plot()` 一致）：`"auto"` 只出颜色图例、
#' `"all"` 颜色 + size 两份、`"none"` 全关。
#' @noRd
.ppi_legend_mode <- function(user, style_value) {
  v <- if (is.null(user)) style_value else user
  if (is.null(v) || !nzchar(as.character(v)[1L])) return("auto")
  v <- as.character(v)[1L]
  if (identical(v, "none")) "none" else if (identical(v, "all")) "all" else "auto"
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
                           min_degree_iterate = FALSE,
                           keep = "top_degree", style_label = "") {
  capt <- .tcm_net_cap(net, view = "overview", max_nodes = max_nodes,
                       min_degree = min_degree,
                       min_degree_iterate = min_degree_iterate, keep = keep)
  sfx <- if (nzchar(style_label)) paste0(" (style = \"", style_label, "\")") else ""
  if (isTRUE(capt$report$capped)) {
    # ⚠️ 报**实际画出来的**节点数（report$shown_nodes），不是 max_nodes：
    #    度数打平时被保留的节点里可能有「在子集内没有连线」的，induced subgraph
    #    之后会少掉几个。旧 message 一律说「前 N 个」，于是报告与图对不上
    #    （2026-10-28 实测：上限 30、图上 29）。
    shown <- capt$report$shown_nodes
    cap_used <- capt$report$cap_used
    message(sprintf(
      "ppi_plot%s: %d \u4e2a\u8282\u70b9\u8d85\u8fc7\u663e\u793a\u4e0a\u9650\uff08%d\uff09\uff0c\u5df2\u6309\u5ea6\u6570\u4fdd\u7559 %d \u4e2a\u6838\u5fc3\u8282\u70b9\uff08%d \u6761\u8fb9\uff09%s\u3002",
      sfx, capt$report$total_nodes, as.integer(cap_used), shown,
      capt$report$shown_edges,
      if (shown < as.integer(cap_used)) {
        sprintf("\uff1b%d \u4e2a\u8282\u70b9\u5728\u4fdd\u7559\u5b50\u96c6\u5185\u65e0\u8fde\u7ebf\u5df2\u5254\u9664",
                as.integer(cap_used) - shown)
      } else ""
    ))
  } else if (isTRUE(capt$report$floored)) {
    # 只做了度数下限过滤（未触发容量上限）时也必须说一声：
    # 否则用户传了 min_degree 却拿到少数节点，无从得知是被谁筛掉的。
    message(.tcm_floor_msg(paste0("ppi_plot", sfx), min_degree, capt$report))
  }
  out <- capt$net
  # 按度数保留的节点之间恰好没有连线（典型：多个小分量各有一个高 degree 核心，
  # 上限又只够留下它们的核心）→ 图为空。必须明说，否则用户拿到一张空白图。
  if (isTRUE(capt$report$capped) && igraph::vcount(out) == 0L) {
    stop(sprintf(
      paste0("ppi_plot%s: max_nodes = %d \u592a\u5c0f\uff0c\u6309\u5ea6\u6570\u4fdd\u7559\u7684\u8282\u70b9",
             "\u4e4b\u95f4\u6ca1\u6709\u8fde\u7ebf\uff0c\u56fe\u4e3a\u7a7a\u3002\n",
             "  \u8bf7\u8c03\u5927 max_nodes\uff0c\u6216\u6539\u7528 keep = \"ego\"",
             "\uff08\u4fdd\u7559\u4e00\u8df3\u90bb\u5c45\uff0c\u5b50\u7f51\u66f4\u8fde\uff09\u3002"),
      sfx, as.integer(capt$report$cap_used)
    ), call. = FALSE)
  }
  # 下限把整张图筛空（典型：星型图取 min_degree=2，2-core 为空）→ 同样是
  # 空白图，同样必须明说，并指明是下限而不是上限造成的。
  if (!isTRUE(capt$report$capped) && igraph::vcount(out) == 0L) {
    stop(sprintf(
      paste0("ppi_plot%s: min_degree = %s \u628a\u6240\u6709\u8282\u70b9\u90fd\u7b5b\u6389\u4e86",
             "\uff08\u5269\u4e0b\u7684\u8282\u70b9\u4e4b\u95f4\u6ca1\u6709\u8fde\u7ebf\uff09\u3002\n",
             "  \u8bf7\u964d\u4f4e min_degree\uff08\u5f53\u524d\u5165\u53c2\u5171 %d \u4e2a\u8282\u70b9\uff09%s\u3002"),
      sfx, format(min_degree), as.integer(capt$report$total_nodes),
      if (isTRUE(min_degree_iterate)) "\uff0c\u6216\u5173\u95ed min_degree_iterate" else ""
    ), call. = FALSE)
  }
  # degree / size 必须按**裁剪后的子图**重算（铁律：拓扑相关属性不能沿用全图）
  igraph::V(out)$degree <- igraph::degree(out)
  igraph::V(out)$size <- igraph::degree(out)
  out
}
