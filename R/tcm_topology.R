#' 拓扑中心性分析（degree / betweenness / closeness / coreness）
#'
#' 从 herb-molecule-target 三元组\strong{或二列边表}构建无向图，计算各节点的
#' 度中心性、介数中心性（betweenness）、接近中心性（closeness）与 k-core 数
#' （coreness）。四者是网络药理学"核心靶点"识别的标准拓扑指标（对应 CytoNCA
#' 的 Degree / BC / CC，以及 MCODE 的 k-Core）。
#'
#' @param network.data 数据框。`input = "triple"`（默认）时必须含
#'   `herb` / `molecule` / `target` 三列（与 [tcm_net()] 同构）；`input = "edge"`
#'   时是\strong{边表}，两端列按 `from`/`to`、`node1`/`node2`、`protein1`/`protein2`、
#'   `gene1`/`gene2` 的别名顺序精确匹配（[tcm_ppi()] 的输出可直接喂进来）。
#' @param class 仅返回该类型节点：`"herb"` / `"molecule"` / `"target"`；
#'   默认 NULL = 返回全部。`input = "edge"` 时无角色信息，传它会报错。
#' @param rem.dis.inter 是否移除无连接的孤立节点（同 [tcm_net()]）
#' @param top_n 仅返回中心性最高的前 N 个节点（按 betweenness 降序）；NULL = 全部
#' @param normalized `betweenness` 是否归一化到 `[0, 1]`。默认 `FALSE` =
#'   igraph 默认的\strong{原始介数值}（与 CytoNCA 的 BC 口径一致，便于跨工具
#'   对照；旧版本一直如此，改成 TRUE 会让所有既有数值缩小
#'   `(n-1)(n-2)/2` 倍）。需要与 `igraph::betweenness(normalized = TRUE)`
#'   对齐时显式传 `TRUE`。
#' @param input 输入形态（2026-10-28 新增）：
#'   \describe{
#'     \item{`"triple"`}{三层网络（herb / molecule / target），默认，行为不变}
#'     \item{`"edge"`}{二列边表（如 PPI）。直接用 `graph_from_data_frame()` 构图，
#'       跳过三层网络的节点角色判定；`class` 列全为 `NA`。
#'       \strong{边权不参与拓扑计算}（与 CytoNCA 默认的无权口径一致），
#'       所以 `combined_score` 只影响画图线宽，不影响中心性排序}
#'   }
#'
#' @return 数据框：`node` / `class` / `degree` / `betweenness` / `closeness` /
#'   `coreness`，按 `betweenness` 降序（核心靶点排最前）。
#'   `betweenness` 与 [igraph::betweenness()] 同口径（默认\strong{不归一化}，
#'   见 `normalized` 参数）。
#' @export
#'
#' @seealso [tcm_ppi()]（PPI 边表来源）、[tcm_modules()]、[ppi_plot()]
#'
#' @importFrom igraph graph_from_data_frame V degree betweenness closeness
#' @importFrom dplyr select rename distinct
#'
#' @examples
#' \dontrun{
#' data("network_data", package = "tcmnetpharma")
#' tcm_centrality(network_data)
#' tcm_centrality(network_data, class = "target", top_n = 10)
#'
#' # PPI 边表 → 中心性（按 betweenness 取 hub 基因）
#' edges <- tcm_ppi(tcm_intersect_target("麻黄", "Asthma")$target_symbol)
#' tcm_centrality(edges, input = "edge", top_n = 20)
#' }
tcm_centrality <- function(network.data,
                           class = NULL,
                           rem.dis.inter = FALSE,
                           top_n = NULL,
                           input = c("triple", "edge"),
                           normalized = FALSE) {
  input <- match.arg(input)
  network.data <- as.data.frame(network.data)
  if (nrow(network.data) == 0L) {
    return(data.frame(
      node = character(), class = character(),
      degree = numeric(), betweenness = numeric(), closeness = numeric(),
      coreness = numeric(), stringsAsFactors = FALSE
    ))
  }
  # ⚠️ `class` 过滤要在构图前判合法性：edge 模式下没有角色信息，
  #    静默返回 0 行（或报 "should be one of"）都无法让用户明白是模式选错了。
  if (!is.null(class)) {
    if (identical(input, "edge")) {
      stop(paste0(
        "input = \"edge\" \u7684\u8fb9\u8868\u6ca1\u6709 herb / molecule / target ",
        "\u89d2\u8272\u4fe1\u606f\uff0c`class` \u8fc7\u6ee4\u4e0d\u53ef\u7528\u3002\n",
        "  \u82e5\u8981\u6309\u8282\u70b9\u7c7b\u578b\u7b5b\u9009\uff0c\u8bf7\u7528\u4e09\u5c42\u7f51\u7edc",
        "\uff08input = \"triple\"\uff0c\u9700 herb/molecule/target \u4e09\u5217\uff09\u3002"
      ), call. = FALSE)
    }
    class <- match.arg(class, c("herb", "molecule", "target"))
  }
  net <- .tcm_topology_graph(network.data, input = input,
                             rem.dis.inter = rem.dis.inter,
                             fn = "tcm_centrality")
  df <- data.frame(
    node = igraph::V(net)$name,
    class = igraph::V(net)$class,
    degree = igraph::degree(net),
    betweenness = igraph::betweenness(net, normalized = isTRUE(normalized)),
    closeness = igraph::closeness(net),
    coreness = igraph::coreness(net),
    stringsAsFactors = FALSE
  )
  if (!is.null(class)) {
    df <- df[df$class == class, , drop = FALSE]
  }
  df <- df[order(df$betweenness, decreasing = TRUE), , drop = FALSE]
  if (!is.null(top_n) && is.numeric(top_n) && top_n > 0) df <- utils::head(df, top_n)
  rownames(df) <- NULL
  df
}

#' 拓扑模块识别（社区发现 + MCODE 风格密度评分）
#'
#' 在 herb-molecule-target 网络上做模块（功能团）识别：
#' - **社区发现**：igraph `cluster_louvain`（默认）/ `cluster_infomap` / `cluster_walktrap`，
#'   全离线、零新依赖；
#' - **MCODE 风格评分**：对每个节点计算邻域密度（`mcode_score`，∈`[0,1]`）与 k-core
#'   数（`coreness`），沿用 Cytoscape MCODE 论文术语（dense module / k-Core），
#'   但复刻于纯 R，无需 Cytoscape，离线 exe 可直接用。
#'
#' @param network.data 数据框。`input = "triple"`（默认）时须含
#'   `herb` / `molecule` / `target`；`input = "edge"` 时为边表
#'   （口径同 [tcm_centrality()]，[tcm_ppi()] 输出可直接用）
#' @param algorithm 社区发现算法：`"louvain"`（默认）/ `"infomap"` / `"walktrap"`
#' @param rem.dis.inter 是否移除孤立节点
#' @param mcode_k 仅保留 k-core 数 ≥ `mcode_k` 的节点参与模块汇总（默认 2，
#'   过滤掉松散单点；设 0 表示不按 k-core 过滤）
#' @param input 输入形态：`"triple"`（三层网络，默认）或 `"edge"`（二列边表，
#'   如 PPI）。见 [tcm_centrality()] 的 `input` 参数。
#'
#' @return 长度为 2 的列表：
#'   - `nodes`：数据框 `node` / `class` / `module` / `degree` / `betweenness` /
#'     `closeness` / `coreness` / `mcode_score`（`input = "edge"` 时 `class` 全为 `NA`）
#'   - `modules`：数据框 `module` / `size` / `hub` / `top_targets` / `density` /
#'     `mean_mcode`（按 `size` 降序）
#' @export
#'
#' @seealso [tcm_centrality()]、[tcm_ppi()]
#'
#' @importFrom igraph graph_from_data_frame V degree betweenness closeness
#'   coreness cluster_louvain cluster_infomap cluster_walktrap induced_subgraph
#'   neighbors vcount ecount membership
#' @importFrom dplyr select rename distinct
#'
#' @examples
#' \dontrun{
#' data("network_data", package = "tcmnetpharma")
#' res <- tcm_modules(network_data)
#' head(res$nodes); head(res$modules)
#'
#' # PPI 边表同样适用（class 列全 NA）
#' tcm_modules(tcm_ppi(hit$target_symbol), input = "edge")
#' }
tcm_modules <- function(network.data,
                        algorithm = c("louvain", "infomap", "walktrap"),
                        rem.dis.inter = FALSE,
                        mcode_k = 2,
                        input = c("triple", "edge")) {
  algorithm <- match.arg(algorithm)
  input <- match.arg(input)
  network.data <- as.data.frame(network.data)
  empty_nodes <- data.frame(
    node = character(), class = character(), module = integer(),
    degree = numeric(), betweenness = numeric(), closeness = numeric(),
    coreness = integer(), mcode_score = numeric(), stringsAsFactors = FALSE
  )
  empty_mods <- data.frame(
    module = integer(), size = integer(), hub = character(),
    top_targets = character(), density = numeric(), mean_mcode = numeric(),
    stringsAsFactors = FALSE
  )
  if (nrow(network.data) == 0L) {
    return(list(nodes = empty_nodes, modules = empty_mods))
  }
  net <- .tcm_topology_graph(network.data, input = input,
                             rem.dis.inter = rem.dis.inter,
                             fn = "tcm_modules")
  comm <- switch(algorithm,
    louvain = igraph::cluster_louvain(net),
    infomap = igraph::cluster_infomap(net),
    walktrap = igraph::cluster_walktrap(net)
  )
  memb <- as.integer(igraph::membership(comm))
  core <- igraph::coreness(net)
  mscore <- .mcode_score(net)
  nodes <- data.frame(
    node = igraph::V(net)$name,
    class = igraph::V(net)$class,
    module = memb,
    degree = igraph::degree(net),
    betweenness = igraph::betweenness(net),
    closeness = igraph::closeness(net),
    coreness = core,
    mcode_score = mscore,
    stringsAsFactors = FALSE
  )

  # --- 模块汇总（按 size 降序）---
  mod_ids <- sort(unique(memb))
  rows <- lapply(mod_ids, function(m) {
    idx <- which(memb == m)
    sub <- nodes[idx, ]
    hub <- sub$node[which.max(sub$degree)]
    # ⚠️ `input = "edge"` 时 class 全为 NA：`sub$class == "target"` 得到 NA，
    #    `sub[NA, ]` 会返回一整块 NA 行（nrow > 0）→ top_targets 变成 "NA"。
    #    必须先排除 NA 再比较。
    tgt <- if (all(is.na(sub$class))) {
      sub[0, , drop = FALSE]
    } else {
      sub[!is.na(sub$class) & sub$class == "target", , drop = FALSE]
    }
    top_t <- if (nrow(tgt) > 0) {
      paste(utils::head(tgt$node[order(tgt$degree, decreasing = TRUE)], 3),
        collapse = ";"
      )
    } else ""
    sg <- igraph::induced_subgraph(net, idx)
    nv <- igraph::vcount(sg)
    dens <- igraph::ecount(sg) / max(1, nv * (nv - 1) / 2)
    data.frame(
      module = m, size = length(idx), hub = hub, top_targets = top_t,
      density = round(dens, 3), mean_mcode = round(mean(sub$mcode_score), 3),
      stringsAsFactors = FALSE
    )
  })
  mods <- do.call(rbind, rows)
  if (nrow(mods) > 0) mods <- mods[order(mods$size, decreasing = TRUE), ]
  rownames(mods) <- NULL

  # --- 按 k-core 过滤（仅影响模块汇总的"显著性"标注，不改节点表）---
  if (is.numeric(mcode_k) && mcode_k > 0) {
    nodes$pass_kcore <- nodes$coreness >= mcode_k
  } else {
    nodes$pass_kcore <- TRUE
  }
  list(nodes = nodes, modules = mods)
}

#' 模块规模柱状图
#'
#' @param modules [tcm_modules()] 返回的 `modules` 数据框（含 `module` / `size`）
#' @param title 标题
#' @return ggplot 对象
#' @export
#'
#' @importFrom ggplot2 ggplot geom_bar aes theme_bw xlab ylab ggtitle theme
#'   element_text
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' data("network_data", package = "tcmnetpharma")
#' module_barplot(tcm_modules(network_data)$modules)
#' }
module_barplot <- function(modules, title = "module size") {
  modules <- as.data.frame(modules)
  modules <- dplyr::mutate(modules, module = factor(module))
  ggplot2::ggplot(modules, ggplot2::aes(x = module, y = size, fill = module)) +
    ggplot2::geom_bar(stat = "identity", width = 0.8) +
    ggplot2::scale_fill_discrete(guide = "none") +
    ggplot2::theme_bw() +
    ggplot2::xlab("module") +
    ggplot2::ylab("size (nodes)") +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 11, colour = "black"),
      plot.title = ggplot2::element_text(size = 13)
    )
}

#' 模块着色的网络图
#'
#' 构建 herb-molecule-target 网络并按所属模块着色，直观展示功能团分布。
#'
#' 视觉实现走 `R/net-style-layer.R` 的统一层（与 `tcm_net(view = "cluster")`
#' **同一份代码**）。旧实现自带一套绘制：没有标签、没有图例、模块用 RdBu 连续
#' 渐变（把无序类别画成有序色阶）、布局不固定种子（同一份数据两次出图不同形）。
#'
#' @param network.data 数据框，必须含 `herb` / `molecule` / `target` 三列
#' @param algorithm 社区发现算法（同 [tcm_modules()]）
#' @param rem.dis.inter 是否移除孤立节点
#' @param graph.layout ggraph 布局，默认 `"fr"`；`"auto"` 也走 fr（与 style 一致）
#' @param max_nodes 节点数上限（`Inf` 不裁剪），默认 150
#' @param min_degree 度数下限，默认 1（移除孤立节点）。此处由
#'   \code{tcm_filter_network()} 的\strong{边规则}实现（两端都低于下限才剔边），
#'   与 \code{tcm_net()} / \code{ppi_plot()} 的「严格节点下限」口径不同：
#'   这里保留悬在核心上的叶子，避免模块图被剪成孤岛。
#' @param keep 超限选点策略：`"top_degree"` / `"ego"`
#' @param node.size 节点大小范围（长度 2）
#' @param label.size 簇名标签字号
#' @param style 外观预设（同 [tcm_net()]）：`default` / `journal` /
#'   `cytoscape` / `presentation`
#' @param palette 配色体系 key（见 [tcm_palette_names()]）；`NULL` 时用 npg
#' @return ggplot 对象
#' @export
#'
#' @importFrom igraph graph_from_data_frame V degree cluster_louvain
#'   cluster_infomap cluster_walktrap membership
#' @importFrom ggraph ggraph geom_edge_link0 geom_node_point theme_graph
#'   scale_edge_width
#' @importFrom ggplot2 scale_color_manual
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr select rename distinct
#'
#' @examples
#' \dontrun{
#' data("network_data", package = "tcmnetpharma")
#' module_netplot(network_data)
#' module_netplot(network_data, algorithm = "louvain", style = "journal")
#' }
module_netplot <- function(network.data,
                           algorithm = c("louvain", "infomap", "walktrap"),
                           rem.dis.inter = FALSE,
                           graph.layout = "fr",
                           max_nodes = 150,
                           min_degree = 1,
                           keep = c("top_degree", "ego"),
                           node.size = c(2, 8),
                           label.size = 3.4,
                           style = c("default", "journal", "cytoscape",
                                     "presentation"),
                           palette = NULL) {
  algorithm <- match.arg(algorithm)
  keep <- match.arg(keep)
  style <- match.arg(style)
  palette <- .tcm_palette_check(palette)

  network.data <- as.data.frame(network.data)
  net <- .tcm_build_graph(network.data, rem.dis.inter = rem.dis.inter)

  # ---- 节点容量控制（2026-10-23，P2）----
  # 原来拓扑页靠 GUI 侧 .cap_network_nodes 裁节点（口径与 ppi 不一致），且
  # module_netplot 本身无上限，大数据量糊成球。现统一走 tcm_filter_network 的
  # 正确顺序（过滤后算度数 → induced subgraph），与 ppi_plot/tcm_net 一致。
  # max_nodes = Inf 回退为不裁剪。
  net <- .tcm_filter_igraph(net, max_nodes = max_nodes,
                            min_degree = min_degree, keep = keep)
  # 裁剪会从边表重建图 → 顶点属性丢失。class 需重挂（community 在裁剪后的
  # 图上算，所以 module 不能预先挂）；degree 由 .tcm_net_plot 重算。
  igraph::V(net)$class <- .tcm_node_class(
    igraph::V(net)$name, network.data[, c("herb", "molecule", "target")]
  )

  .tcm_net_plot(
    net, view = "cluster", style = style,
    node.size = node.size, label.size = label.size,
    graph.layout = graph.layout, palette = palette,
    algorithm = algorithm
  )
}

# =============================================================================
# 内部工具（不导出）
# =============================================================================

# 拓扑入口的构图分派：三层网络 vs 二列边表（2026-10-28 修 D5）。
#
# 缺口背景：网药标准流程是「PPI 网络 → 拓扑中心性 → hub 基因」，但
# `tcm_centrality()` / `tcm_modules()` 只认 herb/molecule/target 三层网络，
# `tcm_ppi()` 的输出是 node1/node2 二列边表 —— 两个函数配对使用是文档示例
# 暗示的用法，接口却对不上（报错文本还只说「必须包含 herb, molecule, target」，
# 用户很难自己想到绕道）。
#
# 这里不猜：`input` 必须显式声明（铁律 60：不做基数/列数启发式自动判定）。
.tcm_topology_graph <- function(network.data, input = "triple",
                                rem.dis.inter = FALSE, fn = "") {
  if (identical(input, "edge")) {
    return(.tcm_edge_graph(network.data, rem.dis.inter = rem.dis.inter, fn = fn))
  }
  .tcm_build_graph(network.data, rem.dis.inter = rem.dis.inter)
}

# 二列边表 → igraph（PPI 形态）
#
# 列解析用 `.tcm_resolve_cols()` 的既有优先级：列名精确匹配（忽略大小写/._），
# 找不到就报错并列出全部候选列名 —— 不做「前两列就是边端点」的位置假设
# （那把 score 列当终点时会静默造出一张错图）。
.tcm_edge_graph <- function(network.data, rem.dis.inter = FALSE, fn = "") {
  cols <- .tcm_resolve_cols(
    network.data,
    aliases = list(
      from = c("from", "node1", "protein1", "gene1", "source"),
      to = c("to", "node2", "protein2", "gene2")
    ),
    what = "\u8fb9\u8868\u7684\u4e24\u7aef\uff08from/to\uff1bPPI \u8fb9\u8868\u4e3a node1/node2\uff09",
    fn = paste0(fn, "(input = \"edge\")")
  )
  links <- data.frame(
    from = as.character(network.data[[cols[["from"]]]]),
    to = as.character(network.data[[cols[["to"]]]]),
    stringsAsFactors = FALSE
  )
  # 自环在无向图里会虚增度数、并把 betweenness 算歪；NA/空串不是合法蛋白名。
  ok <- !is.na(links$from) & !is.na(links$to) &
    nzchar(links$from) & nzchar(links$to) & links$from != links$to
  links <- dplyr::distinct(links[ok, , drop = FALSE])
  if (nrow(links) == 0L) {
    stop(paste0(
      fn, "(input = \"edge\"): \u8fb9\u8868\u53bb\u6389\u81ea\u73af / \u7a7a\u503c\u540e",
      "\u6ca1\u6709\u5269\u4e0b\u4efb\u4f55\u8fb9\u3002"
    ), call. = FALSE)
  }
  if (rem.dis.inter) {
    links <- .ppi_drop_isolated(links)
    if (nrow(links) == 0L) {
      stop(paste0(
        fn, "(input = \"edge\"): \u53bb\u6389\u6e38\u79bb\u4e92\u4f5c\u540e",
        "\u6ca1\u6709\u5269\u4e0b\u4efb\u4f55\u8fb9\u3002"
      ), call. = FALSE)
    }
  }
  nodes <- data.frame(node = unique(c(links$from, links$to)), stringsAsFactors = FALSE)
  net <- igraph::graph_from_data_frame(d = links, vertices = nodes, directed = FALSE)
  igraph::V(net)$degree <- igraph::degree(net)
  # 边表没有节点角色：保留列（下游契约要 `class` 存在），值一律 NA。
  igraph::V(net)$class <- NA_character_
  igraph::V(net)$size <- igraph::degree(net)
  # 边表可能没有 weight 列（graph_from_data_frame 只在有第三列时才建 weight）。
  # 直接 `E(net)$score <- E(net)$weight` 在无该属性时会报 invalid indexing。
  if (!is.null(igraph::E(net)$weight)) {
    igraph::E(net)$score <- igraph::E(net)$weight
  }
  net
}

# 复用 tcm_net 的图构建逻辑：herb->molecule->target 二部边 + 节点角色标注。
# 不动 tcm_net 本身（避免回归），独立实现以保证 G1 自洽。
.tcm_build_graph <- function(network.data, rem.dis.inter = FALSE) {
  need_cols <- c("herb", "molecule", "target")
  if (!all(need_cols %in% names(network.data))) {
    stop(
      "\u60a8\u7684 network.data \u5fc5\u987b\u5305\u542b\u5217\uff1a",
      paste(need_cols, collapse = ", "),
      "\uff1b\u5f53\u524d\u5217\u4e3a\uff1a",
      paste(names(network.data), collapse = ", "),
      # D5：这条报错原本只说了「缺什么」，没说「该怎么办」——
      # 用户拿着 PPI 边表（node1/node2）看到它无法自行解出「改传 input」。
      "\n  \u82e5\u8fd9\u662f\u4e8c\u5217\u8fb9\u8868\uff08\u5982 tcm_ppi() \u7684\u8f93\u51fa",
      " node1/node2\uff09\uff0c\u8bf7\u7528 input = \"edge\"\u3002"
    )
  }
  nd <- network.data[, need_cols, drop = FALSE]
  hm <- dplyr::rename(dplyr::select(nd, herb, molecule), from = herb, to = molecule)
  hm$weight <- 1L
  mt <- dplyr::rename(dplyr::select(nd, molecule, target), from = molecule, to = target)
  mt$weight <- 1L
  links <- dplyr::distinct(rbind(hm, mt))
  if (rem.dis.inter) {
    fc <- table(links$from)
    tc <- table(links$to)
    links$from_c <- as.integer(fc[links$from])
    links$to_c <- as.integer(tc[links$to])
    links <- links[!(links$from_c == 1L & links$to_c == 1L),
      c("from", "to", "weight")]
  }
  nodes <- data.frame(node = unique(c(links$from, links$to)), stringsAsFactors = FALSE)
  net <- igraph::graph_from_data_frame(d = links, vertices = nodes, directed = FALSE)
  igraph::V(net)$degree <- igraph::degree(net)
  igraph::V(net)$class <- .tcm_node_class(igraph::V(net)$name, nd)
  igraph::V(net)$size <- igraph::degree(net)
  igraph::E(net)$score <- igraph::E(net)$weight
  net
}

# MCODE 风格邻域密度评分：节点邻域（含自身）内边数 / 可能边数，∈`[0,1]`。
# 对应 MCODE 的 node score（dense neighborhood），纯 R 复刻，离线可用。
.mcode_score <- function(net) {
  n <- igraph::vcount(net)
  scores <- numeric(n)
  for (v in seq_len(n)) {
    nb <- igraph::neighbors(net, v)
    if (length(nb) == 0L) {
      scores[v] <- 0
      next
    }
    sg <- igraph::induced_subgraph(net, c(v, nb))
    e <- igraph::ecount(sg)
    nv <- igraph::vcount(sg)
    scores[v] <- if (nv <= 1L) 0 else e / (nv * (nv - 1L) / 2)
  }
  scores
}
