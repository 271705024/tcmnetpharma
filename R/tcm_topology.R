#' 拓扑中心性分析（degree / betweenness / closeness）
#'
#' 从 herb-molecule-target 三元组构建无向图，计算各节点的度中心性、介数中心性
#' （betweenness）与接近中心性（closeness）。三者是网络药理学"核心靶点"识别的
#' 标准拓扑指标（对应 CytoNCA 的 Degree / BC / CC）。
#'
#' @param network.data 数据框，必须含 `herb` / `molecule` / `target` 三列
#'   （与 [tcm_net()] 同构；通常来自 GUI 查询结果或 [tcmnetpharma::network_data]）
#' @param class 仅返回该类型节点：`"herb"` / `"molecule"` / `"target"`；
#'   默认 NULL = 返回全部
#' @param rem.dis.inter 是否移除无连接的孤立节点（同 [tcm_net()]）
#' @param top_n 仅返回中心性最高的前 N 个节点（按 betweenness 降序）；NULL = 全部
#'
#' @return 数据框：`node` / `class` / `degree` / `betweenness` / `closeness`
#'   按 `betweenness` 降序（核心靶点排最前）
#' @export
#'
#' @importFrom igraph graph_from_data_frame V degree betweenness closeness
#' @importFrom dplyr select rename distinct
#'
#' @examples
#' \dontrun{
#' data("network_data", package = "tcmnetpharma")
#' tcm_centrality(network_data)
#' tcm_centrality(network_data, class = "target", top_n = 10)
#' }
tcm_centrality <- function(network.data,
                           class = NULL,
                           rem.dis.inter = FALSE,
                           top_n = NULL) {
  network.data <- as.data.frame(network.data)
  if (nrow(network.data) == 0L) {
    return(data.frame(
      node = character(), class = character(),
      degree = numeric(), betweenness = numeric(), closeness = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  net <- .tcm_build_graph(network.data, rem.dis.inter = rem.dis.inter)
  df <- data.frame(
    node = igraph::V(net)$name,
    class = igraph::V(net)$class,
    degree = igraph::degree(net),
    betweenness = igraph::betweenness(net),
    closeness = igraph::closeness(net),
    stringsAsFactors = FALSE
  )
  if (!is.null(class)) {
    class <- match.arg(class, c("herb", "molecule", "target"))
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
#' @param network.data 数据框，必须含 `herb` / `molecule` / `target` 三列
#' @param algorithm 社区发现算法：`"louvain"`（默认）/ `"infomap"` / `"walktrap"`
#' @param rem.dis.inter 是否移除孤立节点
#' @param mcode_k 仅保留 k-core 数 ≥ `mcode_k` 的节点参与模块汇总（默认 2，
#'   过滤掉松散单点；设 0 表示不按 k-core 过滤）
#'
#' @return 长度为 2 的列表：
#'   - `nodes`：数据框 `node` / `class` / `module` / `degree` / `betweenness` /
#'     `closeness` / `coreness` / `mcode_score`
#'   - `modules`：数据框 `module` / `size` / `hub` / `top_targets` / `density` /
#'     `mean_mcode`（按 `size` 降序）
#' @export
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
#' }
tcm_modules <- function(network.data,
                        algorithm = c("louvain", "infomap", "walktrap"),
                        rem.dis.inter = FALSE,
                        mcode_k = 2) {
  algorithm <- match.arg(algorithm)
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
  net <- .tcm_build_graph(network.data, rem.dis.inter = rem.dis.inter)
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
    tgt <- sub[sub$class == "target", ]
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
#' @param network.data 数据框，必须含 `herb` / `molecule` / `target` 三列
#' @param algorithm 社区发现算法（同 [tcm_modules()]）
#' @param rem.dis.inter 是否移除孤立节点
#' @param graph.layout ggraph 布局，默认 `"fr"`
#' @param max_nodes 节点数上限（`Inf` 不裁剪），默认 150
#' @param min_degree 度数下限，默认 1（移除孤立节点）
#' @param keep 超限选点策略：`"top_degree"` / `"ego"`
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
#' }
module_netplot <- function(network.data,
                           algorithm = c("louvain", "infomap", "walktrap"),
                           rem.dis.inter = FALSE,
                           graph.layout = "fr",
                           max_nodes = 150,
                           min_degree = 1,
                           keep = c("top_degree", "ego")) {
  algorithm <- match.arg(algorithm)
  keep <- match.arg(keep)
  net <- .tcm_build_graph(network.data, rem.dis.inter = rem.dis.inter)
  # ---- 节点容量控制（2026-10-23，P2）----
  # 原来拓扑页靠 GUI 侧 .cap_network_nodes 裁节点（口径与 ppi 不一致），且
  # module_netplot 本身无上限，大数据量糊成球。现统一走 tcm_filter_network 的
  # 正确顺序（过滤后算度数 → induced subgraph），与 ppi_plot/tcm_net 一致。
  # max_nodes = Inf 回退为不裁剪。
  net <- .tcm_filter_igraph(net, max_nodes = max_nodes,
                            min_degree = min_degree, keep = keep)
  comm <- switch(algorithm,
    louvain = igraph::cluster_louvain(net),
    infomap = igraph::cluster_infomap(net),
    walktrap = igraph::cluster_walktrap(net)
  )
  igraph::V(net)$module <- as.integer(igraph::membership(comm))
  col <- RColorBrewer::brewer.pal(8, "Set2")
  colorN <- grDevices::colorRampPalette(colors = col)(max(igraph::V(net)$module))
  ggraph::ggraph(net, layout = graph.layout) +
    ggraph::geom_edge_link0(aes(edge_linewidth = weight), edge_colour = "lightgrey") +
    ggraph::geom_node_point(
      ggplot2::aes(color = factor(module), size = degree),
      alpha = 1.0
    ) +
    ggplot2::scale_color_manual(values = colorN) +
    ggraph::scale_edge_width(range = c(0.2, 2)) +
    ggraph::theme_graph(base_family = "sans")
}

# =============================================================================
# 内部工具（不导出）
# =============================================================================

# 复用 tcm_net 的图构建逻辑：herb->molecule->target 二部边 + 节点角色标注。
# 不动 tcm_net 本身（避免回归），独立实现以保证 G1 自洽。
.tcm_build_graph <- function(network.data, rem.dis.inter = FALSE) {
  need_cols <- c("herb", "molecule", "target")
  if (!all(need_cols %in% names(network.data))) {
    stop(
      "\u60a8\u7684 network.data \u5fc5\u987b\u5305\u542b\u5217\uff1a",
      paste(need_cols, collapse = ", "),
      "\uff1b\u5f53\u524d\u5217\u4e3a\uff1a",
      paste(names(network.data), collapse = ", ")
    )
  }
  nd <- network.data[, need_cols, drop = FALSE]
  node_class <- function(vnames, x) {
    out <- rep(NA_character_, length(vnames))
    out[vnames %in% as.character(x[[3]])] <- colnames(x)[3]
    out[vnames %in% as.character(x[[2]])] <- colnames(x)[2]
    out[vnames %in% as.character(x[[1]])] <- colnames(x)[1]
    out[is.na(out)] <- "other"
    out
  }
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
  igraph::V(net)$class <- node_class(igraph::V(net)$name, nd)
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
