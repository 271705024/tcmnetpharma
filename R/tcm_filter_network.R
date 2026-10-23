# ---------------------------------------------------------------------------
# 统一网络筛选管线（2026-10-23，P2）
#
# 背景（boss 反馈「大数据量下药物-靶点图爆炸、仿 Cytoscape 节点筛选反复
# 只有点没有线、桑基图爆炸」）：四个网络入口各有一套筛选逻辑，互不一致，
# 这是爆炸与丢线的总根源 ——
#   · tcm_net    无任何节点容量控制（rem.dis.inter 只去孤立边）
#   · tcm_sankey 无过滤（GUI 侧按行截断会把路径截成半截）
#   · ppi_plot   .ppi_cap_nodes 在**全图**算度数、在**子图**取边 → 被保留的
#                hub 之间恰好无边时只剩点没有线
#   · 拓扑页     .cap_network_nodes 又一套口径
#
# 本函数统一这一件事：把一张（可能很大的）边表，按「边规则先行 → 节点=边端点
# → 过滤后算度数 → 超限 induced subgraph」的顺序裁到可控规模，并返回裁剪报告。
# 顺序本身就是正确性的来源（详见各步注释）。
# ---------------------------------------------------------------------------


#' 统一网络筛选：把边表裁到可控规模并返回裁剪报告
#'
#' 网络图在大数据量下会「爆炸」（力导向布局糊成一团、标签互相压）或「只有点
#' 没有线」（在错误的地方算度数）。本函数用一条固定顺序的管线统一处理，供
#' `tcm_net` / `ppi_plot` / 桑基图 / 拓扑页共用（单一事实源）。
#'
#' 管线顺序（顺序即正确性）：
#' 1. **边规则先行**：先在**全量边**上应用规则（如去掉两端度数都为 `min_degree`
#'    以下的边）。
#' 2. **节点 = 当前边的端点并集**：节点永远从边派生，结构上不可能出现孤点。
#' 3. **度数在过滤后的图上计算**：`ppi_cap_nodes` 那种「全图 hub 之间恰好无边」
#'    的失败，根源是**在全图上算度数、在子图上画边**。这里度数基于第 2 步的图。
#' 4. **超限时按度数取 top `max_nodes`，再取 induced subgraph**：边跟着节点一起
#'    裁，不会丢连线；`keep = "ego"` 保留 top 节点及其一跳邻居（更密子网）。
#' 5. 返回裁剪报告（各步保留/丢弃计数），GUI 直接展示。
#'
#' @param links 边表 data.frame，至少含 `from` / `to` 两列（可带 `weight`）。
#' @param max_nodes 节点数上限。`NA` / `Inf` 表示不限（仅做边规则与孤点清理）。
#' @param min_degree 度数下限：度数低于此值的节点（及其孤边）在**边规则**阶段
#'   被剔除。默认 1（只去掉零度孤点，等价于「移除孤立节点」）。
#' @param keep 超限时的选点策略：`"top_degree"`（默认，保留度数最高者）或
#'   `"ego"`（保留 top 节点及其一跳邻居，子网更密但节点更多）。
#'
#' @return list(links = 裁剪后的边表（仍为 from/to/... 列），
#'   report = list(n_nodes_orig, n_edges_orig, n_nodes_kept, n_edges_kept,
#'   n_nodes_dropped, n_edges_dropped, rule = 字符串描述, truncated = 是否超限裁剪))
#' @export
tcm_filter_network <- function(links, max_nodes = 150L, min_degree = 1L,
                               keep = c("top_degree", "ego")) {
  keep <- match.arg(keep)
  links <- as.data.frame(links)
  if (nrow(links) == 0L) {
    return(list(
      links = links,
      report = list(n_nodes_orig = 0L, n_edges_orig = 0L, n_nodes_kept = 0L,
                    n_edges_kept = 0L, n_nodes_dropped = 0L, n_edges_dropped = 0L,
                    rule = "empty input", truncated = FALSE)
    ))
  }
  need <- c("from", "to")
  if (!all(need %in% names(links))) {
    stop("`links` \u5fc5\u987b\u5305\u542b\u5217 from / to \uff1b\u5f53\u524d\u5217\u4e3a ",
         paste(names(links), collapse = ", "), call. = FALSE)
  }
  keep_cols <- intersect(c("from", "to", "weight"), names(links))
  links <- links[, keep_cols, drop = FALSE]
  links$from <- as.character(links$from)
  links$to <- as.character(links$to)

  n_edges_orig <- nrow(links)
  n_nodes_orig <- length(unique(c(links$from, links$to)))

  # ---- 第 1 步：边规则先行（在全量边上算度数，去孤立/低度边）----
  # 用「两端度数都 < min_degree」作为可剔除边：一端是核心、一端是叶子时保留
  # （叶子是核心的邻居，删了会丢连线）；只有「两端都是叶子」的孤立边才剔。
  if (min_degree > 0L) {
    deg_all <- table(c(links$from, links$to))
    from_d <- as.integer(deg_all[links$from])
    to_d <- as.integer(deg_all[links$to])
    drop_edge <- (from_d < min_degree) & (to_d < min_degree)
    links <- links[!drop_edge, , drop = FALSE]
  }

  # ---- 第 2 步：节点 = 当前边的端点并集 ----
  nodes <- unique(c(links$from, links$to))
  n_after_rule <- length(nodes)

  # ---- 第 3 步：在过滤后的图上算度数 ----
  deg <- table(c(links$from, links$to))
  deg <- as.integer(deg[nodes])
  names(deg) <- nodes

  # ---- 第 4 步：超限时按度数取 top，再取 induced subgraph ----
  truncated <- FALSE
  n_nodes_limit <- max_nodes
  if (is.null(n_nodes_limit) || length(n_nodes_limit) == 0L) n_nodes_limit <- Inf
  n_nodes_limit <- suppressWarnings(as.numeric(n_nodes_limit)[[1]])
  if (is.na(n_nodes_limit)) n_nodes_limit <- Inf

  if (is.finite(n_nodes_limit) && length(nodes) > n_nodes_limit) {
    truncated <- TRUE
    ord <- order(deg, decreasing = TRUE)
    top_idx <- ord[seq_len(min(as.integer(n_nodes_limit), length(nodes)))]
    keep_nodes <- nodes[top_idx]
    if (identical(keep, "ego")) {
      # 保留 top 节点及其一跳邻居（邻居度数可能不高，但让子网更密、更连）
      nbr <- unique(unlist(
        lapply(keep_nodes, function(v) {
          c(links$to[links$from == v], links$from[links$to == v])
        }),
        use.names = FALSE
      ))
      keep_nodes <- unique(c(keep_nodes, nbr))
      # ego 可能超过 max_nodes —— 这是「要更密子网」的代价，但仍要封顶防爆炸：
      # 若一跳邻居太多，仍按度数回退到 top max_nodes*2 以内。
      if (length(keep_nodes) > as.integer(n_nodes_limit) * 2L) {
        ord2 <- order(deg[keep_nodes], decreasing = TRUE)
        keep_nodes <- keep_nodes[ord2[seq_len(as.integer(n_nodes_limit) * 2L)]]
      }
    }
    # induced subgraph：只保留两端都在 keep_nodes 内的边
    keep_edge <- (links$from %in% keep_nodes) & (links$to %in% keep_nodes)
    links <- links[keep_edge, , drop = FALSE]
    # 重新收紧节点 = 剩余边的端点（ego 可能引入了度为 0 的孤立邻居，去掉）
    final_nodes <- unique(c(links$from, links$to))
    n_nodes_kept <- length(final_nodes)
    n_edges_kept <- nrow(links)
  } else {
    n_nodes_kept <- length(nodes)
    n_edges_kept <- nrow(links)
  }

  report <- list(
    n_nodes_orig = as.integer(n_nodes_orig),
    n_edges_orig = as.integer(n_edges_orig),
    n_nodes_kept = as.integer(n_nodes_kept),
    n_edges_kept = as.integer(n_edges_kept),
    n_nodes_dropped = as.integer(n_nodes_orig - n_nodes_kept),
    n_edges_dropped = as.integer(n_edges_orig - n_edges_kept),
    rule = if (truncated) {
      sprintf("top_degree(%d) + induced subgraph", as.integer(n_nodes_limit))
    } else {
      sprintf("min_degree(%d) edge rule", as.integer(min_degree))
    },
    truncated = truncated
  )

  list(links = links, report = report)
}


# ---------------------------------------------------------------------------
# igraph 对象 → 统一管线裁剪 → igraph（供 ppi_plot / module_netplot 复用）
#
# 为什么抽出来：ppi_plot 与拓扑页的 module_netplot 都以「已构好的 igraph」为
# 处理对象，但它们的容量裁剪都必须走 tcm_filter_network 的**正确顺序**
# （过滤后算度数 → induced subgraph），否则「只有点没有线」会复发。单一事实源：
# 这里把「igraph ↔ 边表」的转换封装一次，两处调用不再各写一份。
# ---------------------------------------------------------------------------
.tcm_filter_igraph <- function(net, max_nodes = 150L, min_degree = 0L,
                               keep = c("top_degree", "ego")) {
  keep <- match.arg(keep)
  if (!is.finite(max_nodes) || igraph::vcount(net) <= max_nodes) return(net)
  edges <- igraph::as_data_frame(net, what = "edges")
  flt <- tcm_filter_network(edges, max_nodes = max_nodes,
                            min_degree = min_degree, keep = keep)
  igraph::graph_from_data_frame(d = flt$links, directed = FALSE)
}
