#' venn network
#'
#' 二分图（基因 / 来源）网络图：把「基因 → 所属来源」的长表画成网络。
#'
#' @param data 两列长表（第一列基因、第二列来源）。列名可用 `gene_col` /
#'   `source_col` 显式指定；不指定时先按常见列名（gene / from / source / set …）
#'   识别，识别不到且恰为两列则按**位置**（第 1 列 = 基因、第 2 列 = 来源）取。
#' @param node.color \strong{已废弃}。节点颜色现在由节点角色（gene / source）
#'   决定，改用 `palette` 指定定性色板。传入本参数会给出警告并被忽略。
#' @param node.size 节点大小范围（长度 2）
#' @param label.size 标签字号
#' @param label.degree 标签的**绝对度数阈值**（度数 ≥ 该值的节点出标签）。
#'   默认 `NULL` 表示「标 top 15 个高度数节点」—— 二分图里 source 节点的度数
#'   最高，因此它们必然被标出。旧版默认 `1`（= 全标），节点一多会糊成一团。
#' @param label.repel 标签是否 repel
#' @param edge.color 边色板。二分图里边的颜色表示「属于哪个来源」，因此需要
#'   ≥ 来源数的颜色；给少了会自动插值补足（旧版写死 5 色，来源一多就整张图
#'   渲染失败）。`NULL` 时用 `palette` 的定性色板。
#' @param edge.alpha 边透明度；`NULL` 时用 `style` 预设
#' @param edge.width 边线宽；`NULL` 时用 `style` 预设
#' @param edge.type 边几何：`"arc"`（默认）/ `"fan"` / `"hive"` / `"link"`。
#'   \strong{只改形状，不改编码语义}。
#' @param graph.layout 布局名（传给 [ggraph::create_layout()]）：
#'   "fr" / "kk" / "nicely" / "circle" / "graphopt" / "lgl" / "mds" / "tree" /
#'   "star" / "grid" 等
#' @param gene_col,source_col 显式指定基因列 / 来源列（列名或列号）
#' @param palette 配色体系 key（见 [tcm_palette()]）；`NULL` 时用 `"npg"`
#' @param style 外观轴预设（与节点/边几何正交）
#' @param max_nodes 节点数上限（默认 300）。二分图靠「哪些基因被多个来源共享」
#'   传达信息，把小度数基因裁掉会直接改变结论，所以默认给得比 `tcm_net` 宽松
#' @param min_degree 最小度数过滤（默认 1，即不过滤）
#' @param keep 超限时的保留策略：`"top_degree"`（默认）/ `"ego"`
#' @return ggraph / ggplot 对象
#' @export
#' @importFrom ggplot2 scale_size_continuous
#' @importFrom ggplot2 aes
#' @importFrom dplyr distinct
#' @importFrom ggraph ggraph
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph theme_graph
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph V
#' @importFrom igraph E
#' @examples
#' \dontrun{
#' data(venn_data, package = "tcmnetpharma")
#' set.seed(1234)
#' gene <- names(sort(table(venn_data$gene), decreasing = TRUE))[1:50]
#' data <- venn_data[venn_data$gene %in% gene, ]
#' head(venn_net(data))
#' head(venn_net(data, edge.type = "hive"))
#' }
venn_net <- function(data,
                     node.color = NULL,
                     node.size = c(1, 10),
                     label.size = 3,
                     label.degree = NULL,
                     label.repel = TRUE,
                     edge.color = NULL,
                     edge.alpha = NULL,
                     edge.width = NULL,
                     edge.type = c("arc", "fan", "hive", "link"),
                     graph.layout = "kk",
                     gene_col = NULL, source_col = NULL,
                     palette = NULL,
                     style = c("default", "journal", "cytoscape",
                               "presentation"),
                     max_nodes = 300L, min_degree = 1L,
                     keep = c("top_degree", "ego")) {
  # ⚠️ `match.arg()` 的实参必须是「变量名」而不是表达式（铁律 80）：
  #    省略 choices 时它用 substitute(arg) 去读形参默认值。
  edge.type <- match.arg(edge.type)
  style <- match.arg(style)
  keep <- match.arg(keep)

  # ---- 0. 废弃参数 --------------------------------------------------------
  if (!is.null(node.color)) {
    warning(
      "`node.color` \u5df2\u5e9f\u5f03\uff1a\u8282\u70b9\u989c\u8272\u73b0\u7531\u8282\u70b9\u89d2\u8272",
      "\uff08gene / source\uff09\u51b3\u5b9a\uff0c\u8bf7\u6539\u7528 `palette =` ",
      "\u6307\u5b9a\u5b9a\u6027\u8272\u677f\u3002\u672c\u53c2\u6570\u5df2\u88ab\u5ffd\u7565\u3002",
      call. = FALSE
    )
  }

  # ---- 1. 输入校验（旧版此处只 print 一句然后继续往下跑，最终报出
  #         "attempt to set 'colnames' on an object with less than two dimensions"）----
  if (!is.data.frame(data)) {
    stop(sprintf(
      "`data` \u5fc5\u987b\u662f\u4e24\u5217 data.frame / tibble\uff0c\u5b9e\u9645\u4e3a\uff1a%s",
      paste(class(data), collapse = " / ")
    ), call. = FALSE)
  }
  nms <- names(data)
  if (length(nms) < 2L) {
    stop(sprintf(
      "`data` \u81f3\u5c11\u9700\u8981\u4e24\u5217\uff08\u57fa\u56e0 + \u6765\u6e90\uff09\uff0c\u5b9e\u9645\u53ea\u6709 %d \u5217\u3002",
      length(nms)
    ), call. = FALSE)
  }

  # ---- 2. 列解析：显式 > 列名别名 > 位置（仅当恰为两列）--------------------
  # ⚠️ 旧实现用 `length(unique(data[, 1])) > length(unique(data[, 2]))` 猜哪列是
  #    from/to。这个判据**在 tibble 和 data.frame 上取值不同**：1 列 tibble 的
  #    `length()` 是**列数 1**，而 data.frame 单列取出来是向量、`length()` 是行数。
  #    实测 venn_data（gene 149 / source 4）：
  #      tibble     -> 1 > 1 为假 -> c("from","to") -> 正确（纯属侥幸）
  #      data.frame -> 149 > 4 为真 -> c("to","from") -> 边按**基因名**着色
  #                    （149 个水平 vs 5 个颜色）-> 渲染直接报错
  #                    "Insufficient values in manual scale. 149 needed but only 5 provided."
  #    2026-09-15 的 p42 报告把这归因为「适配层传了 as.data.frame」，**归因是错的**：
  #    缺陷在包内这一行。现改为显式 / 别名 / 位置三级解析，不再按基数猜。
  pick_col <- function(explicit, aliases, pos, what) {
    if (!is.null(explicit)) {
      key <- if (is.numeric(explicit)) {
        if (explicit < 1L || explicit > length(nms)) {
          stop(sprintf("\u5217\u53f7 %s \u8d85\u51fa\u8303\u56f4\uff08\u5171 %d \u5217\uff09\u3002",
                       format(explicit), length(nms)), call. = FALSE)
        }
        nms[[as.integer(explicit)]]
      } else {
        as.character(explicit)[[1L]]
      }
      if (!key %in% nms) {
        stop(sprintf("\u627e\u4e0d\u5230 %s \u5217\u300c%s\u300d\u3002\u5f53\u524d\u5217\uff1a%s",
                     what, key, paste(nms, collapse = ", ")), call. = FALSE)
      }
      return(key)
    }
    hit <- intersect(aliases, nms)
    if (length(hit)) return(hit[[1L]])
    if (length(nms) == 2L) return(nms[[pos]])
    stop(sprintf(
      paste0("\u65e0\u6cd5\u4ece\u5217\u540d\u8bc6\u522b\u51fa %s \u5217\uff08\u5171 %d \u5217\uff09\u3002\n",
             "  \u5f53\u524d\u5217\uff1a%s\n  \u8bf7\u663e\u5f0f\u6307\u5b9a\uff1a%s"),
      what, length(nms), paste(nms, collapse = ", "),
      if (identical(what, "gene")) 'gene_col = "<\u5217\u540d>"'
      else 'source_col = "<\u5217\u540d>"'
    ), call. = FALSE)
  }
  gcol <- pick_col(gene_col,
                   c("gene", "Gene", "from", "name", "GeneID", "symbol"),
                   1L, "gene")
  scol <- pick_col(source_col,
                   c("source", "Source", "set", "Set", "to", "database",
                     "dataset", "database_name"),
                   2L, "source")
  if (identical(gcol, scol)) {
    stop("gene \u5217\u4e0e source \u5217\u4e0d\u80fd\u662f\u540c\u4e00\u5217\u3002",
         call. = FALSE)
  }

  # ---- 3. 边表 ------------------------------------------------------------
  ed <- data.frame(
    from = as.character(data[[gcol]]),
    to   = as.character(data[[scol]]),
    stringsAsFactors = FALSE
  )
  ok <- !is.na(ed$from) & !is.na(ed$to) &
    nzchar(ed$from) & nzchar(ed$to)
  ed <- ed[ok, , drop = FALSE]
  ed <- ed[!duplicated(ed), , drop = FALSE]
  if (!nrow(ed)) {
    stop("\u53bb\u6389 NA / \u7a7a\u503c / \u91cd\u590d\u540e\u6ca1\u6709\u5269\u4e0b\u4efb\u4f55\u8fb9\u3002",
         call. = FALSE)
  }

  # ---- 4. 构图 + 角色 -----------------------------------------------------
  net <- igraph::graph_from_data_frame(ed, directed = FALSE)
  # 角色按「是否出现在 to 列」判定。⚠️ 若某个基因名恰与某来源同名，会并成同一
  # 节点并被判为 source —— 这是数据问题，不在这里猜。
  igraph::V(net)$class <- ifelse(igraph::V(net)$name %in% ed$to,
                                 "source", "gene")

  # ---- 5. 容量裁剪（与 tcm_net / module_netplot 同一条管线）---------------
  net <- .tcm_net_cap(net, view = "overview", max_nodes = max_nodes,
                      min_degree = min_degree, keep = keep)$net
  # degree 必须按裁剪后的子图重算（铁律：裁剪会重建图）
  igraph::V(net)$degree <- igraph::degree(net)

  # ---- 6. 边属性：来源 -----------------------------------------------------
  # ⚠️ 不能用 `ed$to` 直接挂：裁剪可能重建过图、边的顺序不再保证。
  #    按端点里**角色为 source 的那一端**取名，与图结构始终一致。
  is_src <- igraph::V(net)$class == "source"
  names(is_src) <- igraph::V(net)$name
  ends <- igraph::ends(net, igraph::E(net), names = TRUE)
  ea <- ends[, 1L]
  eb <- ends[, 2L]
  igraph::E(net)$source <- ifelse(is_src[ea] & !is_src[eb], ea, eb)

  # ---- 7. 交给共享视觉层 --------------------------------------------------
  # `label_extra` = 所有 source 节点：二分图的锚必须始终带标签，
  # 否则来源一多就会被 top-N 挤掉，整张图读不出「基因属于哪个来源」。
  .tcm_net_plot(
    net, view = "overview", style = style,
    node.size = node.size, label.size = label.size,
    graph.layout = graph.layout, palette = palette,
    label_degree = label.degree, label_repel = label.repel,
    label_extra = igraph::V(net)$name[igraph::V(net)$class == "source"],
    edge_color = "lightgrey",
    edge_geom = edge.type,
    edge_colour_by = "source",
    edge_palette = edge.color,
    edge_alpha = edge.alpha,
    edge_lw = edge.width
  )
}
