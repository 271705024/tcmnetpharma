# ===========================================================================
# R2 网络视觉公共层（2026-10-24）
#
# 为什么要有这一层：`tcm_net` / `ppi_plot` / `venn_net` 各自实现了「构图 ->
# 裁剪 -> 配色 -> 标签 -> 图例」，结果是同一类缺陷在三个函数里各犯一次
# （双绘边、legend 档位失效、size 通道被错标度接管）。收敛到一处后，
# **视觉契约只有一个事实源**（铁律 54）。
#
# 分解方式与 R1 一致：**正交轴不合并成一个枚举**
#   view  = 数据/形状轴（看哪些节点、颜色表什么、标哪些标签）
#   style = 外观轴（边宽/边透明度/标签字号/底色描边）
# 两者任意组合都合法 —— `view = "cluster", style = "journal"` 是有意义的。
# ===========================================================================


# ---------------------------------------------------------------------------
# 节点角色（herb / molecule / target）赋值
#
# 为什么单列出来：这段闭包在 `tcm_net` 和 `.tcm_build_graph` 里各有一份完全
# 相同的实现，`module_netplot` 接入新层后是第三处。三份同源 → 按铁律 54 收敛。
#
# 关键约束（E5 修复的原意）：**按节点名逐行判定**，长度恒等于顶点数。
# 旧实现按列位置 `nd[, 2]` 取值，入参含多余列时长度对不上 →
# igraph 报 "Length of new attribute value must be 1 or 888, ... not 887"。
# 角色优先级：herb > molecule > target（同一名字出现在多列时取最先匹配的）。
# ---------------------------------------------------------------------------

#' 按节点名判定角色
#' @param vnames 顶点名向量
#' @param nd 含三列的 data.frame（列名必须含 herb/molecule/target）
#' @noRd
.tcm_node_class <- function(vnames, nd) {
  out <- rep(NA_character_, length(vnames))
  out[vnames %in% as.character(nd[["target"]])] <- "target"
  out[vnames %in% as.character(nd[["molecule"]])] <- "molecule"
  out[vnames %in% as.character(nd[["herb"]])] <- "herb"
  out[is.na(out)] <- "other"
  out
}


# ---------------------------------------------------------------------------
# 列解析（供 venn_plot / tf_cirplot 共用）
#
# ⚠️ 2026-10-24 实测的缺陷形态：旧实现用「两列的基数谁大」判断谁是基因列
#    —— 合法输入 `gene/source` 在「5 个基因 × 8 个来源」时会判成
#    `c(source, gene)`，于是 ggvenn 的集合名变成 G1..G5（把基因当来源），
#    **不报错、图照出**，只有读集合名才发现。
#    这与铁律 60 同源：基数/包含关系这类启发式 + 全候选集合 = 假通过。
#
# 解析优先级（**永不使用基数猜测**）：
#   ① 用户显式传参
#   ② 列名精确匹配（忽略大小写 / 下划线 / 点号）
#   ③ 报错并列出全部候选列名，提示用显式参数指定
# ---------------------------------------------------------------------------

#' 把列名归一化后用于匹配（只做无损处理：去空白、转小写、去 . 和 _）
#' @noRd
.tcm_norm_colname <- function(x) {
  gsub("[._[:space:]]", "", tolower(as.character(x)))
}

#' 在 data 中按「显式参数 > 列名精确匹配 > 报错」解析出一组列
#'
#' @param data data.frame
#' @param explicit 命名 list，元素为用户显式传入的列名（可为 NULL）
#' @param aliases 命名 list，元素为候选别名向量（第一个是规范名）
#' @param what 用于报错文案的角色描述（如 "gene / source"）
#' @param fn 调用方函数名（报错文案用）
#' @return 命名 integer 向量：每个角色对应的列序号
#' @noRd
.tcm_resolve_cols <- function(data, explicit = list(), aliases = list(),
                             what = "", fn = "") {
  nms <- names(data)
  norm <- .tcm_norm_colname(nms)
  out <- integer(length(aliases))
  names(out) <- names(aliases)

  for (role in names(aliases)) {
    # ① 显式参数
    ex <- explicit[[role]]
    if (!is.null(ex) && length(ex) && nzchar(as.character(ex)[1L])) {
      ex <- as.character(ex)[1L]
      hit <- which(norm == .tcm_norm_colname(ex))
      if (!length(hit)) {
        stop(sprintf(
          "%s: \u5217 '%s' \u4e0d\u5b58\u5728\u3002\u5f53\u524d\u5217\u4e3a\uff1a%s",
          fn, ex, paste(nms, collapse = ", ")
        ), call. = FALSE)
      }
      out[[role]] <- hit[1L]
      next
    }
    # ② 列名精确匹配（按别名顺序，先到先得）
    hit <- integer(0)
    for (a in aliases[[role]]) {
      h <- which(norm == .tcm_norm_colname(a))
      if (length(h)) {
        hit <- h
        break
      }
    }
    if (length(hit)) {
      out[[role]] <- hit[1L]
      next
    }
    # ③ 报错，绝不猜
    stop(sprintf(
      paste0(
        "%s: \u65e0\u6cd5\u4ece\u5217\u540d\u8bc6\u522b\u51fa %s\u3002\n",
        "  \u5f53\u524d\u5217\uff1a%s\n",
        "  \u53ef\u63a5\u53d7\u7684\u5217\u540d\uff1a%s\n",
        "  \u8bf7\u663e\u5f0f\u6307\u5b9a\uff1a%s"
      ),
      fn, what,
      paste(nms, collapse = ", "),
      paste(vapply(aliases, function(a) paste(a, collapse = " / "), ""), collapse = "\uff1b"),
      paste(sprintf("%s = \"<%s>\"", names(aliases), names(aliases)), collapse = ", ")
    ), call. = FALSE)
  }
  out
}


# ---------------------------------------------------------------------------
# view 预设：数据/形状轴
#
# 每个 view 只决定三件事：**看哪些节点、颜色表什么、标哪些标签**。
# ⚠️ 硬约束（来自 R1 的「一个变量一个通道」）：degree 默认**只控 size**；
#    只有 hub 视图把 degree 离散分档给 colour —— 因为分档是类别信息，
#    与「连续渐变」不是一回事，且 hub 视图的标签是全标，不会造成通道冲突。
# ---------------------------------------------------------------------------
.tcm_net_view_presets <- function() {
  list(
    overview = list(
      max_nodes = 50L, colour = "class", label_mode = "top",
      label_top = 15L, keep = "top_degree"
    ),
    cluster = list(
      max_nodes = 50L, colour = "module", label_mode = "cluster",
      # 这里 label_top 的含义是「最多标几个簇」（不是节点数）：16 个模块全标时
      # 质心标签会互相压，只标最大的 8 个，其余仍着色。
      label_top = 8L, keep = "top_degree"
    ),
    hub = list(
      # ⚠️ 与设计稿的偏差（2026-10-24 视觉验收后改）：设计稿写「全标（节点少）」，
      #    实测 keep = "ego" 会把 30 的上限扩张到 60 个节点（核心 + 一跳邻居），
      #    全标后标签互相压成一团 —— 反而看不出「关键节点是谁」。
      #    改为：结构上保留 hub + 邻居（看得到连接关系），
      #    但标签只给 top 10（也就是 hub 本身）。
      max_nodes = 20L, colour = "degree_bin", label_mode = "top",
      label_top = 10L, keep = "ego"
    )
  )
}

#' view 的合法取值（给 match.arg 与文档用）
#' @noRd
.tcm_net_views <- function() names(.tcm_net_view_presets())

#' style 预设：外观轴（与 view 正交）
#' @noRd
.tcm_net_style_presets <- function() {
  list(
    default = list(edge_alpha = 0.35, edge_lw = 0.35, label_size = 3.2,
                   label_box = TRUE),
    journal = list(edge_alpha = 0.30, edge_lw = 0.25, label_size = 3.0,
                   label_box = TRUE),
    cytoscape = list(edge_alpha = 0.45, edge_lw = 0.45, label_size = 3.2,
                     label_box = TRUE),
    presentation = list(edge_alpha = 0.55, edge_lw = 0.60, label_size = 4.0,
                        label_box = TRUE)
  )
}

#' 边图层：几何由 `edge_geom` 决定，**编码语义不变**
#'
#' `edge_geom` 只影响**形状**（直线 / 弧 / 扇形 / hive），不影响「边表示什么」。
#' 之所以把它做成共享层的一个参数，而不是让各函数自带一套绘制代码：
#' 旧 `venn_net` 为三种边几何各复制了 30 行完全相同的节点 / 标签 / 色标代码
#' （三份同源 → 铁律 54），改一处必漏两处。
#'
#' @param edge_geom "link" / "arc" / "fan" / "hive"
#' @param sp style 预设（提供 edge_alpha / edge_lw）
#' @param edge_color 固定边色（`colour_by` 为 NULL 时使用）
#' @param net igraph 对象（`colour_by` 非 NULL 时用于校验边属性存在）
#' @param colour_by 边属性名；非 NULL 时按它给边上色**并出图例**
#' @noRd
.tcm_edge_layer <- function(edge_geom = "link", sp, edge_color = "lightgrey",
                            net = NULL, colour_by = NULL,
                            width_by = NULL, width_range = c(0.2, 2)) {
  edge_geom <- match.arg(edge_geom, c("link", "arc", "fan", "hive"))
  fn <- switch(edge_geom,
    link = ggraph::geom_edge_link,
    arc  = ggraph::geom_edge_arc,
    fan  = ggraph::geom_edge_fan,
    hive = ggraph::geom_edge_hive
  )

  # ---- 宽度通道：默认固定线宽；给了 width_by 就按边属性映射 ----
  # ⚠️ 固定宽度必须用 `linewidth`，映射宽度必须走 `aes(edge_width =)`；
  #    两者混用会得到一个「有图例但没有标度」的假通道。
  # 只在映射时补标度；色标由调用方另行添加（edge_scale）。
  # ⚠️ 返回 `list(layer, scale)` 是安全的：ggplot2 的 `+.gg` 会展开列表。
  w_scale <- if (is.null(width_by)) {
    NULL
  } else {
    ggraph::scale_edge_width(range = width_range, guide = "none")
  }

  if (is.null(colour_by)) {
    if (is.null(width_by)) {
      return(fn(colour = edge_color, alpha = sp$edge_alpha,
                linewidth = sp$edge_lw, show.legend = FALSE))
    }
    return(list(
      fn(ggplot2::aes(edge_width = !!rlang::sym(width_by)),
         colour = edge_color, alpha = sp$edge_alpha, show.legend = FALSE),
      w_scale
    ))
  }
  # ⚠️ 用 `!!rlang::sym()` 而不是 `.data[[colour_by]]`：与本文件节点图层同一理由
  #    （`.data[[x]]` 会把图例标题渲染成表达式文本）。
  if (is.null(width_by)) {
    return(fn(ggplot2::aes(edge_colour = !!rlang::sym(colour_by)),
              alpha = sp$edge_alpha, linewidth = sp$edge_lw))
  }
  list(
    fn(ggplot2::aes(edge_colour = !!rlang::sym(colour_by),
                    edge_width = !!rlang::sym(width_by)),
       alpha = sp$edge_alpha),
    w_scale
  )
}

#' 把连续 degree 离散分档（用于 hub 视图的 colour 通道）
#'
#' 为什么分档而不是连续渐变：hub 视图的标签是**全标**，若 colour 也用连续
#' 渐变，节点大小 + 颜色两条通道都在表达同一个连续量，读者无法把「大且深」
#' 拆成两个独立判断（R1 的「一个变量一个通道」）。分档把 degree 降成 4 个
#' 类别，颜色重新变成可数信息。
#' @noRd
.tcm_degree_bins <- function(deg, bins = 4L) {
  d <- suppressWarnings(as.numeric(deg))
  if (!length(d)) return(factor(character(0)))
  u <- unique(d[is.finite(d)])
  if (length(u) <= 1L) {
    return(factor(rep("degree", length(d)), levels = "degree"))
  }
  k <- max(2L, min(as.integer(bins), length(u)))
  br <- unique(stats::quantile(d, probs = seq(0, 1, length.out = k + 1L),
                               na.rm = TRUE, names = FALSE))
  if (length(br) < 2L) {
    return(factor(rep("degree", length(d)), levels = "degree"))
  }
  # 标签**直接从断点拼**，不要用 cut() 的默认 levels 再去正则清洗 ——
  # 实测那条路（gsub 去掉 ()/[]）会静默失败，图例上留下 "[1-4]" 这种残留符号。
  nb <- length(br) - 1L
  fmt <- function(x) sub("\\.0+$", "", format(x, trim = TRUE, scientific = FALSE))
  labs <- paste0(fmt(br[seq_len(nb)]), "-", fmt(br[-1L]))
  cut(d, breaks = br, include.lowest = TRUE, labels = labs)
}

#' 网络容量裁剪（view 感知），返回 list(net=, report=)
#'
#' 两个**正交**的收缩动作，顺序固定：
#' 1. `min_degree`：严格度数下限（`degree < min_degree` 的节点整体剔除）。
#'    与 `max_nodes` 无关，**任何** cap 取值下都生效。
#'    - 默认**单次**过滤后，再清掉「因下限而失去全部连线」的孤点
#'      （2026-10-28 起；否则图上会留孤零零一个点，与"度数下限"自相矛盾）。
#'    - `min_degree_iterate = TRUE` 时迭代到稳定（k-core）。**代价很大**：
#'      食管炎实测 113 → 27（10 轮，−76%），默认关。
#' 2. `max_nodes`：容量上限（超限才按度数保留 top N / ego 子网）。
#'
#' report 里 `floor_dropped`（下限共剔除多少，含孤点）、`floor_isolated`
#' （其中因失联被剔）、`floor_rounds`（迭代轮数）三个字段用于向用户交代。
#'
#' @noRd
.tcm_net_cap <- function(net, view, max_nodes = NULL,
                         min_degree = 0L, min_degree_iterate = FALSE,
                         keep = NULL) {
  vp <- .tcm_net_view_presets()[[view]]
  cap <- if (is.null(max_nodes)) vp$max_nodes else max_nodes
  keep <- if (is.null(keep)) vp$keep else keep

  total_n <- igraph::vcount(net)
  total_e <- igraph::ecount(net)

  # ---- ① 度数下限先行（2026-10-28 修 D1）--------------------------------
  # ⚠️ 旧实现的 `min_degree` 只在下游 `.tcm_filter_igraph()` 生效，而那里
  #    `vcount <= max_nodes` 时会提前 return → 「不想截断、只想按度数筛」
  #    （`max_nodes` 调大或 Inf）时参数**完全静默失效**，图照出 386 个点，
  #    用户只会怀疑整个工具。度数下限与容量上限是两个正交诉求，必须先各自
  #    独立生效（铁律 23：一个参数一个语义）。
  floored <- FALSE
  floor_dropped <- 0L
  floor_isolated <- 0L
  floor_rounds <- 0L
  md <- suppressWarnings(as.numeric(min_degree)[1L])
  if (is.finite(md) && md > 0) {
    keep_idx <- which(igraph::degree(net) >= md)
    if (!length(keep_idx)) {
      # 「下限高于全图最大度数」= 用户要的是一张空图：这必须报错，
      # 不能悄悄退回全图（那正是这次要修的静默失效）。
      stop(sprintf(
        paste0("min_degree = %s \u9ad8\u4e8e\u5168\u56fe\u6700\u5927\u5ea6\u6570 %d\uff0c",
               "\u6ca1\u6709\u4efb\u4f55\u8282\u70b9\u6ee1\u8db3\u3002\n",
               "  \u8bf7\u964d\u4f4e min_degree\uff08\u5f53\u524d %d \u4e2a\u8282\u70b9\uff09\u3002"),
        format(md), max(igraph::degree(net)), total_n
      ), call. = FALSE)
    }
    if (length(keep_idx) < total_n) {
      if (isTRUE(min_degree_iterate)) {
        # k-core：反复「按当前图重算度数 → 再筛」直到稳定。
        # ⚠️ 代价很大且不可逆：食管炎实测 113 → 27（10 轮，−76%），因为
        #    度≥6 的节点之间大量靠低度节点搭桥，桥一拆就级联掉。
        #    所以默认关（min_degree_iterate = FALSE），只有明确要 k-core 才开。
        repeat {
          keep_idx <- which(igraph::degree(net) >= md)
          if (!length(keep_idx)) {
            stop(sprintf(
              paste0("min_degree = %s \u8fed\u4ee3\u5230 %d \u8f6e\u65f6\u6240\u6709\u8282\u70b9",
                     "\u90fd\u88ab\u7b5b\u6389\u4e86\uff08k-core \u4e3a\u7a7a\uff09\u3002\n",
                     "  \u8bf7\u964d\u4f4e min_degree \u6216\u5173\u95ed min_degree_iterate\u3002"),
              format(md), floor_rounds
            ), call. = FALSE)
          }
          if (length(keep_idx) == igraph::vcount(net)) break
          net <- igraph::induced_subgraph(net, keep_idx)
          floor_rounds <- floor_rounds + 1L
          if (floor_rounds > 500L) break   # 理论上到不了，防死循环
        }
      } else {
        # 单次过滤（Cytoscape 的 Degree Filter 也是这一次）
        net <- igraph::induced_subgraph(net, keep_idx)
      }
      # 清掉因下限而**失去全部连线**的节点：它自身 degree≥md，但邻居全被筛掉。
      # 留着就是一个孤零零的圆点（食管炎 113 里有 1 个），而且与「度数下限」
      # 的语义自相矛盾。只清一次、不级联 —— 与上面的迭代模式无关。
      iso <- which(igraph::degree(net) == 0)
      if (length(iso)) {
        net <- igraph::delete_vertices(net, iso)
        floor_isolated <- length(iso)
      }
      # 拓扑相关属性必须清掉：留着就是「全图度数」挂在子图上，
      # 下游点大小 / 标签阈值会静默沿用旧口径（与下方 keep_attrs 同理）。
      for (a in intersect(c("degree", "size", "module", "degree_bin"),
                          igraph::vertex_attr_names(net))) {
        net <- igraph::delete_vertex_attr(net, a)
      }
      floored <- TRUE
      floor_dropped <- total_n - igraph::vcount(net)
    }
  }
  n_now <- igraph::vcount(net)
  e_now <- igraph::ecount(net)

  if (!is.finite(cap) || n_now <= cap) {
    return(list(net = net, report = list(
      shown_nodes = n_now, total_nodes = total_n,
      shown_edges = e_now, total_edges = total_e,
      # cap_used：实际生效的上限（view 预设 / 用户值 / 默认值都归一到这里），
      # 供调用方在 message 里说「上限多少、实际画了多少」（两者可能不等）。
      capped = FALSE, cap_used = cap,
      floored = floored, floor_dropped = floor_dropped,
      floor_isolated = floor_isolated, floor_rounds = floor_rounds
    )))
  }
  # ⚠️ `.tcm_filter_igraph` 用 `graph_from_data_frame()` 从**边表重建**图，
  #    顶点属性（class / module …）会全部丢失。旧代码靠调用方各自手工重挂
  #    `class`，接第二、第三个调用方时必然漏。这里统一保留：
  #    只搬「与拓扑无关」的属性；degree / size / score 这类**与图规模有关**的
  #    必须由调用方在裁剪后的图上重算，否则会静默沿用全图度数
  #    （表现为「点大小像全图排名、标签阈值却按子图算」）。
  keep_attrs <- setdiff(
    igraph::vertex_attr_names(net),
    c("name", "degree", "size", "module", "degree_bin")
  )
  old <- igraph::vertex_attr(net)
  # ⚠️ 这里传 `min_degree = 0`：严格下限已在 ① 应用过。`tcm_filter_network()`
  #    的 `min_degree` 是**另一套语义**（边规则：两端都低于下限才剔边），
  #    同一个名字在一条调用链上出现两套语义会互相抵消、也让报告口径说不清。
  out <- .tcm_filter_igraph(net, max_nodes = cap,
                            min_degree = 0L, keep = keep)
  if (length(keep_attrs)) {
    idx <- match(igraph::V(out)$name, as.character(old$name))
    for (a in keep_attrs) {
      # ⚠️ 不能用 `V(out)[[a]] <- ...`：那是在给**顶点序列**做下标赋值
      #    （`[[<-.igraph.vs` 期待的是顶点索引/名字），会报 "Invalid indexing."
      #    （2026-10-24 实测）。写属性必须走 set_vertex_attr()。
      out <- igraph::set_vertex_attr(out, a, value = old[[a]][idx])
    }
  }
  list(net = out, report = list(
    shown_nodes = igraph::vcount(out), total_nodes = total_n,
    shown_edges = igraph::ecount(out), total_edges = total_e,
    capped = TRUE, cap_used = cap,
    floored = floored, floor_dropped = floor_dropped,
    floor_isolated = floor_isolated, floor_rounds = floor_rounds
  ))
}

#' 度数下限的提示文案（三个入口共用，保证口径一致）
#'
#' 必须同时交代三件事，否则用户无法判断点了为什么少了：
#' 1. 是**下限**筛掉的（而不是容量上限）；
#' 2. 有没有**迭代**（k-core，会级联）、迭代了几轮；
#' 3. 其中几个是「自身够度数、但邻居被筛光」而失去全部连线的。
#'
#' @param label 调用方标识（如 `ppi_plot (style = "journal")`）
#' @param min_degree 用户传的下限
#' @param report `.tcm_net_cap()` 返回的 report
#' @noRd
.tcm_floor_msg <- function(label, min_degree, report) {
  # report 可能是旧调用方手工拼的（缺字段）—— 用 %||% 的等价写法兜底，
  # 不要引入 rlang 的空值算子（包内没有定义，R CMD check 会报未声明符号）。
  nz <- function(x) if (is.null(x) || !length(x) || is.na(x[1])) 0L else as.integer(x[1])
  iso <- nz(report$floor_isolated)
  rounds <- nz(report$floor_rounds)
  sprintf(
    "%s: min_degree = %s %s\u5df2\u5254\u9664 %d \u4e2a\u4f4e\u5ea6\u8282\u70b9%s\uff08%d \u2192 %d \u4e2a\u8282\u70b9\uff0c%d \u6761\u8fb9\uff09\u3002",
    label, format(min_degree),
    if (rounds > 0L) sprintf("\u8fed\u4ee3 %d \u8f6e\u5230\u7a33\u5b9a\uff08k-core\uff09\uff0c", rounds) else "",
    nz(report$floor_dropped),
    if (iso > 0L) sprintf("\uff0c\u5176\u4e2d %d \u4e2a\u56e0\u5931\u53bb\u5168\u90e8\u8fde\u7ebf\u88ab\u5254\u9664", iso) else "",
    nz(report$total_nodes), nz(report$shown_nodes), nz(report$shown_edges)
  )
}

#' 网络统一绘制（view × style 两个正交轴）
#'
#' @param net igraph 对象，已带 `degree` 顶点属性（可选带 `class`）
#' @param view "overview" / "cluster" / "hub"
#' @param style 外观预设名
#' @param node.size 节点大小范围（长度 2）
#' @param label.size 标签字号
#' @param graph.layout 布局；"auto" 表示用该 style 的推荐布局
#' @param node_base 基准色向量（供**有序**分档取色）
#' @param palette 配色体系 key（名义类别用它的离散模式）；`NULL` 时用 npg
#' @param algorithm 社区发现算法（仅 `view = "cluster"` / 颜色通道为 module 时用）
#' @param label_degree 显式度数阈值；非 NULL 时覆盖 view 的取法（旧版语义）
#' @param label_repel 标签是否 repel
#' @param label_extra 始终带标签的节点名（在 view 的标签规则之外并集）
#' @param edge_color 边颜色（`edge_colour_by` 为 NULL 时使用）
#' @param edge_geom 边几何："link" / "arc" / "fan" / "hive"（只改形状）
#' @param edge_colour_by 边属性名；非 NULL 时按它给边上色**并出图例**
#' @param edge_palette 边色板（按来源着色时用；给少了自动插值补足）
#' @param edge_alpha,edge_lw 显式覆盖 style 预设的边透明度 / 线宽
#' @param seed 布局随机种子（可复现）
#' @noRd
.tcm_net_plot <- function(net, view = "overview", style = "default",
                          node.size = c(2, 8), label.size = 4,
                          graph.layout = "auto", node_base = NULL,
                          palette = NULL, algorithm = "walktrap",
                          label_degree = NULL, label_repel = TRUE,
                          label_extra = NULL,
                          edge_color = "lightgrey",
                          edge_geom = "link", edge_colour_by = NULL,
                          edge_palette = NULL,
                          edge_alpha = NULL, edge_lw = NULL,
                          node_style = c("solid", "ringed"), node_stroke = 0.5,
                          edge_width_by = NULL,
                          edge_width_range = c(0.2, 2),
                          colour_by = NULL, label_top = NULL,
                          legend = c("auto", "none", "all"),
                          palette_colours = NULL, niter = NULL,
                          seed = 20261024L) {
  node_style <- match.arg(node_style)
  legend <- match.arg(legend)
  # ---- legend 三态语义（2026-10-28 修 D4）--------------------------------
  #   "auto" ：只给**颜色通道**出一份图例；size 不出（大小含义写图注）
  #   "all"  ：颜色 + size 两份（size 图例标题带 "(size)" 以区分同名图例）
  #   "none" ：全关
  # 旧实现只有 `== "all"` / `== "none"` 两个分支（三态实际两态），且两个通道
  # 都把 `show.legend` 打开，size 的 `guide = "none"` 又只关标度不关图层 →
  # 图上出现「有标题、无键」的残影图例，和 fill 色条重复成两份 "degree"。
  lg_size <- identical(legend, "all")
  lg_on <- !identical(legend, "none")
  vp <- .tcm_net_view_presets()[[view]]
  sp <- .tcm_net_style_presets()[[style]]
  if (is.null(sp)) sp <- .tcm_net_style_presets()[["default"]]
  # ---- 调用方覆盖 view 预设的两个字段 -----------------------------------
  # ppi_plot 自带一套 style 表（它有自己的 layout / size 范围 / 图例档位），
  # 需要在不新造 view 预设的前提下指定「颜色表什么」与「标几条」。
  if (!is.null(colour_by)) vp$colour <- colour_by
  if (!is.null(label_top)) vp$label_top <- label_top
  # 显式覆盖优先于 style 预设：旧调用方（venn_net）有 edge.alpha / edge.width
  # 参数，语义是「直接调这一个值」，不是「选档位」。
  if (!is.null(edge_alpha)) sp$edge_alpha <- as.numeric(edge_alpha)[1L]
  if (!is.null(edge_lw)) sp$edge_lw <- as.numeric(edge_lw)[1L]
  if (is.null(node_base)) node_base <- RColorBrewer::brewer.pal(8, "RdBu")

  if (is.null(igraph::V(net)$degree)) {
    igraph::V(net)$degree <- igraph::degree(net)
  }

  # ---- 颜色通道（由 view 决定，只此一个通道）----
  if (identical(vp$colour, "module")) {
    cl <- switch(algorithm,
      louvain  = igraph::cluster_louvain(net),
      infomap  = igraph::cluster_infomap(net),
      walktrap = igraph::cluster_walktrap(net),
      igraph::cluster_walktrap(net)
    )
    igraph::V(net)$module <- factor(igraph::membership(cl))
  }
  if (identical(vp$colour, "degree_bin")) {
    igraph::V(net)$degree_bin <- .tcm_degree_bins(igraph::V(net)$degree)
  }
  # ⚠️ `switch()` 无匹配时**静默返回 NULL**（不是报错）：漏一个档位会让
  #    `ccol` 变成 NULL，随后 `!ccol %in% ...` 报出难以理解的错误。
  #    这里显式列出四档并加守卫（2026-10-26 加 degree 时踩到）。
  ccol <- switch(vp$colour,
    class = "class", module = "module", degree_bin = "degree_bin",
    degree = "degree"
  )
  if (is.null(ccol)) {
    stop(sprintf("\u672a\u77e5\u7684 colour \u901a\u9053\uff1a%s\uff08\u53ef\u7528\uff1aclass / module / degree_bin / degree\uff09",
                 as.character(vp$colour)), call. = FALSE)
  }
  if (!ccol %in% igraph::vertex_attr_names(net)) {
    # 数据没给 class（如 ppi_plot 的单类型网络）时退回 degree 分档，
    # 保证 colour 通道永远有可用信息（而不是画成单色却留个空图例）。
    igraph::V(net)$degree_bin <- .tcm_degree_bins(igraph::V(net)$degree)
    ccol <- "degree_bin"
  }
  cv <- igraph::vertex_attr(net, ccol)
  is_disc <- !is.numeric(cv)
  lv <- if (is_disc) levels(factor(cv)) else character(0)
  n_lv <- length(lv)
  # 图例标题用**人能读的语义名**，而不是内部列名
  lg_name <- switch(ccol,
    class = "type", module = "module",
    degree_bin = "degree", degree = "degree", ccol
  )

  # ---- 布局（固定种子，可复现）----
  lay <- if (identical(graph.layout, "auto")) "fr" else graph.layout
  old_seed <- if (exists(".Random.seed", envir = globalenv()))
    get(".Random.seed", envir = globalenv()) else NULL
  set.seed(seed)
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv()))
        rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  # ⚠️ `niter` 只对**力导向**布局有意义，且 igraph 里只有 fr / graphopt 接受它。
  #    R2 集中布局时写死 `niter = 3000L` → `tcm_net(graph.layout = "kk")` 直接报
  #    "The `niter` argument of `layout_with_kk()` ... is now defunct"，
  #    而 nicely / circle / lgl / mds / tree / star / grid 会报 "unused argument"
  #    （2026-10-25 实测 10 种布局：仅 fr / graphopt 通过）。
  #    这里按**报错文本**精确回退，不用宽泛的 tryCatch 吞掉真错误。
  # `niter` 可由调用方指定（ppi_plot 的 style 表带自己的迭代数）；默认沿用 3000
  niter_use <- if (is.null(niter)) 3000L else as.integer(niter)[1L]
  lay_obj <- tryCatch(
    ggraph::create_layout(net, layout = lay, niter = niter_use),
    error = function(e) {
      if (!grepl("niter", conditionMessage(e), fixed = TRUE)) stop(e)
      ggraph::create_layout(net, layout = lay)
    }
  )
  # ★ 布局坐标归一化（2026-10-28 修 D2，见 .tcm_normalize_layout 说明）
  #   传 net 才能做「分量按节点数分配面积」那一层：只归一化到 bbox 不够 ——
  #   食管炎 520 节点 / 53 分量的实测里主分量只占画布宽 15%。
  lay_obj <- .tcm_normalize_layout(lay_obj, net = net)

  # ---- 色标 ----
  # ★ 名义 ≠ 有序：`class` / `module` 是无序类别 → 必须用**定性**调色板；
  #   只有 `degree_bin` 是有序分档才用顺序色阶。旧实现在 cluster 视图上把
  #   module 画成 RdBu 红→蓝渐变，读者会本能地在模块之间找「大小关系」——
  #   而那是个假信息（类别编号不代表大小）。
  # ⚠️ `node_style = "ringed"` 用 **fill** 上色（shape 21 才有填充），
  #    "solid" 用 colour（shape 19 只有描边色）。标度函数必须跟着换 ——
  #    写 `scale_colour_*` 配 fill 映射的结果是「节点全灰、图例空白」。
  sc_fun <- if (identical(node_style, "ringed")) {
    list(manual = ggplot2::scale_fill_manual,
         grad = ggplot2::scale_fill_gradientn)
  } else {
    list(manual = ggplot2::scale_colour_manual,
         grad = ggplot2::scale_colour_gradientn)
  }
  col_scale <- if (is_disc) {
    vals <- if (!is.null(palette_colours)) {
      # 调用方给显式色板（ppi 的 module 视图沿用 ColorBrewer Set2）
      rep_len(as.character(palette_colours), n_lv)
    } else if (identical(ccol, "degree_bin")) {
      grDevices::colorRampPalette(node_base)(max(2L, n_lv))
    } else if (!is.null(palette)) {
      suppressMessages(tcm_palette(palette, n_lv, "discrete"))
    } else {
      suppressMessages(tcm_palette("npg", n_lv, "discrete"))
    }
    sc_fun$manual(values = stats::setNames(vals, lv), name = lg_name)
  } else {
    sc_fun$grad(colours = node_base, name = lg_name)
  }

  # ---- 边色标（只在显式指定「按某条边属性着色」时才生成）------------------
  # 默认是单色边、无图例：边不承载信息时不该多一个图例（R2 的图例收敛原则）。
  # venn_net 是唯一需要它的调用方 —— 二分图里「边属于哪个来源」是真实信息，
  # 且**无法**由节点颜色表达（所有 source 节点的 class 相同）。
  edge_scale <- NULL
  if (!is.null(edge_colour_by)) {
    if (!edge_colour_by %in% igraph::edge_attr_names(net)) {
      stop(sprintf(
        paste0("edge_colour_by = \"%s\"\uff0c\u4f46\u56fe\u4e2d\u6ca1\u6709\u8fd9\u4e2a\u8fb9\u5c5e\u6027\u3002\n",
               "  \u53ef\u7528\u7684\u8fb9\u5c5e\u6027\uff1a%s"),
        edge_colour_by,
        paste(igraph::edge_attr_names(net), collapse = ", ")
      ), call. = FALSE)
    }
    ev <- as.character(igraph::edge_attr(net, edge_colour_by))
    e_lv <- sort(unique(ev))
    # ⚠️ 色数必须 >= 类别数，否则 build 阶段报
    #    "Insufficient values in manual scale. N needed but only M provided."
    #    ——旧 venn_net 的 `scale_edge_color_manual(values = edge.color)` 写死
    #    5 色，来源一多就整张图渲染失败（2026-10-25 实测：149 个水平直接报错）。
    e_vals <- if (!is.null(edge_palette)) {
      if (length(edge_palette) >= length(e_lv)) {
        edge_palette[seq_along(e_lv)]
      } else {
        grDevices::colorRampPalette(edge_palette)(length(e_lv))
      }
    } else {
      suppressMessages(
        tcm_palette(if (is.null(palette)) "npg" else palette,
                    length(e_lv), "discrete")
      )
    }
    edge_scale <- ggraph::scale_edge_colour_manual(
      values = stats::setNames(e_vals, e_lv), name = edge_colour_by
    )
  }

  # ---- 标签：显式阈值 > view 三档（top / all / cluster）----
  # 为什么显式阈值优先：GUI 侧 server.R 会先算出「前 12 个节点对应的度数阈值」
  # 再作为 label.degree 传进来（固定数量比绝对阈值/分位数稳定，2026-10-20 的结论）。
  # 那个语义正是旧版的「绝对度数阈值」，所以保留它 = 向后兼容，而不是另造一套。
  deg <- igraph::V(net)$degree
  vname <- igraph::V(net)$name
  .lab_layer <- function(thr = NULL, keep_names = NULL) {
    gg <- if (!is.null(keep_names)) {
      # ★ 「前 N 个」必须按**名字集合**取，不能按度数阈值取：
      #   度数大量打平时（二分图里 149 个基因多是 degree 1–2），
      #   第 N 名的阈值会把同分节点**全部**放进來。
      #   实测 venn_data：label_top = 15 实际标了 ~40 个，中心区域糊成一团。
      ggplot2::aes(filter = name %in% keep_names,
                   label = .tcm_net_trunc(name))
    } else if (is.null(thr)) {
      ggplot2::aes(label = .tcm_net_trunc(name))
    } else {
      ggplot2::aes(filter = degree >= thr, label = .tcm_net_trunc(name))
    }
    ggraph::geom_node_label(
      gg, size = sp$label_size, repel = isTRUE(label_repel),
      # ⚠️ ggplot2 4.0 起 GeomLabel 的边框宽度参数是 `linewidth`；
      #    旧名 `label.size` 会被**静默忽略**（不报错、不告警）。
      linewidth = 0.1, label.padding = ggplot2::unit(0.08, "lines"),
      fill = "white", colour = "grey15", alpha = 0.9, show.legend = FALSE
    )
  }
  lab_layer <- if (!is.null(label_degree) && length(label_degree) &&
                   is.finite(as.numeric(label_degree)[1L])) {
    .lab_layer(thr = as.numeric(label_degree)[1L])
  } else if (identical(vp$label_mode, "top")) {
    # 调用方可以把 label_top 覆盖成 Inf（全标）或 <= 0（不标）——
    # `as.integer(Inf)` 会得到 NA 并在 `seq_len()` 处炸，这里先归一化。
    # ⚠️ 这里**不能写 `return(NULL)`**：这段代码在 `.tcm_net_plot` 的函数体里，
    #    `return()` 会结束整个函数（图变成 NULL），2026-10-26 差点这样写。
    lt <- suppressWarnings(as.numeric(vp$label_top)[1L])
    if (is.null(vp$label_top) || is.na(lt) || lt <= 0) {
      NULL
    } else {
      n_lab <- if (is.finite(lt)) {
        max(1L, min(as.integer(lt), length(deg)))
      } else {
        length(deg)
      }
      ord <- order(-as.numeric(deg), vname)        # 同分时按名字，保证可复现
      # `label_extra`：必须**始终**带标签的节点名（二分图里就是所有 source 节点 ——
      # 它们是图的锚，被 top-N 挤掉后整张图读不出「基因属于哪个来源」）。
      keep <- union(vname[ord[seq_len(n_lab)]], as.character(label_extra))
      .lab_layer(keep_names = keep)
    }
  } else if (identical(vp$label_mode, "all")) {
    .lab_layer(NULL)
  } else {
    NULL  # cluster 视图另走 annotate（簇名画在质心）
  }

  # ---- 节点层：实心 or 白描边 ------------------------------------------
  # ⚠️ 用 `!!rlang::sym(ccol)` 而不是 `.data[[ccol]]`：后者会把图例标题
  #    渲染成 ".data[[ccol]]" 这种表达式文本（因为 aes 的 label 取的是
  #    未求值的表达式）。sym 注入后标题就是真实列名。
  # ⚠️ `show.legend` 用**命名逻辑向量**逐通道开关（ggplot2 支持
  #    `c(fill = TRUE, size = FALSE)`）：一个 geom 同时映射 fill 与 size 时，
  #    `show.legend = TRUE/FALSE` 只能整体开关，做不到「只出颜色图例」。
  node_layer <- if (identical(node_style, "ringed")) {
    # shape 21 = 有填充的圆：fill 上色 + 白色描边（Cytoscape 观感）
    ggraph::geom_node_point(
      ggplot2::aes(fill = !!rlang::sym(ccol), size = degree),
      shape = 21, colour = "white", stroke = node_stroke,
      show.legend = if (lg_on) c(fill = TRUE, size = lg_size) else FALSE
    )
  } else {
    ggraph::geom_node_point(
      ggplot2::aes(colour = !!rlang::sym(ccol), size = degree),
      alpha = 1,
      show.legend = if (lg_on) c(colour = TRUE, size = lg_size) else FALSE
    )
  }

  # size 图例（仅 legend = "all"）：必须显式给可见的填充/描边颜色。
  # ringed 节点的 fill 来自色标，在 size 图例里没有对应色值 → 默认画出一排
  # 白点，白底上等于「只有 4/8/12/16 四个数字、没有键」（2026-10-28 实测
  # presentation 档就是这个观感，等于图例白出）。
  size_guide <- if (lg_size) {
    ggplot2::guide_legend(
      order = 2,
      override.aes = list(fill = "grey40", colour = "grey40", stroke = 0.4)
    )
  } else {
    "none"
  }

  p <- ggraph::ggraph(lay_obj) +
    # ★ 单层边（R2 硬约束）：只画一次，宽度固定 —— 边不承载统计量大小，
    #   避免「同一批边画两遍」糊成一团（旧 default 的双绘边）。
    #   `edge_geom` 只改**形状**（直线/弧/扇形/hive），不改编码语义。
    .tcm_edge_layer(edge_geom, sp, edge_color,
                    net = net, colour_by = edge_colour_by,
                    width_by = edge_width_by,
                    width_range = edge_width_range) +
    node_layer +
    col_scale +
    edge_scale +
    ggplot2::scale_size(
      range = node.size,
      # "all" 时 size 图例标题写成 "degree (size)"：否则两张图例都叫 degree，
      # 读者无法判断哪一张是色阶、哪一张是点大小。
      name = if (lg_size) "degree (size)" else "degree",
      guide = size_guide
    ) +
    # 兜底：任何分支漏出 size 残影图例都在这里收口（单图只允许一份 degree 图例）
    ggplot2::guides(size = size_guide) +
    lab_layer +
    ggplot2::coord_fixed() +
    ggraph::theme_graph(background = "white", base_family = "sans") +
    ggplot2::theme(
      legend.position = if (identical(legend, "none")) "none" else "right",
      legend.key.size = ggplot2::unit(0.9, "lines"),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )

  # cluster 视图：只标簇名（不标节点名）
  if (identical(vp$label_mode, "cluster")) {
    xy <- as.data.frame(lay_obj)
    xy$module <- as.character(igraph::vertex_attr(net, "module"))
    # 只标**最大的若干个模块**：实测 16 个模块全标时 M1..M16 的质心标签会互相压
    # （尤其是小模块挤在大模块边缘）。小模块仍然着色，只是不出名字 —— 与
    # Cytoscape 的「簇多时只标主簇」一致。
    top_n <- if (is.null(vp$label_top)) Inf else vp$label_top
    sz <- sort(table(xy$module), decreasing = TRUE)
    keep_mod <- names(sz)[seq_len(min(length(sz), top_n))]
    xy <- xy[xy$module %in% keep_mod, , drop = FALSE]
    cen <- stats::aggregate(cbind(x, y) ~ module, data = xy, FUN = stats::median)
    p <- p + ggplot2::annotate(
      "label", x = cen$x, y = cen$y, label = paste0("M", cen$module),
      size = sp$label_size + 0.4, fontface = "bold",
      # ⚠️ ggplot2 4.0 起 GeomLabel 的边框宽度参数从 `label.size` 改名为
      #    `linewidth`；写旧名会被 ignore 并给出 "Ignoring unknown parameters"。
      linewidth = 0, label.padding = ggplot2::unit(0.25, "lines"),
      fill = "white", colour = "grey20", alpha = 0.85
    )
  }
  p
}

#' 布局坐标归一化（分量按节点数比例打包 + 中心化 + 缩放）
#'
#' 为什么必须有：力导向布局的坐标幅度完全由图结构与**分量间距离**决定。
#' `coord_fixed()` 锁定 1:1 长宽比后，整幅画布的尺度由全图 bbox 决定 ——
#' 主簇被压成画面一角的一小团。换 fr / kk / stress / nicely / graphopt 都一样，
#' 因为问题不在布局算法，而在「坐标没归一化」。
#'
#' **两层修复**（2026-10-28 实测：只做第二层不够）：
#' 1. `mem` 非空且存在多个连通分量时，先做**面积按节点数分配**的重新打包：
#'    分量内部几何（力导向算出的结构）保持不变，只改「每个分量占多大画布」。
#'    实测食管炎 520 节点 / 53 个分量：igraph 自带的 merge 把 386 节点主分量
#'    压到画布宽度 15%，而按节点数分配面积应得 ~74%（线性 ~86%）。
#' 2. 再做中心化 + 除以最大绝对坐标 → 画布恒为 `[-1, 1]`。
#'    对单连通分量网络近似恒等（整体等比缩放 ± 平移），相对形态不变。
#'
#' 为什么必须两层：只做 ② 时「全图 bbox 装得下所有分量」是满足了，但各分量
#' 占多大由 igraph 的 merge 决定，与节点数无关 —— 52 个两点小分量照样能吃掉
#' 85% 的画布。只做 ① 而不归一化，则分量间距离仍可能是任意量级。
#'
#' 不做分位数裁剪（裁掉 98% 之外的离群点）：那会改变「点是否落在框内」的
#' 语义，需要单独评估。
#'
#' @param lay_obj `ggraph::create_layout()` 的返回值（data.frame 子类）
#' @param net 可选，igraph 对象；给了才算连通分量并按面积比重排
#' @return 同类型对象，x / y 已归一化
#' @noRd
.tcm_normalize_layout <- function(lay_obj, net = NULL) {
  lay_df <- as.data.frame(lay_obj)
  if (!all(c("x", "y") %in% names(lay_df))) return(lay_obj)
  ok <- is.finite(lay_df$x) & is.finite(lay_df$y)
  if (!any(ok)) return(lay_obj)

  # ---- ① 分量按节点数分配面积（仅多分量时）----
  if (!is.null(net) && igraph::vcount(net) == nrow(lay_df)) {
    lay_df <- .tcm_pack_components(lay_df, net)
  }

  # ---- ② 全局中心化 + 缩放 ----
  ok <- is.finite(lay_df$x) & is.finite(lay_df$y)
  cx <- mean(range(lay_df$x[ok]))
  cy <- mean(range(lay_df$y[ok]))
  x <- lay_df$x - cx
  y <- lay_df$y - cy
  rr <- max(abs(c(x[ok], y[ok])))
  if (is.finite(rr) && rr > 0) {
    x <- x / rr
    y <- y / rr
  }
  lay_obj$x <- x
  lay_obj$y <- y
  lay_obj
}

#' 分量重新打包：主分量占主角，其余分量收成一条「卫星带」
#'
#' 为什么不是简单归一化：`create_layout()` 对多分量图会先分子量布局再 merge，
#' 每个分量拿到的画布面积与它的节点数**无关**。食管炎实测（520 节点 / 53 个
#' 分量）：386 节点的主分量只拿到画布宽度的 15%，而按节点数它该占 ~74%。
#' 只做「全图 bbox 归一化」修不了这件事 —— bbox 装得下所有分量了，但每个
#' 分量多大仍由 merge 决定。
#'
#' 排布规则（确定性，无随机、无调参）：
#' 1. **面积 ∝ 节点数**；但非主分量的**总面积封顶在主分量的 30%** ——
#'    否则 52 个两点分量能吃掉 85% 画布，主簇又缩回去。封顶后小分量之间
#'    仍按节点数等比，读者能看出谁比谁大。
#' 2. 主分量按**自己的长宽比**占左侧一块；其余分量在右侧一条
#'    「宽度 = 主分量 0.30」的竖带里**网格**排布。网格而不是货架：货架对
#'    五十来个两点分量空隙率能到 50%，排出来比主分量还高（实测卫星带高 57
#'    vs 主分量 21.7，归一化后主分量被压回 55%）。格子在排布前就算准，
#'    不会溢出画布。
#' 3. 卫星在格子内按 `sqrt(节点数)` 等比（上限是格子边长，下限 1/4 格）：
#'    既保留「谁比谁大」，又保证不少于可见像素。
#' 4. 分量内部坐标：中心化后按 x / y 分别缩放到它的盒子里。盒子长宽比就是
#'    该分量自己的长宽比，所以力导向算出的形状不变（不是拉成椭圆）。
#'
#' @param lay_df 含 x / y 的 data.frame（行序与 net 顶点序一致）
#' @param net igraph 对象
#' @noRd
.tcm_pack_components <- function(lay_df, net) {
  comp <- igraph::components(net)
  if (!is.finite(comp$no) || comp$no <= 1L) return(lay_df)

  mem <- comp$membership
  # 行序可能与顶点序不同（布局实现不保证）—— 有 name 列就按名字对齐
  if ("name" %in% names(lay_df) && !is.null(igraph::V(net)$name)) {
    mem <- mem[match(as.character(lay_df$name), igraph::V(net)$name)]
  }
  if (all(is.na(mem))) return(lay_df)

  ids <- unique(mem[!is.na(mem)])
  cnt <- vapply(ids, function(k) sum(!is.na(mem) & mem == k), 0L)
  # ⚠️ 排序**结果**与节点计数必须一起重排：只把 ids 排序、cnt 留在原顺序，
  #    再 `names(area) <- 排好的 key`，名字与数值就错位了（实测：13 节点的
  #    分量拿到 4 节点分量的面积，卫星大小完全乱序）。
  ord <- order(-cnt, vapply(ids, function(k) min(which(mem == k)), 0L))
  order_id <- ids[ord]
  cnt <- as.numeric(cnt)[ord]
  if (length(order_id) <= 1L) return(lay_df)

  key <- as.character(order_id)
  n_tot <- length(mem)
  full <- sqrt(n_tot)

  # ---- 面积：∝ 节点数，卫星合计封顶为主分量的 30% ----------------------
  area <- stats::setNames(as.numeric(cnt), key)
  big <- order_id[1L]
  sat_ids <- order_id[-1L]
  ks <- as.character(sat_ids)
  cap <- 0.30 * area[[as.character(big)]]
  # ⚠️ 必须按**名字**取（area 的顺序是 order_id，不是分量编号）：
  #    `area[sat_ids]` 会按位置取到别的分量，面积分配整体错位。
  sat_sum <- sum(area[ks])
  if (sat_sum > cap && sat_sum > 0) {
    area[ks] <- area[ks] / sat_sum * cap
  }
  # 单点分量也要留可见面积（画布线性的 2%，183 mm 图上约 3.6 mm）
  area <- pmax(area, (full * 0.02)^2)
  names(area) <- key

  # ---- 每个分量的盒子：面积 + 自身长宽比 ---------------------------------
  member_idx <- lapply(order_id, function(k) which(!is.na(mem) & mem == k))
  names(member_idx) <- key
  box <- lapply(key, function(k) {
    idx <- member_idx[[k]]
    sx <- diff(range(lay_df$x[idx]))
    sy <- diff(range(lay_df$y[idx]))
    asp <- if (is.finite(sy) && sy > 1e-12) sx / sy else 1
    if (!is.finite(asp) || asp <= 0) asp <- 1
    c(w = sqrt(area[[k]] * asp), h = sqrt(area[[k]] / asp))
  })
  names(box) <- key

  bw <- box[[as.character(big)]][["w"]]
  bh <- box[[as.character(big)]][["h"]]

  # ---- 卫星带：右侧竖条内的**网格**排布 ---------------------------------
  # 为什么用网格而不是「面积 ∝ 节点数的货架」：卫星动辄四五十个、每个只占
  # 2~3 个节点，货架的空隙率能到 50%，排出来比主分量还高（实测食管炎 53 分量
  # 时卫星带高度 57 > 主分量 21.7，归一化后主分量又被压回 55%）。
  # 网格的占用是**先算准的**：格子数由卫星个数与带的宽高比决定，绝不溢出。
  # 卫星之间的相对大小仍保留：格子内按 sqrt(节点数) 等比缩放（上限为格子）。
  m <- length(sat_ids)
  if (!m) return(lay_df)
  # 带的目标：宽度 = 主分量的 0.30（画布长宽比 ≈ 1.2，接近常见面板比例）
  strip_w <- 0.30 * bw
  strip_h <- bh
  aspect_cells <- strip_w / strip_h
  ncol <- max(1L, as.integer(round(sqrt(m * aspect_cells))))
  ncol <- min(ncol, m)
  nrow <- as.integer(ceiling(m / ncol))
  cell_w <- strip_w / ncol
  cell_h <- strip_h / nrow
  cell <- min(cell_w, cell_h)
  cnt_sat <- vapply(sat_ids, function(k) area[[as.character(k)]], 0)
  ref <- max(sqrt(cnt_sat), 1e-9)

  # ---- 落位 -------------------------------------------------------------
  x <- lay_df$x
  y <- lay_df$y
  place <- function(k, cx, cy, w, h) {
    # ⚠️ 必须按**字符**键：member_idx 是按分量大小排的，用整数 k 会按下标取到
    # 「第 k 大」的分量而不是编号 k 的那个分量（实测导致卫星大小与节点数反序）。
    idx <- member_idx[[as.character(k)]]
    xs <- lay_df$x[idx]
    ys <- lay_df$y[idx]
    xs <- xs - mean(range(xs))
    ys <- ys - mean(range(ys))
    rx <- max(abs(xs))
    ry <- max(abs(ys))
    if (is.finite(rx) && rx > 0) xs <- xs / rx * w / 2
    if (is.finite(ry) && ry > 0) ys <- ys / ry * h / 2
    x[idx] <<- xs + cx
    y[idx] <<- ys + cy
  }
  place(big, bw / 2, bh / 2, bw, bh)

  # 卫星逐个放进右侧网格：格子内按 sqrt(节点数) 等比，上限是格子边长
  for (i in seq_along(sat_ids)) {
    k <- sat_ids[i]
    r <- (i - 1L) %/% ncol
    cc <- (i - 1L) %% ncol
    cx <- bw + cc * cell_w + cell_w / 2
    cy <- r * cell_h + cell_h / 2
    stay <- cell * sqrt(cnt_sat[i]) / ref      # 小分量留更小（信息量即节点数）
    d <- min(cell, max(stay, cell * 0.25))     # 最小也留 1/4 格，避免成像素
    place(k, cx, cy, d, d)
  }

  lay_df$x <- x
  lay_df$y <- y
  lay_df
}


#' 长标签截断（IUPAC 分子名可达 100+ 字符，会把版面横向劈开）
#' @noRd
.tcm_net_trunc <- function(x, n = 24L) {
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1L), "\u2026"), x)
}
