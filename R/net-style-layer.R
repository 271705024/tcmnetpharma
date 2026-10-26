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
                            net = NULL, colour_by = NULL) {
  edge_geom <- match.arg(edge_geom, c("link", "arc", "fan", "hive"))
  fn <- switch(edge_geom,
    link = ggraph::geom_edge_link,
    arc  = ggraph::geom_edge_arc,
    fan  = ggraph::geom_edge_fan,
    hive = ggraph::geom_edge_hive
  )

  if (is.null(colour_by)) {
    return(fn(colour = edge_color, alpha = sp$edge_alpha,
              linewidth = sp$edge_lw, show.legend = FALSE))
  }
  # ⚠️ 用 `!!rlang::sym()` 而不是 `.data[[colour_by]]`：与本文件节点图层同一理由
  #    （`.data[[x]]` 会把图例标题渲染成表达式文本）。`do.call` 已无必要——
  #    mapping 在这里就构造好，直接位置参数传入。
  fn(ggplot2::aes(edge_colour = !!rlang::sym(colour_by)),
     alpha = sp$edge_alpha, linewidth = sp$edge_lw)
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
#' @noRd
.tcm_net_cap <- function(net, view, max_nodes = NULL,
                         min_degree = 0L, keep = NULL) {
  vp <- .tcm_net_view_presets()[[view]]
  cap <- if (is.null(max_nodes)) vp$max_nodes else max_nodes
  keep <- if (is.null(keep)) vp$keep else keep

  total_n <- igraph::vcount(net)
  total_e <- igraph::ecount(net)
  if (!is.finite(cap) || total_n <= cap) {
    return(list(net = net, report = list(
      shown_nodes = total_n, total_nodes = total_n,
      shown_edges = total_e, total_edges = total_e, capped = FALSE
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
  out <- .tcm_filter_igraph(net, max_nodes = cap,
                            min_degree = min_degree, keep = keep)
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
    shown_edges = igraph::ecount(out), total_edges = total_e, capped = TRUE
  ))
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
                          seed = 20261024L) {
  vp <- .tcm_net_view_presets()[[view]]
  sp <- .tcm_net_style_presets()[[style]]
  if (is.null(sp)) sp <- .tcm_net_style_presets()[["default"]]
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
  ccol <- switch(vp$colour,
    class = "class", module = "module", degree_bin = "degree_bin"
  )
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
    class = "type", module = "module", degree_bin = "degree", ccol
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
  lay_obj <- tryCatch(
    ggraph::create_layout(net, layout = lay, niter = 3000L),
    error = function(e) {
      if (!grepl("niter", conditionMessage(e), fixed = TRUE)) stop(e)
      ggraph::create_layout(net, layout = lay)
    }
  )

  # ---- 色标 ----
  # ★ 名义 ≠ 有序：`class` / `module` 是无序类别 → 必须用**定性**调色板；
  #   只有 `degree_bin` 是有序分档才用顺序色阶。旧实现在 cluster 视图上把
  #   module 画成 RdBu 红→蓝渐变，读者会本能地在模块之间找「大小关系」——
  #   而那是个假信息（类别编号不代表大小）。
  col_scale <- if (is_disc) {
    vals <- if (identical(ccol, "degree_bin")) {
      grDevices::colorRampPalette(node_base)(max(2L, n_lv))
    } else if (!is.null(palette)) {
      suppressMessages(tcm_palette(palette, n_lv, "discrete"))
    } else {
      suppressMessages(tcm_palette("npg", n_lv, "discrete"))
    }
    ggplot2::scale_colour_manual(values = stats::setNames(vals, lv), name = lg_name)
  } else {
    ggplot2::scale_colour_gradientn(colours = node_base, name = lg_name)
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
    n_lab <- max(1L, min(as.integer(vp$label_top), length(deg)))
    ord <- order(-as.numeric(deg), vname)          # 同分时按名字，保证可复现
    # `label_extra`：必须**始终**带标签的节点名（二分图里就是所有 source 节点 ——
    # 它们是图的锚，被 top-N 挤掉后整张图读不出「基因属于哪个来源」）。
    keep <- union(vname[ord[seq_len(n_lab)]], as.character(label_extra))
    .lab_layer(keep_names = keep)
  } else if (identical(vp$label_mode, "all")) {
    .lab_layer(NULL)
  } else {
    NULL  # cluster 视图另走 annotate（簇名画在质心）
  }

  p <- ggraph::ggraph(lay_obj) +
    # ★ 单层边（R2 硬约束）：只画一次，宽度固定 —— 边不承载统计量大小，
    #   避免「同一批边画两遍」糊成一团（旧 default 的双绘边）。
    #   `edge_geom` 只改**形状**（直线/弧/扇形/hive），不改编码语义。
    .tcm_edge_layer(edge_geom, sp, edge_color,
                    net = net, colour_by = edge_colour_by) +
    ggraph::geom_node_point(
      # ⚠️ 用 `!!rlang::sym(ccol)` 而不是 `.data[[ccol]]`：后者会把图例标题
      #    渲染成 ".data[[ccol]]" 这种表达式文本（因为 aes 的 label 取的是
      #    未求值的表达式）。sym 注入后标题就是真实列名（class / module /
      #    degree 分档），且不会因为中文而崩。
      ggplot2::aes(size = degree, colour = !!rlang::sym(ccol)), alpha = 1
    ) +
    col_scale +
    edge_scale +
    ggplot2::scale_size(range = node.size, name = "degree", guide = "none") +
    lab_layer +
    ggplot2::coord_fixed() +
    ggraph::theme_graph(background = "white", base_family = "sans") +
    ggplot2::theme(
      legend.position = "right",
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

#' 长标签截断（IUPAC 分子名可达 100+ 字符，会把版面横向劈开）
#' @noRd
.tcm_net_trunc <- function(x, n = 24L) {
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1L), "\u2026"), x)
}
