# ---------------------------------------------------------------------------
# 统一富集条形图（2026-10-24，R1）
#
# 为什么新建而不是继续改 go_barplot：
#   go_barplot 的 6 个 style 把「形制」（是否分面、x 轴是 Count 还是 -log10 p）
#   与「外观」（配色/字体/画布）混在一个枚举里，导致：
#     - classic / significance 两个分支**全局按数值排序**，BP/CC/MF 必然交错；
#     - 标题是形参默认值 "GO enrichment of cluster"，任何非 GO 数据都身份错标；
#     - 分组靠 .tcm_inject_group_key() 把 source 回填进 ONTOLOGY，语义污染。
#   本文件把三件事拆开：形制 layout / 条目数 top / 外观 style，三者正交。
#
# 设计依据（见 docs/plot-module-audit-2026-10-24.md §3）：
#   - Enrichr   默认 top 10，同一结果多视图切换
#   - OmicShare 分组柱状图（grouped bar charts for GO enrichment）
#   - clusterProfiler  barplot(split="ONTOLOGY") + facet_grid
#   - Metascape  去掉冗余后每簇一条，标题/图注与数据一致
#   boss 决策（2026-10-24）：GO 默认「单轴按本体分段」，其余形制做可选预设。
# ---------------------------------------------------------------------------

#' 富集条形图（按来源 / 本体分组，标题从数据派生）
#'
#' 统一入口：同一份富集结果，按 \code{source}（GO / KEGG / Reactome /
#' WikiPathways）与 \code{ontology}（BP / CC / MF）自动决定分组方式，
#' **绝不把 BP、CC、MF 按数值大小混排**。
#'
#' @param data \code{\link{tcm_enrich_table}()} 的输出、\code{tcm_enrich()} 的
#'   返回、单个 \code{enrichResult}，或 data.frame（含 \code{Description} /
#'   \code{p.adjust}；\code{source} 列可选）。
#' @param measure 条形长度口径：
#'   \code{"neglogp"}（默认，\code{-log10(p.adjust)}）、
#'   \code{"count"}（基因数，文献最常见）、
#'   \code{"generatio"}（\code{Count / 背景数}）。
#' @param layout 形制（「怎么不混排」）：
#'   \code{"segmented"}（默认）单轴按分组分段，段间画分隔线、段内各自排序；
#'   \code{"facet"} 每组分一栏（\code{clusterProfiler} 三合一图）；
#'   \code{"flat"} 单面板不分段（仅单组时有意义）。
#' @param top 每组条目数。数字，或预设名 \code{"compact"}=5 /
#'   \code{"standard"}=10 / \code{"full"}=20。
#' @param sort_by 组内排序依据：\code{"p.adjust"}（默认，越小越靠前）或
#'   \code{"Count"}（越大越靠前）。
#' @param color 分组底色（默认三色对应 BP/CC/MF）。分组数超过给色数时按色相插值补足。
#' @param bar.width 条宽。
#' @param text.size 字号。
#' @param text.width 条目名换行宽度。
#' @param show_value 是否在条端标数值。
#' @param title 图标题。\code{NULL}（默认）时**从数据派生**（如
#'   \code{"KEGG pathway enrichment"}、\code{"GO enrichment (BP / CC / MF)"}）。
#' @param legacy_title 设为 \code{TRUE} 时标题强制为旧版的
#'   \code{"GO enrichment of cluster"}，用于兼容已存档脚本。默认 \code{FALSE}。
#' @param style 外观预设：\code{"default"} / \code{"journal"} / \code{"presentation"}。
#'   **只改配色、字体、画布，不改数据与排序**。
#' @param palette 配色方案键，见 \code{\link{tcm_palette_names}()}；
#'   设定后覆盖 \code{color}。
#'
#' @return ggplot 对象。
#'
#' @examples
#' \dontrun{
#' data(xfbdf, package = "tcmnetpharma")
#' res <- tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human")
#'
#' # GO：默认单轴按 BP / CC / MF 分段，标题自动写 GO
#' enrich_barplot(res$go)
#'
#' # 换成三栏分面（文献三合一图）
#' enrich_barplot(res$go, layout = "facet", measure = "count")
#'
#' # 通路来源：自动按来源分组，标题自动写对应库名
#' enrich_barplot(res$reactome)
#' }
#' @export
enrich_barplot <- function(data,
                           measure = c("neglogp", "count", "generatio"),
                           layout = c("segmented", "facet", "flat"),
                           top = "standard",
                           sort_by = c("p.adjust", "Count"),
                           color = c("#A40545", "#7FCBA4", "#4B65AF"),
                           bar.width = 0.8,
                           text.size = 12,
                           text.width = 30,
                           show_value = TRUE,
                           title = NULL,
                           legacy_title = FALSE,
                           style = c("default", "journal", "presentation"),
                           palette = NULL) {
  measure <- match.arg(measure)
  layout <- match.arg(layout)
  sort_by <- match.arg(sort_by)
  style <- match.arg(style)
  palette <- .tcm_palette_check(palette)

  # ---- 1. 数据标准化（source / source_label / ontology 三列相互独立）-------
  tb <- tcm_enrich_table(data)
  if (!is.data.frame(tb) || !nrow(tb))
    stop("No enrichment rows found in `data`.", call. = FALSE)
  need <- c("Description", "p.adjust")
  miss <- setdiff(need, names(tb))
  if (length(miss))
    stop("`data` is missing required column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)

  # ---- 2. 分组（来源 / 本体，由数据决定）----------------------------------
  grp <- .tcm_resolve_group(tb)
  tb$.grp <- as.character(grp$key)
  grp_name <- grp$name

  # ---- 3. 分组取 top + 组内排序（分组与排序正交）-------------------------
  n_top <- .tcm_top_n(top)
  d <- .tcm_pick_top(
    tb, n = n_top, by = sort_by, group = ".grp",
    decreasing = identical(sort_by, "Count"), strict = TRUE
  )
  if (!nrow(d))
    stop("No terms left after grouping/slicing.", call. = FALSE)

  # ---- 4. 度量与数值标签 ---------------------------------------------------
  d$.value <- .tcm_bar_value(d, measure)
  xlab <- .tcm_bar_xlab(measure)
  d$.label <- if (identical(measure, "count")) {
    as.character(d$Count)
  } else {
    formatC(round(d$.value, 1), format = "f", digits = 1)
  }

  # ---- 5. 标题：身份信息绝不来自字面量默认值 ------------------------------
  ttl <- if (isTRUE(legacy_title)) {
    "GO enrichment of cluster"
  } else if (!is.null(title) && length(title) && nzchar(as.character(title)[[1L]])) {
    as.character(title)[[1L]]
  } else {
    .tcm_enrich_title(d)
  }

  # ---- 6. 分组配色 ---------------------------------------------------------
  grp_lv <- unique(as.character(d$.grp))
  n_g <- length(grp_lv)
  cols <- if (!is.null(palette)) {
    tcm_palette(palette, n_g)
  } else if (n_g <= length(color)) {
    color[seq_len(n_g)]
  } else {
    grDevices::colorRampPalette(color)(n_g)
  }
  names(cols) <- grp_lv

  # ---- 7. 形制分派 ---------------------------------------------------------
  args <- list(
    d = d, cols = cols, grp_name = grp_name, xlab = xlab,
    bar.width = bar.width, text.size = text.size, text.width = text.width,
    show_value = show_value, ttl = ttl
  )
  p <- switch(layout,
    segmented = do.call(.enrich_bar_segmented, args),
    facet     = do.call(.enrich_bar_facet, args),
    flat      = do.call(.enrich_bar_flat, args)
  )

  .tcm_style_apply(p, style)
}

# ---------------------------------------------------------------------------
# 内部：度量口径
# ---------------------------------------------------------------------------

#' 条形长度取值
#' @noRd
.tcm_bar_value <- function(d, measure) {
  if (identical(measure, "count")) {
    v <- suppressWarnings(as.numeric(d$Count))
    v[is.na(v)] <- 0
    return(v)
  }
  if (identical(measure, "generatio")) {
    gr <- if ("GeneRatio" %in% names(d)) as.character(d$GeneRatio) else NA_character_
    den <- suppressWarnings(as.numeric(sub("/.*$", "", gr)))
    den[!is.finite(den) | den <= 0] <- NA_real_
    v <- suppressWarnings(as.numeric(d$Count)) / den
    v[!is.finite(v)] <- 0
    return(v)
  }
  p <- suppressWarnings(as.numeric(d$p.adjust))
  p[!is.finite(p) | p <= 0] <- .Machine$double.xmin
  -log10(p)
}

#' x 轴标签
#' @noRd
.tcm_bar_xlab <- function(measure) {
  switch(measure,
    count     = "Gene count",
    generatio = "Gene ratio",
    "-log10(p.adjust)"
  )
}

# ---------------------------------------------------------------------------
# 内部：y 轴准备（唯一化 + 按标签行数分配槽位）
#
# 为什么要唯一化：不同来源可能有同名 term（如 GO 与 Reactome 都有
# "immune response"），直接拿 Description 当 factor 会串行。
# 用「分组 + 控制字符 + 描述」做 level 键，显示时再映射回 Description。
#
# ★ 为什么要分配槽位（2026-10-24 R2.4）：
#   ggplot 的**离散** y 位置间距恒为 1 个 level。`str_wrap` 换行后的标签以该
#   位置**垂直居中**渲染，行数 × 行高 > 1 个 level 时必然压到邻居 —— 画布加高
#   也救不了（因为间距与行数无关）。实测 24 条 × text.width=34 出图顶部 3 条重叠。
#
#   解法：给第 i 条分配 `k_i` 行（k_i = 它的标签行数），在它上下插入空白
#   placeholder level：上方 ceil((k_i-1)/2) 个、下方 floor((k_i-1)/2) 个。
#   于是每条占的 level 数 = 行数，标签永远不会压到邻居。
#
#   为什么不用连续坐标轴（`scale_y_continuous`）：那样柱宽要按 level 折算，
#   facet 的 `space="free_y"` 也要重算面板高度。placeholder 方案不动坐标轴类型，
#   柱宽仍是 0.8 个 level，改动面小得多。
#
#   ⚠️ 单轴形制用 `scale_y_discrete(drop = FALSE)`：placeholder level 没有数据行，
#      `drop = TRUE`（默认）会把它们全部丢掉，槽位就白插了。
#
#   ⚠️⚠️ 分面形制（layout="facet"）不能用 `drop = FALSE`：它是**全局**锁 limits，
#      于是每个面板都会吞进别组的 placeholder —— 实测 8 条/组、3 组的面板
#      从 16 槽位被撑到 48 槽位（3 倍空白）。补 NA 行也没用（limits 已锁死）。
#      正确做法：`drop = TRUE` + 给每组补**该组自己区间内** placeholder 的
#      0 宽柱行（`.value = 0`），让 level 在该面板里"有数据"从而被保留，
#      而别的面板看不到它。前提是**分组在 level 轨道上连续** —— `.tcm_pick_top`
#      按 `.grp` 排序保证了这一点（segmented 的分隔线依赖同一前提）。
# ---------------------------------------------------------------------------
.enrich_bar_prep_y <- function(d, text.width = 30, desc = TRUE, facet = FALSE) {
  key <- paste0(as.character(d$.grp), "\u0001", as.character(d$Description))
  lv <- unique(key)
  # desc = TRUE 时「第一行显示在最上方」（barplot：最显著的在上）
  # desc = FALSE 时「第一行显示在最下方」（dotplot：richFactor 最大的在上）
  # 下面的 lv 一律是「从下到上」的 level 顺序。
  if (isTRUE(desc)) lv <- rev(lv)

  # 每条目的折行文本 + 行数
  lab_item <- as.character(d$Description)
  w <- suppressWarnings(as.integer(text.width)[1L])
  if (!is.na(w) && w > 0L) lab_item <- stringr::str_wrap(lab_item, width = w)
  first <- !duplicated(key)
  lab_item <- lab_item[first][match(lv, key[first])]
  nl <- vapply(strsplit(lab_item, "\n", fixed = TRUE), length, integer(1))
  nl[!is.finite(nl) | nl < 1L] <- 1L

  # 从下往上铺设 level 轨道
  below <- floor((nl - 1L) / 2)
  above <- ceiling((nl - 1L) / 2)
  pos <- integer(length(lv))
  off <- 0L
  for (i in seq_along(lv)) {
    off <- off + below[i]          # 本条下方的 placeholder
    pos[i] <- off + 1L             # 本条自身
    off <- off + 1L + above[i]     # 越过本条及其上方的 placeholder
  }
  n_levels <- as.integer(off)   # ⚠️ floor() 派生的是 double；转整型，
                                #    否则 n_levels 与 sum(nl)（integer）类型不符

  # level 轨道：真实 key 放在 pos[i]，其余位置是 placeholder
  # ⚠️ placeholder 的 level 名必须以 \u0002 开头、与真实 key（含 \u0001）不冲突；
  #    且**不能**用空字符串（factor 的 levels 不能重复）。
  lev <- sprintf("\u0002sp%04d", seq_len(n_levels))
  lev[pos] <- lv
  # 标签映射：先按 lev 建满，再把条目位置填上折行文本。
  # 必须用 key（lv）而不是值去填，否则 `labs[[key]]` 查不到 → 又退回显示原始 key。
  lab_full <- stats::setNames(rep("", n_levels), lev)
  lab_full[lv] <- lab_item

  d$.ykey <- factor(key, levels = lev)
  # 只存原始文本（供诊断）；真正的 level→显示名映射由
  # .enrich_bar_ylab_fun(lab_full) 负责（参数里的 names 不会被 data.frame 剥掉）。
  d$.ylab_txt <- as.character(d$Description)
  d$.n_lines <- nl[match(key, lv)]
  d$.pos <- as.integer(d$.ykey)

  # ---- 分面形制：按组补 placeholder 的 0 宽柱行（原因见上方注释）----------
  # ⚠️ 这些行只喂给**图层**，不进 `p$data`：`p$data` 是"数据"，用户/测试/
  #    tcm_plot_size 都会读它，塞进来的幽灵行会污染语义
  #    （2026-10-24 实测：test-style-presets.R 的
  #      `nrow(p$data) == length(items_of(p))` 立刻失败）。
  drop <- FALSE
  d_render <- d
  if (isTRUE(facet)) {
    g <- as.character(d$.grp)
    add <- lapply(unique(g), function(gg) {
      rng <- range(d$.pos[g == gg])
      ph <- setdiff(lev[rng[[1L]]:rng[[2L]]], key)
      if (!length(ph)) return(NULL)
      out <- d[rep(1L, length(ph)), , drop = FALSE]
      out$.grp <- gg
      out$.ykey <- factor(ph, levels = lev)
      # 0 宽柱 → 几何上不可见；标签用 ""（不是 NA）避免 geom_text 报
      # "Removed N rows containing missing values" 的噪音。
      out$.value <- 0
      out$.label <- ""
      out$.ylab_txt <- NA_character_
      out$.n_lines <- NA_integer_
      out$.pos <- match(ph, lev)
      out
    })
    add <- do.call(rbind, add)
    if (!is.null(add) && nrow(add)) d_render <- rbind(d, add)
    drop <- TRUE
  }

  list(d = d, d_render = d_render, labs = lab_full, breaks = lv,
       n_levels = n_levels, n_lines_total = sum(nl), n_items = length(lv),
       drop = drop)
}

#' y 轴标签函数（按 level 查显示名）
#'
#' ⚠️ 不要传「带 names 的向量存进 data.frame 再取回来」：data.frame 的列会
#' **剥离 names 属性**（2026-10-24 实测），于是 `lab[k]` 全部返回 NA、fallback
#' 返回原始 key，y 轴显示成 `BP\u0001term` 这种字符串，而图仍能出、不报错。
#' 所以 lab_full 作为**函数参数**传入（参数里的 names 不会被剥）。
#' @noRd
.enrich_bar_ylab_fun <- function(labs) {
  function(k) {
    out <- unname(labs[as.character(k)])
    out[is.na(out)] <- as.character(k)[is.na(out)]
    out
  }
}

#' 公共主题（只放与形制无关的部分）
#' @noRd
.enrich_bar_theme <- function(text.size) {
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = text.size, colour = "black"),
      axis.title.x = ggplot2::element_text(size = text.size),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# ---------------------------------------------------------------------------
# 形制 1：segmented（默认）—— 单轴，按分组分段
#
# 视觉上是一张图（boss 要的「单 x 轴」），但组与组之间画浅灰分隔线，
# 且**组内各自按 sort_by 排序**，因此不会出现 BP/CC/MF 交错。
# ---------------------------------------------------------------------------
.enrich_bar_segmented <- function(d, cols, grp_name, xlab, bar.width,
                                  text.size, text.width, show_value, ttl) {
  py <- .enrich_bar_prep_y(d, text.width)
  d <- py$d
  ylab_fun <- .enrich_bar_ylab_fun(py$labs)

  # 段边界 -> 水平分隔线。
  # ⚠️ 不能用 `d$.pos[ends] - 0.5`：插入 placeholder 槽位后 .pos 不再连续，
  #    减 0.5 会落在某个 placeholder 上、偏离真正的段间空隙。
  #    取「上一组最后一条」与「下一组第一条」的中点，无槽位时与旧式完全等价。
  runs <- rle(as.character(d$.grp))
  hl <- numeric(0)
  if (length(runs$lengths) > 1L) {
    ends <- cumsum(runs$lengths)[-length(runs$lengths)]
    hl <- (d$.pos[ends] + d$.pos[ends + 1L]) / 2
  }

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data$.value, y = .data$.ykey, fill = .data$.grp)
  ) +
    # ⚠️ 图层用 py$d_render（分面形制里含 placeholder 的 0 宽柱行），
    #    `p$data` 仍是干净的 d —— 见 .enrich_bar_prep_y() 里的说明。
    ggplot2::geom_col(data = py$d_render, width = bar.width,
                      orientation = "y") +
    ggplot2::scale_fill_manual(
      values = cols, name = grp_name, breaks = names(cols), drop = FALSE
    ) +
    # ⚠️ drop 由 .enrich_bar_prep_y() 决定，不能写死：
    #    单轴形制（segmented/flat）用 drop = FALSE 保住 placeholder 槽位；
    #    分面形制（facet）必须 drop = TRUE，否则全局 limits 会把别组的槽位
    #    灌进每个面板（实测 16 槽位 → 48 槽位，3 倍空白）。
    ggplot2::scale_y_discrete(breaks = py$breaks, labels = ylab_fun,
                                drop = py$drop) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = xlab, y = NULL, title = ttl) +
    .enrich_bar_theme(text.size)

  if (isTRUE(show_value)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$.label),
      hjust = -0.18, size = text.size / 4
    )
  }
  if (length(hl)) {
    p <- p + ggplot2::geom_hline(
      yintercept = hl, colour = "grey80", linewidth = 0.35
    )
  }
  p
}

# ---------------------------------------------------------------------------
# 形制 2：facet —— 每组分一栏（clusterProfiler 三合一图 / OmicShare 分组柱）
# ---------------------------------------------------------------------------
.enrich_bar_facet <- function(d, cols, grp_name, xlab, bar.width,
                              text.size, text.width, show_value, ttl) {
  py <- .enrich_bar_prep_y(d, text.width, facet = TRUE)
  d <- py$d
  ylab_fun <- .enrich_bar_ylab_fun(py$labs)

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data$.value, y = .data$.ykey, fill = .data$.grp)
  ) +
    # ⚠️ 图层用 py$d_render（分面形制里含 placeholder 的 0 宽柱行），
    #    `p$data` 仍是干净的 d —— 见 .enrich_bar_prep_y() 里的说明。
    ggplot2::geom_col(data = py$d_render, width = bar.width,
                      orientation = "y") +
    ggplot2::scale_fill_manual(
      values = cols, name = grp_name, breaks = names(cols), drop = FALSE
    ) +
    # ⚠️ drop 由 .enrich_bar_prep_y() 决定，不能写死：
    #    单轴形制（segmented/flat）用 drop = FALSE 保住 placeholder 槽位；
    #    分面形制（facet）必须 drop = TRUE，否则全局 limits 会把别组的槽位
    #    灌进每个面板（实测 16 槽位 → 48 槽位，3 倍空白）。
    ggplot2::scale_y_discrete(breaks = py$breaks, labels = ylab_fun,
                                drop = py$drop) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$.grp),
      scales = "free_y", space = "free_y"
    ) +
    ggplot2::labs(x = xlab, y = NULL, title = ttl) +
    .enrich_bar_theme(text.size) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = text.size),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA)
    )

  if (isTRUE(show_value)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$.label),
      hjust = -0.18, size = text.size / 4
    )
  }
  p
}

# ---------------------------------------------------------------------------
# 形制 3：flat —— 单面板不分段（仅单组时有意义；多组时等同于不分组排序）
# ---------------------------------------------------------------------------
.enrich_bar_flat <- function(d, cols, grp_name, xlab, bar.width,
                             text.size, text.width, show_value, ttl) {
  py <- .enrich_bar_prep_y(d, text.width)
  d <- py$d
  ylab_fun <- .enrich_bar_ylab_fun(py$labs)

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data$.value, y = .data$.ykey, fill = .data$.grp)
  ) +
    # ⚠️ 图层用 py$d_render（分面形制里含 placeholder 的 0 宽柱行），
    #    `p$data` 仍是干净的 d —— 见 .enrich_bar_prep_y() 里的说明。
    ggplot2::geom_col(data = py$d_render, width = bar.width,
                      orientation = "y") +
    ggplot2::scale_fill_manual(
      values = cols, name = grp_name, breaks = names(cols), drop = FALSE
    ) +
    # ⚠️ drop 由 .enrich_bar_prep_y() 决定，不能写死：
    #    单轴形制（segmented/flat）用 drop = FALSE 保住 placeholder 槽位；
    #    分面形制（facet）必须 drop = TRUE，否则全局 limits 会把别组的槽位
    #    灌进每个面板（实测 16 槽位 → 48 槽位，3 倍空白）。
    ggplot2::scale_y_discrete(breaks = py$breaks, labels = ylab_fun,
                                drop = py$drop) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = xlab, y = NULL, title = ttl) +
    .enrich_bar_theme(text.size)

  if (isTRUE(show_value)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$.label),
      hjust = -0.18, size = text.size / 4
    )
  }
  p
}
