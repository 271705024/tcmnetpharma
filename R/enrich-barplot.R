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
# 内部：y 轴准备（唯一化 + 逆序，使第一行显示在最上方）
#
# 为什么要唯一化：不同来源可能有同名 term（如 GO 与 Reactome 都有
# "immune response"），直接拿 Description 当 factor 会串行。
# 用「分组 + 控制字符 + 描述」做 level 键，显示时再映射回 Description。
# ---------------------------------------------------------------------------
.enrich_bar_prep_y <- function(d, text.width = 30, desc = TRUE) {
  key <- paste0(as.character(d$.grp), "\u0001", as.character(d$Description))
  lv <- unique(key)
  # desc = TRUE 时「第一行显示在最上方」（barplot：最显著的在上）
  # desc = FALSE 时「第一行显示在最下方」（dotplot：richFactor 最大的在上）
  if (isTRUE(desc)) lv <- rev(lv)
  d$.ykey <- factor(key, levels = lv)
  # level -> 显示名（长条目按 text.width 折行）
  first <- !duplicated(key)
  lab <- as.character(d$Description)[first]
  w <- suppressWarnings(as.integer(text.width)[1L])
  if (!is.na(w) && w > 0L) lab <- stringr::str_wrap(lab, width = w)
  names(lab) <- key[first]
  d$.ylab <- lab
  # 位置：level 序号
  d$.pos <- as.integer(d$.ykey)
  d
}

#' y 轴标签函数（按 level 查显示名）
#' @noRd
.enrich_bar_ylab_fun <- function(d) {
  lab <- d$.ylab
  function(k) {
    out <- unname(lab[as.character(k)])
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
  d <- .enrich_bar_prep_y(d, text.width)
  ylab_fun <- .enrich_bar_ylab_fun(d)

  # 段边界 -> 水平分隔线（.pos 递减，故边界取 -0.5）
  runs <- rle(as.character(d$.grp))
  hl <- numeric(0)
  if (length(runs$lengths) > 1L) {
    ends <- cumsum(runs$lengths)[-length(runs$lengths)]
    hl <- d$.pos[ends] - 0.5
  }

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data$.value, y = .data$.ykey, fill = .data$.grp)
  ) +
    ggplot2::geom_col(width = bar.width, orientation = "y") +
    ggplot2::scale_fill_manual(
      values = cols, name = grp_name, breaks = names(cols), drop = FALSE
    ) +
    ggplot2::scale_y_discrete(labels = ylab_fun) +
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
  d <- .enrich_bar_prep_y(d, text.width)
  ylab_fun <- .enrich_bar_ylab_fun(d)

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data$.value, y = .data$.ykey, fill = .data$.grp)
  ) +
    ggplot2::geom_col(width = bar.width, orientation = "y") +
    ggplot2::scale_fill_manual(
      values = cols, name = grp_name, breaks = names(cols), drop = FALSE
    ) +
    ggplot2::scale_y_discrete(labels = ylab_fun) +
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
  d <- .enrich_bar_prep_y(d, text.width)
  ylab_fun <- .enrich_bar_ylab_fun(d)

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data$.value, y = .data$.ykey, fill = .data$.grp)
  ) +
    ggplot2::geom_col(width = bar.width, orientation = "y") +
    ggplot2::scale_fill_manual(
      values = cols, name = grp_name, breaks = names(cols), drop = FALSE
    ) +
    ggplot2::scale_y_discrete(labels = ylab_fun) +
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
