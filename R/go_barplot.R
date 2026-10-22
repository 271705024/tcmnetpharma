#' Bar graphs showed the results of GO results
#'
#' @param go.diff R clusterprofiler package for enrichGO results
#' @param top  According to the order of P adjust value from small to large
#' the number of categories to show
#' @param plot.set "horizontal" or "vertical"
#' @param facet facet
#' @param color color, Recommended color:
#' colour1 <- c("#c0583d", "#daa152", "#b3c6dc")
#' colour2 <- c("#8fa1c7", "#ef916a", "#7dbfa5")
#' colour3 <- c("#7dbfa5", "#487eb3", "#d2342b")
#' colour4 <- c("#6176b5", "#DFC86B", "#87BD5B")
#' colour5 <- c("#852f88", "#eb990c", "#0f8096")
#' colour6 <- c("#3c4b8d", "#da2e20", "#3c884c")
#' colour7 <- c("#7aa354", "#d67d44", "#6594c4")
#' colour8 <- c("#501d8a", "#e55709", "#1c8041")
#' colour9 <- c("#A40545", "#7FCBA4", "#4B65AF")
#' @param bar.width  bar width
#' @param text.size  text size
#' @param text.width text width
#' @param title title
#' @param style visual preset. \code{"default"} keeps the original look.
#'   \code{"classic"} is the form most used in the GO literature: horizontal
#'   bars whose length is the gene \code{Count}, filled by ontology
#'   (BP/CC/MF), with the count printed at the bar end.
#'   \code{"grouped"} facets BP/CC/MF into separate panels, bar length =
#'   \code{Count}, fill = \code{p.adjust} gradient ("three-in-one" figure).
#'   \code{"significance"} keeps one panel and uses \code{-log10(p.adjust)}
#'   as both bar length and fill gradient.
#'   \code{"journal"} / \code{"presentation"} apply a publication /
#'   slide theme on top of the default form.
#'   **Only the visual form changes** — the selected terms, their counts and
#'   all statistics are identical across presets.
#' @param ... additional parameters
#'
#' @return bar plot
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 geom_text
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr slice
#' @importFrom tidyr drop_na
#' @importFrom stats reorder
#' @importFrom stringr str_wrap
#'
#' @examples
#' \dontrun{
#' # 富集结果：GO 离线即可，无需 OrgDb 包（等价 enrichGO(ont = "all")）
#' data(xfbdf, package = "tcmnetpharma")
#' res <- tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human",
#'                   pvalueCutoff = 0.01, qvalueCutoff = 0.05)
#' go.diff <- res$go
#' go_barplot(go.diff)
#' # 文献常用形制（2026-10-22 新增预设）：横条长度 = 基因数，按本体三色
#' go_barplot(go.diff, top = 10, style = "classic")
#' # BP/CC/MF 三栏分面 + p 值渐变（"三合一图"）
#' go_barplot(go.diff, top = 8, style = "grouped")
#' }
go_barplot <- function(go.diff,
                       top = 5,
                       plot.set = "vertical",
                       facet = TRUE,
                       color = c("#A40545", "#7FCBA4", "#4B65AF"),
                       bar.width = 0.8,
                       text.size = 12,
                       text.width = 30,
                       title = "GO enrichment of cluster",
                       style = c("default", "classic", "grouped", "significance",
                                 "journal", "presentation"),
                       ...) {
  style <- match.arg(style)
  # data processing
  if (isS4(go.diff)) {
    go.diff <- go.diff@result
  } else if (is.data.frame(go.diff)) {
    go.diff <- go.diff
  } else {
    print("The data format must be S4 object or data frame.")
  }

  go_enrich <- go.diff %>%
    tidyr::drop_na() %>%
    dplyr::group_by(ONTOLOGY) %>%
    dplyr::slice(1:top)
  go_enrich <- go_enrich[order(go_enrich$ONTOLOGY,
    go_enrich$p.adjust,
    decreasing = TRUE
  ), ]
  go_enrich$Description <- factor(go_enrich$Description,
    levels = go_enrich$Description
  )

  # ---- 文献形制预设（2026-10-22）-------------------------------------------
  # 文献调研（见 NEWS / 手册）：GO 富集图在中药网络药理学论文里**柱状图是主流**，
  # 常见三种形制 ——
  #   classic      x = Count（基因数），按本体三色，条端标 Count
  #                （clusterProfiler barplot(x="Count", split="ONTOLOGY") 观感）
  #   grouped      BP/CC/MF 分面，x = Count，fill = p.adjust 渐变（"三合一图"）
  #   significance x = -log10(p.adjust)，且按显著性渐变上色（单图）
  # ⚠️ 三者与 default **共用同一份 go_enrich**（同样每本体 top 个条目），
  #    只改「x 轴口径 / 配色映射 / 是否分面」——条目集合与统计量完全一致。
  if (style %in% c("classic", "grouped", "significance")) {
    return(.go_barplot_literature(go_enrich, style, color, bar.width,
                                  text.size, text.width, title))
  }

  # ggplot2 plotting
  if (plot.set == "vertical" && facet == FALSE) {
    p1 <- ggplot(go_enrich, aes(Description, -log10(p.adjust))) +
      geom_bar(aes(fill = ONTOLOGY),
        width = bar.width,
        stat = "identity"
      ) +
      geom_text(
        aes(
          label = round(-log10(p.adjust), digits = 0),
          y = round(-log10(p.adjust), digits = 0) + 1.5
        ),
        size = text.size / 3
      ) +
      coord_flip() +
      labs(x = "", y = "-log10 p.adjust", title = title) +
      labs(fill = "type") +
      scale_fill_manual(values = color) +
      theme_bw() +
      theme(
        axis.text.x = element_text(
          size = text.size,
          colour = "black"
        ),
        axis.text.y = element_text(
          size = text.size,
          colour = "black"
        )
      ) +
      theme(axis.title.x = element_text(size = text.size)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = text.width)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
    .tcm_style_apply(p1, style)
  } else if (plot.set == "vertical" && facet == TRUE) {
    p2 <- ggplot(go_enrich, aes(Description, -log10(p.adjust))) +
      geom_bar(aes(fill = ONTOLOGY),
        width = bar.width,
        stat = "identity"
      ) +
      geom_text(
        aes(
          label = round(-log10(p.adjust), digits = 0),
          y = round(-log10(p.adjust), digits = 0) + 1.5
        ),
        size = text.size / 3
      ) +
      coord_flip() +
      labs(x = "", y = "-log10 p.adjust", title = title) +
      labs(fill = "type") +
      scale_fill_manual(values = color) +
      theme_bw() +
      theme(
        axis.text.x = element_text(size = text.size, colour = "black"),
        axis.text.y = element_text(size = text.size, colour = "black")
      ) +
      theme(axis.title.x = element_text(size = text.size)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = text.width)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      theme(strip.text = element_text(face = "bold", size = text.size)) +
      facet_grid(ONTOLOGY ~ ., scales = "free_y", space = "free_y") +
      theme(legend.position = "none")
    .tcm_style_apply(p2, style)
  } else if (plot.set == "horizontal" && facet == FALSE) {
    p3 <- ggplot(go_enrich, aes(Description, -log10(p.adjust))) +
      geom_col(aes(fill = ONTOLOGY), width = bar.width) +
      geom_text(
        aes(
          label = round(-log10(p.adjust), digits = 0),
          y = round(-log10(p.adjust), digits = 0) + 1
        ),
        size = text.size / 3
      ) +
      scale_fill_manual(values = color) +
      theme(
        axis.text.x = element_text(
          size = text.size,
          colour = "black",
          angle = 60,
          hjust = 1,
          vjust = 1
        ),
        axis.text.y = element_text(
          size = text.size,
          colour = "black"
        )
      ) +
      theme(axis.title.y = element_text(size = text.size)) +
      theme(
        panel.grid = element_blank(),
        panel.background = element_rect(
          color = "black",
          fill = "transparent"
        )
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "", y = "-Log10 p.adjust") +
      labs(fill = "type") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = text.width))
    .tcm_style_apply(p3, style)
  } else if (plot.set == "horizontal" && facet == TRUE) {
    p4 <- ggplot(go_enrich, aes(Description, -log10(p.adjust))) +
      geom_col(aes(fill = ONTOLOGY), width = bar.width) +
      geom_text(
        aes(
          label = round(-log10(p.adjust), digits = 0),
          y = round(-log10(p.adjust), digits = 0) + 1
        ),
        size = text.size / 3
      ) +
      scale_fill_manual(values = color) +
      theme(
        axis.text.x = element_text(
          size = text.size,
          colour = "black",
          angle = 60,
          hjust = 1,
          vjust = 1
        ),
        axis.text.y = element_text(size = text.size, colour = "black")
      ) +
      theme(axis.title.y = element_text(size = text.size)) +
      theme(
        panel.grid = element_blank(),
        panel.background = element_rect(
          color = "black",
          fill = "transparent"
        )
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "", y = "-Log10 p.adjust") +
      labs(fill = "type") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = text.width)) +
      facet_grid(. ~ ONTOLOGY, scales = "free_x", space = "free_x") +
      theme(strip.text = element_text(
        face = "bold",
        size = text.size
      )) +
      theme(legend.position = "none")
    .tcm_style_apply(p4, style)
  } else {
    print("The plot.set must be 'horizontal' or 'vertical'.")
  }
}


# ---------------------------------------------------------------------------
# 文献形制预设（2026-10-22）
#
# 为什么单列成函数：三种形制的 x 轴口径不同（Count / Count / -log10 p），
# 塞进原 if-else 会让「plot.set × facet × style」爆成 3×2×3 = 18 分支，
# 可读性归零。这里只处理形制，主题统一交给 .tcm_style_apply（风格单一事实源）。
#
# ⚠️ 排序口径：classic 按 Count 升序（clusterProfiler 同款），
#    significance 按 -log10(p) 升序 —— 只改**显示顺序**，不改入选条目。
# ---------------------------------------------------------------------------
.go_barplot_literature <- function(go_enrich, style, color, bar.width,
                                   text.size, text.width, title) {
  lab_fun <- function(x) str_wrap(x, width = text.width)

  if (identical(style, "classic")) {
    # 文献最常见：横条 + 长度=基因数 + 本体三色 + 条端标数
    d <- go_enrich
    d$Description <- factor(d$Description,
                            levels = d$Description[order(d$Count, d$ONTOLOGY)])
    return(
      ggplot(d, aes(Count, Description, fill = ONTOLOGY)) +
        geom_col(width = bar.width) +
        geom_text(aes(label = Count), hjust = -0.25, size = text.size / 4) +
        scale_fill_manual(values = color, name = "Ontology") +
        scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
        scale_y_discrete(labels = lab_fun) +
        labs(x = "Gene count", y = NULL, title = title) +
        theme_bw() +
        theme(
          axis.text = element_text(size = text.size, colour = "black"),
          axis.title.x = element_text(size = text.size),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
        )
    )
  }

  if (identical(style, "grouped")) {
    # "三合一图"：BP/CC/MF 各一栏，长度=Count，颜色=p.adjust 渐变
    d <- go_enrich
    d$Description <- factor(d$Description,
                            levels = d$Description[order(d$Count, d$ONTOLOGY)])
    return(
      ggplot(d, aes(Count, Description, fill = p.adjust)) +
        geom_col(width = bar.width) +
        scale_fill_gradientn(
          colours = rev(RColorBrewer::brewer.pal(8, "RdBu")),
          trans = "log10",
          name = "p.adjust"
        ) +
        facet_grid(ONTOLOGY ~ ., scales = "free_y", space = "free_y") +
        scale_x_continuous(expand = expansion(mult = c(0, 0.10))) +
        scale_y_discrete(labels = lab_fun) +
        labs(x = "Gene count", y = NULL, title = title) +
        theme_bw() +
        theme(
          axis.text = element_text(size = text.size, colour = "black"),
          axis.title.x = element_text(size = text.size),
          strip.text = element_text(face = "bold", size = text.size),
          panel.grid.minor = element_blank()
        )
    )
  }

  # significance：单图，长度与颜色都是 -log10(p.adjust)
  d <- go_enrich
  d$neglogp <- -log10(d$p.adjust)
  d$Description <- factor(d$Description, levels = d$Description[order(d$neglogp)])
  ggplot(d, aes(neglogp, Description, fill = neglogp)) +
    geom_col(width = bar.width) +
    scale_fill_gradientn(
      colours = rev(RColorBrewer::brewer.pal(8, "RdBu")),
      name = "-log10(p.adjust)"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.10))) +
    scale_y_discrete(labels = lab_fun) +
    labs(x = "-log10(p.adjust)", y = NULL, title = title) +
    theme_bw() +
    theme(
      axis.text = element_text(size = text.size, colour = "black"),
      axis.title.x = element_text(size = text.size),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}
