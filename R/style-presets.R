# ---------------------------------------------------------------------------
# 出图风格预设（2026-10-22）
#
# 为什么需要：boss 反馈「文献里 GO 富集柱状图用得比气泡图多，希望有预设可挑」，
# 且其余绘图模块也应有「期刊风 / 演示风」。PPI 与 tcm_net 已有 style 预设
# （.ppi_style_plot / .tcm_net_style_plot），但那些是**形制级**重绘；对
# bar_plot / degree_plot / tcm_sankey 这类图，重绘成本高、收益低，
# 真正需要统一的只是**外观主题**（去网格、细轴线、字号层级）。
#
# 本文件只提供两件事：
#   1. .tcm_style_generic  —— 通用主题型预设的合法取值；
#   2. .tcm_style_apply()  —— 后置修改任意 ggplot 对象的外观。
#
# 铁律（同 .ppi_style_plot 的口径）：**style 只改视觉，不改数据与统计口径**。
# 后置主题天然满足这一点（不碰 data、不改映射），故通用预设一律走这里，
# 不各写一份（铁律 54 单一事实源）。
# ---------------------------------------------------------------------------

# 通用主题型预设。形制型预设（classic / grouped / significance / genratio）
# 需要重建图层，由各图函数自己实现，不在此列。
.tcm_style_generic <- c("default", "journal", "presentation")

# 把通用主题套到 ggplot 上。未识别的取值原样返回（防御：不外抛，
# 因为调用点已用 match.arg 校验过，这里的兜底只为避免上游漏校验时崩图）。
.tcm_style_apply <- function(p, style) {
  if (is.null(style) || !length(style)) return(p)
  style <- as.character(style)[[1]]
  if (identical(style, "default")) return(p)

  if (identical(style, "journal")) {
    # 期刊风：白底无网格、细轴线、加粗分面标题、紧凑图例。
    # 参照 Elsevier/Springer 常见排版：图内不放大标题（标题写在图注里），
    # 故标题字号压到 11pt 并保留（便于快速识图，投稿时可在图注覆盖）。
    return(p + ggplot2::theme_classic(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11, face = "bold",
                                           hjust = 0, colour = "black"),
        axis.text = ggplot2::element_text(colour = "grey15"),
        axis.title = ggplot2::element_text(colour = "black"),
        axis.line = ggplot2::element_line(colour = "grey25", linewidth = 0.35),
        axis.ticks = ggplot2::element_line(colour = "grey25", linewidth = 0.35),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(face = "bold", colour = "black"),
        legend.key.size = ggplot2::unit(0.8, "lines"),
        legend.title = ggplot2::element_text(size = 9),
        legend.text = ggplot2::element_text(size = 8.5),
        plot.margin = ggplot2::margin(6, 8, 6, 6)
      ))
  }

  if (identical(style, "presentation")) {
    # 演示风：整体放大、轴线加重，投影/答辩场景后排也能看清。
    return(p + ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      axis.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 12.5, colour = "black"),
      strip.text = ggplot2::element_text(size = 13, face = "bold"),
      legend.title = ggplot2::element_text(size = 12.5),
      legend.text = ggplot2::element_text(size = 11.5),
      axis.line = ggplot2::element_line(linewidth = 0.6),
      axis.ticks = ggplot2::element_line(linewidth = 0.6),
      panel.grid.minor = ggplot2::element_blank()
    ))
  }

  p
}


# ---------------------------------------------------------------------------
# 【已移除】.tcm_inject_group_key()（2026-10-24）
#
# 该函数曾在这里把 source 列回填进 ONTOLOGY，以「让下游 group_by / fill /
# facet 逻辑完全不用改」。这个"最小侵入"的妥协在用户界面上以**语义污染**的
# 形式暴露了出来：
#
#   boss 2026-10-24 实测（数据是 WikiPathways）：
#     标题 -> "GO enrichment of cluster"     （身份错标）
#     图例 -> "Ontology: wikipathways"        （本体字段装着来源值）
#
# 省下的改动成本，最终由用户支付。现已由 R/enrich-source.R 取代：
#   - tcm_enrich_table()  把 source / source_label / ontology 建成三列独立字段
#   - .tcm_resolve_group() 显式决定"按来源还是按本体分组"
# 下游不再需要任何字段复用。
#
# 保留这段说明而不是静默删除，是为了让后来者知道为什么这里曾经有个函数。
# ---------------------------------------------------------------------------

