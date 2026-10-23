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
# 富集图分组键注入（2026-10-23）
#
# 背景：go_barplot / go_dotplot 的历史实现假设输入必有 ONTOLOGY 列（GO 的
# BP/CC/MF 三本体），用它做 group_by / fill / facet。但 KEGG / Reactome /
# WikiPathways 的富集结果**没有 ONTOLOGY 列**（只有 source 列），直接喂进去
# 会在 group_by(ONTOLOGY) 处静默失效或报错，这正是「KEGG 跑通却画不了图」的
# 包层根因之一。
#
# 策略（最小侵入）：只在 ONTOLOGY **缺失**时，用 source 列补一个同名列。这样
# 下游 group_by / fill / facet 逻辑完全不用改，且：
#   - GO 数据 ONTOLOGY 已存在（BP/CC/MF），本函数不动它 → 三色/三面行为不变；
#   - KEGG/Reactome/Wiki 数据注入 ONTOLOGY = source → 退化为「按来源单色 /
#     单面」，条目与统计口径不变（设计 doc §2）。
# 单值来源时（如只跑 KEGG）fill/facet 退化为单色单面，图仍正确可读。
.tcm_inject_group_key <- function(df) {
  if (!is.data.frame(df)) return(df)
  if (!"ONTOLOGY" %in% names(df)) {
    # 完全无 ONTOLOGY 列（KEGG/Reactome/Wiki 富集结果，或内置示例 kegg.filter）
    if ("source" %in% names(df)) {
      df$ONTOLOGY <- as.character(df$source)
    } else {
      df$ONTOLOGY <- rep("\u6240\u6709\u6765\u6e90", nrow(df))
    }
  } else {
    # ONTOLOGY 列存在但部分行为 NA（GO+KEGG 混合经 bind_rows 合并后，KEGG 行
    # 的 ONTOLOGY 是 NA）。若不清掉，下游 drop_na() 会整批删掉非 GO 行。
    # 用 source 回填 NA；无 source 时填常量，保证 group_by/fill/facet 可用。
    miss <- is.na(df$ONTOLOGY)
    if (any(miss)) {
      if ("source" %in% names(df)) {
        df$ONTOLOGY[miss] <- as.character(df$source[miss])
      } else {
        df$ONTOLOGY[miss] <- "\u6240\u6709\u6765\u6e90"
      }
    }
  }
  df
}
