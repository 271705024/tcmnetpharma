#' Bar graphs showed the results of enrichment analysis
#'
#' @description
#' \strong{兼容垫片}：本函数现在转发到 \code{\link{enrich_barplot}()}。
#'
#' 2026-10-24 之前的实现有三个结构性问题（boss 实测反馈）：
#' \enumerate{
#'   \item \code{style = "classic"} / \code{"significance"} 两个分支**全局按数值
#'         排序**（\code{order(Count, ONTOLOGY)} 里 ONTOLOGY 只是第二排序键），
#'         于是 BP / CC / MF 按 Count 大小交错出现，无法按本体阅读；
#'   \item \code{title} 的默认值是字面量 \code{"GO enrichment of cluster"}，
#'         任何非 GO 数据（KEGG / Reactome / WikiPathways）都继承了这个错误身份；
#'   \item 分组靠 \code{.tcm_inject_group_key()} 把 \code{source} 回填进
#'         \code{ONTOLOGY}，导致图例出现 \code{"Ontology: wikipathways"}。
#' }
#' 新版把「形制 / 条目数 / 外观」拆成正交参数，标题从数据派生，来源与本体
#' 分列存储。本函数的旧参数仍全部接受，\code{style} 按下表映射到新形制：
#'
#' \tabular{lll}{
#'   \strong{style} \tab \strong{新 layout} \tab \strong{新 measure} \cr
#'   \code{"default"} \tab 由 \code{facet} 决定 \tab \code{neglogp} \cr
#'   \code{"classic"} \tab \code{segmented} \tab \code{count} \cr
#'   \code{"grouped"} \tab \code{facet} \tab \code{count} \cr
#'   \code{"significance"} \tab \code{segmented} \tab \code{neglogp} \cr
#'   \code{"journal"} \tab 由 \code{facet} 决定 \tab \code{neglogp} \cr
#'   \code{"presentation"} \tab 由 \code{facet} 决定 \tab \code{neglogp} \cr
#' }
#'
#' @param go.diff clusterProfiler 的 enrichGO 结果（S4）、\code{tcm_enrich()}
#'   的返回，或 data.frame。
#' @param top **每组**保留的条目数（不再全局截断）。
#' @param plot.set 仅接受 \code{"vertical"}。传 \code{"horizontal"} 会给出
#'   警告并仍按水平条绘制（新实现只有一种方向）。
#' @param facet 是否分面。仅当 \code{style} 为 \code{"default"} /
#'   \code{"journal"} / \code{"presentation"} 时生效：\code{TRUE} -> 三栏分面，
#'   \code{FALSE} -> 单轴分段。
#' @param color 分组底色，默认三色对应 BP/CC/MF。
#' @param bar.width 条宽。
#' @param text.size 字号。
#' @param text.width 条目名换行宽度。
#' @param title 图标题。\code{NULL}（默认）时从数据派生。
#' @param legacy_title 设为 \code{TRUE} 时标题强制为旧版字面量
#'   \code{"GO enrichment of cluster"}。默认 \code{FALSE}；仅用于兼容已存档脚本。
#' @param style 视觉预设，见上表。
#' @param palette 配色方案键，见 \code{\link{tcm_palette_names}()}。
#' @param ... 未识别的参数会触发警告（不静默忽略）。
#'
#' @return ggplot 对象
#' @export
#'
#' @examples
#' \dontrun{
#' data(xfbdf, package = "tcmnetpharma")
#' res <- tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human")
#' go_barplot(res$go)
#' go_barplot(res$go, top = 10, style = "classic")   # 现在按本体分段，不混排
#' go_barplot(res$go, top = 8, style = "grouped")    # 三栏分面
#' go_barplot(res$kegg, style = "classic")           # 标题自动写 "KEGG pathway enrichment"
#' }
go_barplot <- function(go.diff,
                       top = 5,
                       plot.set = "vertical",
                       facet = TRUE,
                       color = c("#A40545", "#7FCBA4", "#4B65AF"),
                       bar.width = 0.8,
                       text.size = 12,
                       text.width = 30,
                       title = NULL,
                       legacy_title = FALSE,
                       style = c("default", "classic", "grouped", "significance",
                                 "journal", "presentation"),
                       palette = NULL,
                       ...) {
  style <- match.arg(style)
  palette <- .tcm_palette_check(palette)

  # 未识别参数不静默吞掉（铁律 28：`...` 吞参数会造成"假通过"）
  dots <- list(...)
  if (length(dots)) {
    warning(
      "go_barplot(): ignoring unrecognised argument(s): ",
      paste(names(dots), collapse = ", "),
      call. = FALSE
    )
  }

  if (!identical(plot.set, "vertical")) {
    warning(
      "go_barplot(): `plot.set = \"", plot.set,
      "\"` is no longer supported; drawing horizontal bars instead.",
      call. = FALSE
    )
  }

  # style -> (layout, measure, visual style)
  map <- switch(style,
    default      = list(layout = NA_character_, measure = "neglogp", vs = "default"),
    classic      = list(layout = "segmented",    measure = "count",   vs = "default"),
    grouped      = list(layout = "facet",        measure = "count",   vs = "default"),
    significance = list(layout = "segmented",    measure = "neglogp", vs = "default"),
    journal      = list(layout = NA_character_,  measure = "neglogp", vs = "journal"),
    presentation = list(layout = NA_character_,  measure = "neglogp", vs = "presentation")
  )
  layout <- map$layout
  if (is.na(layout)) layout <- if (isTRUE(facet)) "facet" else "segmented"

  enrich_barplot(
    data         = go.diff,
    measure      = map$measure,
    layout       = layout,
    top          = top,
    sort_by      = "p.adjust",
    color        = color,
    bar.width    = bar.width,
    text.size    = text.size,
    text.width   = text.width,
    show_value   = TRUE,
    title        = title,
    legacy_title = legacy_title,
    style        = map$vs,
    palette      = palette
  )
}
