# ---------------------------------------------------------------------------
# 富集来源建模（2026-10-24，R1）
#
# 背景：在 2026-10-24 之前，"来源"（GO / KEGG / Reactome / WikiPathways）在包层
# **从未被显式建模**。tcm_enrich() 返回 list(go=, reactome=, wikipathways=, kegg=)，
# 每个 enrichResult@result 里**没有 source 列**；source 是 GUI 侧
# flatten_enrich_to_df() 临时 append 的。为让下游代码不用改，
# .tcm_inject_group_key() 把 source 回填进 ONTOLOGY 字段，于是 ONTOLOGY 同时
# 承担「GO 三本体」与「富集来源」两种语义 —— 标题、图例、分面、排序、配色
# 全部错位（boss 2026-10-24 实测：数据是 WikiPathways，标题却写 "GO enrichment"、
# 图例写 "Ontology: wikipathways"）。
#
# 本文件把来源升格为一等公民：
#   source       机器键，恒小写：go / kegg / reactome / wikipathways
#   source_label 显示名：GO / KEGG / Reactome / WikiPathways
#   ontology     BP / CC / MF（**仅 GO 行有值**，其余来源一律 NA）
#
# 三者相互独立，绝不互相回填。
# ---------------------------------------------------------------------------

#' 已知富集来源的机器键（顺序即默认展示顺序）
#' @noRd
.tcm_source_levels <- c("go", "kegg", "reactome", "wikipathways")

#' 来源别名 -> 机器键
#'
#' 为什么要别名表：GUI 侧历史上有过 `reac` / `wp` 之类简写，用户也可能直接
#' 用 "WikiPathways" / "wikipathways" 两种大小写。统一在这里收敛，避免
#' 下游每个函数各写一遍 tolower()。
#' @noRd
.tcm_source_keys <- c(
  "go"           = "go",
  "gos"          = "go",
  "go:bp"        = "go",
  "go:cc"        = "go",
  "go:mf"        = "go",
  "kegg"         = "kegg",
  "kegg pathway" = "kegg",
  "reactome"     = "reactome",
  "reac"         = "reactome",
  "wikipathways" = "wikipathways",
  "wiki"         = "wikipathways",
  "wp"           = "wikipathways"
)

#' 来源机器键 -> 显示名
#' @noRd
.tcm_source_label <- function(x) {
  lv <- c(
    go           = "GO",
    kegg         = "KEGG",
    reactome     = "Reactome",
    wikipathways = "WikiPathways"
  )
  key <- as.character(x)
  out <- unname(lv[key])
  # 未知来源原样保留（不丢信息，也不假装认识）
  out[is.na(out)] <- key[is.na(out)]
  out
}

#' 来源归一化（大小写 / 别名 -> 机器键）
#' @noRd
.tcm_normalize_source <- function(x) {
  key <- tolower(trimws(as.character(x)))
  hit <- unname(.tcm_source_keys[key])
  hit[is.na(hit)] <- key[is.na(hit)]
  hit[!nzchar(hit)] <- NA_character_
  hit
}

#' 从任意 enrichResult / data.frame 取出 result 表
#' @noRd
.tcm_result_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (isS4(x)) {
    r <- tryCatch(x@result, error = function(e) NULL)
    if (is.data.frame(r)) return(r)
    return(NULL)
  }
  if (is.data.frame(x)) return(x)
  NULL
}

#' 把结果表统一成带 source / source_label / ontology 的标准表
#' @noRd
.tcm_finalize_table <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) return(df)

  nm <- names(df)

  # ONTOLOGY -> ontology（保留大小写，因为要判定 BP/CC/MF）
  if ("ONTOLOGY" %in% nm) {
    if (!"ontology" %in% nm) df$ontology <- as.character(df$ONTOLOGY)
    df$ONTOLOGY <- NULL
  } else if (!"ontology" %in% nm) {
    df$ontology <- NA_character_
  } else {
    df$ontology <- as.character(df$ontology)
  }

  # source 归一（缺失则 NA，下方由调用方或标题兜底）
  if (!"source" %in% names(df)) df$source <- NA_character_
  df$source <- .tcm_normalize_source(df$source)
  df$source_label <- .tcm_source_label(df$source)

  # ---- 关键：非 GO 行的 ontology 一律清成 NA ------------------------------
  # 这条是整个修复的核心。历史数据里可能因 .tcm_inject_group_key 把
  # "wikipathways" 塞进了 ONTOLOGY，这里**不报错、直接清掉**，属于自愈。
  nongo <- !is.na(df$source) & df$source != "go"
  df$ontology[nongo] <- NA_character_

  # 非法本体值（不是 BP/CC/MF）同样清掉 —— 宁可无本体，不要错本体
  bad <- !is.na(df$ontology) & !df$ontology %in% c("BP", "CC", "MF")
  df$ontology[bad] <- NA_character_

  rownames(df) <- NULL
  df
}

#' 富集结果标准表
#'
#' 把 \code{\link{tcm_enrich}()} 的返回值（enrichResult 列表）、单个
#' \code{enrichResult}、或已扁平化的 data.frame 统一成一张带来源标识的标准表。
#'
#' 这是「来源」在包层的唯一入口。返回表里 \code{source}、\code{source_label}、
#' \code{ontology} 三列**相互独立**：GO 行有 \code{ontology}（BP/CC/MF），
#' 其他来源的 \code{ontology} 一律为 \code{NA} —— 不再把来源回填进本体字段。
#'
#' @param x \code{tcm_enrich()} 的返回（list）、单个 \code{enrichResult}，
#'   或 data.frame（需含 \code{Description} 等列；\code{source} 列可选）。
#' @return data.frame，至少含：
#'   \describe{
#'     \item{source}{机器键：go / kegg / reactome / wikipathways}
#'     \item{source_label}{显示名：GO / KEGG / Reactome / WikiPathways}
#'     \item{ontology}{BP / CC / MF，仅 GO 行有值}
#'     \item{Description, p.adjust, Count, GeneRatio, BgRatio, ...}{原样保留}
#'   }
#' @examples
#' \dontrun{
#' data(xfbdf, package = "tcmnetpharma")
#' res <- tcm_enrich(unique(xfbdf$target), keytype = "SYMBOL", species = "human")
#' tb <- tcm_enrich_table(res)
#' table(tb$source_label, useNA = "ifany")
#' table(tb$ontology, useNA = "ifany")
#' }
#' @export
tcm_enrich_table <- function(x) {
  if (is.null(x)) stop("`x` is NULL.", call. = FALSE)

  # ---- 单个 enrichResult ---------------------------------------------------
  if (isS4(x)) {
    df <- .tcm_result_df(x)
    if (is.null(df) || !nrow(df))
      stop("`x` is an enrichResult with no rows in @result.", call. = FALSE)
    if (!"source" %in% names(df)) df$source <- NA_character_
    return(.tcm_finalize_table(df))
  }

  # ---- 已扁平化的 data.frame ----------------------------------------------
  if (is.data.frame(x)) return(.tcm_finalize_table(x))

  # ---- tcm_enrich() 返回的 list -------------------------------------------
  if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms) || !length(nms)) nms <- rep(NA_character_, length(x))
    parts <- list()
    for (i in seq_along(x)) {
      df <- .tcm_result_df(x[[i]])
      if (is.null(df) || !nrow(df)) next
      if (!"source" %in% names(df)) df$source <- nms[[i]]
      parts[[length(parts) + 1L]] <- df
    }
    if (!length(parts))
      stop("No non-empty enrichment result found in `x`.", call. = FALSE)

    # 列对齐后再 rbind（不同来源的 enrichResult 列集可能不同：
    # GO 有 ONTOLOGY，KEGG 没有）
    all_cols <- unique(unlist(lapply(parts, names), use.names = FALSE))
    parts <- lapply(parts, function(d) {
      miss <- setdiff(all_cols, names(d))
      for (m in miss) d[[m]] <- NA
      d[, all_cols, drop = FALSE]
    })
    return(.tcm_finalize_table(do.call(rbind, parts)))
  }

  stop("`x` must be a tcm_enrich() result, an enrichResult, or a data.frame.",
       call. = FALSE)
}

#' 从数据派生富集图标题
#'
#' 为什么必须派生：\code{go_barplot()} 曾把 \code{title = "GO enrichment of
#' cluster"} 写成形参默认值，于是任何非 GO 数据（KEGG / Reactome /
#' WikiPathways）都继承了这个错误身份。身份信息不该有字面量默认值。
#'
#' @param df 标准表（含 \code{source_label} / \code{ontology}）
#' @return 单字符串
#' @noRd
.tcm_enrich_title <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) return("Functional enrichment")

  lv <- .tcm_source_levels
  src <- if ("source" %in% names(df)) df$source else rep(NA_character_, nrow(df))
  src <- unique(src[!is.na(src)])
  # 按固定顺序排（GO -> KEGG -> Reactome -> WikiPathways），不随数据顺序漂移
  src <- lv[lv %in% src]

  onto <- if ("ontology" %in% names(df)) df$ontology else rep(NA_character_, nrow(df))
  onto <- unique(onto[!is.na(onto)])
  onto <- c("BP", "CC", "MF")[c("BP", "CC", "MF") %in% onto]

  if (!length(src)) return("Functional enrichment")

  if (length(src) == 1L) {
    s <- src[[1L]]
    if (identical(s, "go")) {
      if (!length(onto)) return("GO enrichment")
      if (length(onto) == 1L) {
        nm <- c(
          BP = "biological process",
          CC = "cellular component",
          MF = "molecular function"
        )
        return(paste0("GO ", unname(nm[onto]), " enrichment"))
      }
      return("GO enrichment (BP / CC / MF)")
    }
    if (identical(s, "kegg")) return("KEGG pathway enrichment")
    if (identical(s, "reactome")) return("Reactome pathway enrichment")
    if (identical(s, "wikipathways")) return("WikiPathways enrichment")
    return(paste0(.tcm_source_label(s), " enrichment"))
  }

  paste0("Functional enrichment (", paste(.tcm_source_label(src), collapse = " \u00b7 "), ")")
}

#' 决定「按什么分组（上色 / 分面）」
#'
#' 规则（对应 boss 2026-10-24 决策：GO 默认按本体分段）：
#'   - 单一来源 + GO + 多个本体 -> 按 ontology（BP/CC/MF）
#'   - 单一来源 + 单本体       -> 按 ontology（退化为单色，图仍正确）
#'   - 多来源                  -> 按 source_label
#'   - 无任何来源信息          -> 全部归为 "\u6240\u6709\u6765\u6e90"
#'
#' @return list(key = 分组向量, name = 图例名, column = 使用的列名)
#' @noRd
.tcm_resolve_group <- function(df) {
  n <- nrow(df)
  src <- if ("source" %in% names(df)) df$source else rep(NA_character_, n)
  src_lab <- if ("source_label" %in% names(df)) df$source_label else rep(NA_character_, n)
  onto <- if ("ontology" %in% names(df)) df$ontology else rep(NA_character_, n)

  n_src <- length(unique(src[!is.na(src)]))
  onto_vals <- unique(onto[!is.na(onto)])

  # 单源 GO（或来源未知但本体齐全）-> 按本体
  use_onto <- length(onto_vals) > 0L && (
    n_src == 0L ||
      (n_src == 1L && all(src[!is.na(src)] == "go"))
  )

  if (use_onto) {
    key <- ifelse(is.na(onto), src_lab, onto)
    return(list(key = key, name = "Ontology", column = "ontology"))
  }

  if (n_src >= 1L) {
    key <- src_lab
    key[is.na(key) | !nzchar(key)] <- "\u6240\u6709\u6765\u6e90"
    return(list(key = key, name = "Source", column = "source_label"))
  }

  list(
    key = rep("\u6240\u6709\u6765\u6e90", n),
    name = "Group",
    column = NA_character_
  )
}
