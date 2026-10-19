#' Intersect Drug Targets with Disease Targets
#'
#' The canonical network-pharmacology step: take the targets of a herb (or a
#' herb formula) and intersect them with the targets associated with a disease.
#' The resulting target set is what downstream enrichment / PPI / topology
#' analysis should be run on.
#'
#' This is a thin, \strong{cache-free} composition of \code{\link{herb_target}}
#' and \code{\link{disease_target}} — both sides are queried through the same
#' lazy DuckDB layer, and the intersection is taken on \strong{gene SYMBOL},
#' which both sides expose (herb side: \code{target}; disease side:
#' \code{target_symbol}). No extra mapping table is involved.
#'
#' @param herb character vector of herb names (see \code{type}).
#' @param disease character vector of disease names / MeSH / UMLS / SymMap IDs
#'   (see \code{disease_type}).
#' @param type herb identifier type, \code{"Herb_cn_name"} (default) or
#'   \code{"Herb_name_pin_yin"}.
#' @param disease_type disease identifier type, one of \code{"disease_name"},
#'   \code{"mesh_id"}, \code{"umls_id"}, \code{"disease_id"}.
#' @param match disease name matching, \code{"exact"} (default) or
#'   \code{"contains"}.
#' @param OB.value,DL.value ADME filters applied on the herb side, same
#'   semantics as \code{\link{herb_target}}.
#' @param min_score minimum Open Targets association score on the disease side,
#'   same semantics as \code{\link{disease_target}}. Default \code{0}
#'   (no implicit threshold — nothing is silently dropped).
#' @param resolve_herb passed to \code{\link{herb_target}}; expand alternative
#'   spellings of the same herb entity.
#'
#' @return A \code{data.frame} with one row per intersected target, ordered by
#'   \code{disease_score} then by \code{n_molecule} (both descending):
#'   \describe{
#'     \item{target_symbol}{gene symbol shared by both sides (the join key)}
#'     \item{herb}{semicolon-separated herb names that hit this target}
#'     \item{n_molecule}{number of distinct molecules supporting this target}
#'     \item{disease_score}{max Open Targets overall-direct score among the
#'       matched disease–target pairs (0 when the score is missing)}
#'     \item{evidence_count}{max evidence count among matched pairs}
#'     \item{n_ot_disease}{number of Open Targets diseases behind this target}
#'     \item{target_entrez,target_ensembl}{IDs carried over from the disease side
#'       (\code{NA} when the target dictionary has no entry)}
#'   }
#'   Zero matches returns a \strong{0-row data.frame with the same columns}, so
#'   downstream code never has to special-case \code{NULL}.
#'
#' @details
#' \strong{Species}: disease–target associations come from Open Targets, which
#' is human-only. The intersection is therefore on human gene symbols; passing
#' mouse-style symbols will simply yield zero rows.
#'
#' \strong{Why not return the raw union}: the point of this function is the
#' \emph{intersection}. If you want the two sides separately, call
#' \code{\link{herb_target}} and \code{\link{disease_target}} directly.
#'
#' @examples
#' \dontrun{
#' # Classic workflow: herb targets ∩ asthma targets -> enrichment
#' hit <- tcm_intersect_target("麻黄", "Asthma")
#' head(hit)
#'
#' res <- tcm_enrich(hit$target_symbol, keytype = "SYMBOL", species = "human")
#' }
#' @seealso \code{\link{herb_target}}, \code{\link{disease_target}},
#'   \code{\link{tcm_enrich}}
#' @export
tcm_intersect_target <- function(herb, disease,
                                 type = "Herb_cn_name",
                                 disease_type = "disease_name",
                                 match = c("exact", "contains"),
                                 OB.value = 30, DL.value = 0.18,
                                 min_score = 0,
                                 resolve_herb = FALSE) {
  # 类型归一（与 tcm_enrich 同源）：GUI 的 JSON→R 可能把字符串/数组转成 list，
  # 而下游 `type == "Herb_cn_name"` 这类比较对 list 会直接报错（错误信息还
  # 完全看不出是类型问题）。包层宽容，让集成方不必各自记得 unlist。
  chr1 <- function(x, default) {
    y <- as.character(unlist(x, use.names = FALSE))
    y <- y[nzchar(y)]
    if (length(y)) y[[1]] else default
  }
  type <- chr1(type, "Herb_cn_name")
  disease_type <- chr1(disease_type, "disease_name")
  # ⚠️ match.arg 必须显式给 choices：省略时它靠 `substitute(arg)` 去读形参默认值，
  #    传**表达式**（如 match.arg(chr1(x))）会让它拿调用对象当索引，
  #    报 "no such index at level 1"（2026-10-19 实测踩到）。
  match <- match.arg(chr1(match, "exact"), c("exact", "contains"))
  # herb / disease 也归一：GUI 的 JSON 数组 -> list 会让下游
  # `.tcm_distinct_where()` 报 "no such index at level 1"（同样是类型问题，
  # 错误信息却完全看不出根因）。
  herb <- trimws(as.character(unlist(herb, use.names = FALSE)))
  herb <- herb[nzchar(herb)]
  disease <- trimws(as.character(unlist(disease, use.names = FALSE)))
  disease <- disease[nzchar(disease)]
  if (!length(herb))
    stop("`herb` is empty.", call. = FALSE)
  if (!length(disease))
    stop("`disease` is empty.", call. = FALSE)
  num1 <- function(x, default) {
    y <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
    y <- y[!is.na(y)]
    if (length(y)) y[[1]] else default
  }
  min_score <- num1(min_score, 0)
  OB.value <- num1(OB.value, 30)
  DL.value <- num1(DL.value, 0.18)

  empty <- function() {
    data.frame(
      target_symbol = character(0), herb = character(0),
      n_molecule = integer(0), disease_score = numeric(0),
      evidence_count = numeric(0), n_ot_disease = integer(0),
      target_entrez = character(0), target_ensembl = character(0),
      stringsAsFactors = FALSE
    )
  }

  ht <- herb_target(herb, OB.value = OB.value, DL.value = DL.value,
                    type = type, resolve_herb = resolve_herb)
  if (is.null(ht) || nrow(ht) == 0L) {
    warning("No herb targets found for: ", paste(herb, collapse = ", "),
            call. = FALSE)
    return(empty())
  }

  dt <- disease_target(disease, type = disease_type, match = match,
                       min_score = min_score)
  if (is.null(dt) || nrow(dt) == 0L) {
    warning("No disease targets found for: ", paste(disease, collapse = ", "),
            call. = FALSE)
    return(empty())
  }

  # ---- 药侧：按靶点聚合（一味药可能多个分子命中同一靶点）----
  ht$target <- as.character(ht$target)
  ht <- ht[!is.na(ht$target) & nzchar(ht$target), , drop = FALSE]
  if (nrow(ht) == 0L) return(empty())

  herb_side <- do.call(rbind, lapply(split(ht, ht$target), function(g) {
    data.frame(
      target_symbol = g$target[[1]],
      herb = paste(sort(unique(as.character(g$herb))), collapse = ";"),
      n_molecule = length(unique(stats::na.omit(as.character(g$molecule_id)))),
      stringsAsFactors = FALSE
    )
  }))

  # ---- 病侧：按靶点取「最强证据」（同一靶点可能挂在多个 OT 疾病下）----
  # 用 max 而不是 sum：sum 会把「多个疾病各一条弱证据」抬成强证据，
  # 掩盖真实强度；来源数另用 n_ot_disease 如实记录（不静默聚合）。
  dt <- dt[!is.na(dt$target_symbol) & nzchar(dt$target_symbol), , drop = FALSE]
  if (nrow(dt) == 0L) return(empty())

  dis_side <- do.call(rbind, lapply(split(dt, dt$target_symbol), function(g) {
    sc <- suppressWarnings(as.numeric(g$score))
    ev <- suppressWarnings(as.numeric(g$evidence_count))
    nt <- suppressWarnings(as.numeric(g$n_ot_disease))
    pick <- function(v, f) if (all(is.na(v))) NA else f(v, na.rm = TRUE)
    data.frame(
      target_symbol = g$target_symbol[[1]],
      disease_score = if (all(is.na(sc))) 0 else max(sc, na.rm = TRUE),
      evidence_count = pick(ev, max),
      n_ot_disease = pick(nt, max),
      target_entrez = {
        v <- unique(stats::na.omit(as.character(g$target_entrez)))
        if (length(v)) v[[1]] else NA_character_
      },
      target_ensembl = {
        v <- unique(stats::na.omit(as.character(g$target_ensembl)))
        if (length(v)) v[[1]] else NA_character_
      },
      stringsAsFactors = FALSE
    )
  }))

  # ---- 交集（symbol 层面，双方都已是 symbol 口径）----
  out <- merge(herb_side, dis_side, by = "target_symbol")
  if (nrow(out) == 0L) return(empty())

  out <- out[order(-out$disease_score, -out$n_molecule, out$target_symbol), ,
             drop = FALSE]
  rownames(out) <- NULL
  out[, c("target_symbol", "herb", "n_molecule", "disease_score",
          "evidence_count", "n_ot_disease", "target_entrez",
          "target_ensembl")]
}
