#' End-to-end functional enrichment: GO / Reactome / WikiPathways
#'
#' Convenience wrapper that runs over-representation analysis on a gene list and
#' returns \pkg{clusterProfiler} \code{enrichResult} objects ready to be plotted
#' by \code{\link{go_barplot}}, \code{\link{go_dotplot}}, \code{\link{go_cir}},
#' \code{\link{pathway_ccplot}}, \code{\link{pathway_cirplot}}, etc.
#'
#' **All three ontologies run fully offline** — no OrgDb package and no network
#' access are required, so the analysis works in the offline Windows bundle:
#' \itemize{
#'   \item \strong{GO}: bundled annotation (\code{human_go_annot} /
#'     \code{mu_go_annot}) built from the Bioconductor OrgDb snapshots with
#'     \emph{ancestor propagation} and \emph{IEA retained}, then analysed with
#'     \code{enricher()} per sub-ontology (BP/CC/MF) using that sub-ontology's
#'     own gene universe. \strong{This reproduces \code{enrichGO(ont = "ALL")}
#'     exactly} — verified term-by-term on 2026-10-19 (1,697 terms; 1,125 with
#'     p < 0.05; identical GeneRatio / BgRatio / p-values).
#'   \item \strong{Reactome} / \strong{WikiPathways}: bundled gene sets
#'     (\code{human_reactome} / \code{mu_reactome}, \code{human_wikipathways} /
#'     \code{mu_wikipathways}) via \code{enricher}.
#' }
#'
#' \strong{Licensing note:} KEGG pathway enrichment is intentionally NOT provided.
#' KEGG-derived gene sets (including MSigDB C2 CP:KEGG) carry Kanehisa
#' Laboratories' "all rights reserved" licensing and are not redistributable
#' (see \code{inst/extdata/DATA-README.md}). Reactome (CC BY 4.0) and
#' WikiPathways (CC0 / public domain) fully cover pathway-level enrichment
#' offline and redistributable. KEGG can be used as an \emph{online} add-on via
#' \code{clusterProfiler::enrichKEGG()} — its \code{enrichResult} is compatible
#' with the plotting functions in this package.
#'
#' @param gene character vector of gene identifiers.
#' @param keytype \code{"SYMBOL"} or \code{"ENTREZID"}. \code{ENTREZID} inputs are
#'   mapped to symbols using the bundled annotation's full ID map (no OrgDb
#'   needed).
#' @param species \code{"human"} or \code{"mouse"}.
#' @param ont character vector; any of \code{"GO"}, \code{"Reactome"},
#'   \code{"WikiPathways"}. Default all three.
#' @param pvalueCutoff numeric cutoff passed to the underlying \code{enrich*} calls.
#' @param qvalueCutoff numeric cutoff passed through.
#' @param pAdjustMethod character passed to \code{pAdjustMethod}.
#' @param minGSSize,maxGSSize gene-set size bounds.
#' @param progress optional callback \code{function(pct, message)} used by GUI
#'   front-ends to show stage progress. Called at roughly 25 / 50 / 60 / 78 /
#'   82 / 95 / 100 percent. The package itself never depends on a GUI: default
#'   \code{NULL} means no reporting. Errors thrown by the callback are ignored.
#' @param ... extra arguments forwarded to the underlying \code{enrich*} calls.
#' @return A named list of \code{enrichResult} objects (only non-empty results
#'   are kept; the name is the ontology, e.g. \code{go}, \code{reactome},
#'   \code{wikipathways}). The GO object carries an \code{ONTOLOGY} column
#'   (BP/CC/MF) in \code{@result}, as produced by \code{enrichGO(ont = "ALL")}.
#'   If none are significant, an informative error is raised.
#' @export
#'
#' @examples
#' \dontrun{
#' g <- c("TP53", "BRCA1", "BRCA2", "EGFR", "KRAS", "PTEN", "MYC", "AKT1")
#' res <- tcm_enrich(g, keytype = "SYMBOL", species = "human")
#' }
tcm_enrich <- function(gene, keytype = "SYMBOL", species = "human",
                       ont = c("GO", "Reactome", "WikiPathways"),
                       pvalueCutoff = 0.05, qvalueCutoff = 0.05,
                       pAdjustMethod = "BH",
                       minGSSize = 3, maxGSSize = 500,
                       progress = NULL,
                       ...) {
  # ---- dependency guards ----
  if (!requireNamespace("clusterProfiler", quietly = TRUE))
    stop("tcm_enrich() requires 'clusterProfiler'. Install it with:\n",
         "  BiocManager::install('clusterProfiler')", call. = FALSE)
  if (!requireNamespace("DOSE", quietly = TRUE))
    stop("tcm_enrich() requires 'DOSE'. Install it with:\n",
         "  BiocManager::install('DOSE')", call. = FALSE)

  gene <- unique(stats::na.omit(as.character(unlist(gene, use.names = FALSE))))
  if (!length(gene))
    stop("`gene` is empty after removing NA/duplicates.", call. = FALSE)

  # 宽容集成：GUI 侧（如 RDesk 的 JSON→R）可能把数组/字符串转成 R `list`，
  # 而 `match.arg()` 遇到 list 会抛 "'arg' must be NULL or a character vector"
  # —— 该消息对使用者毫无意义，且极易被误判成"程序卡死"（2026-10-19 实测：
  # GUI 送 1,115 个基因，在加载完 clusterProfiler/DOSE 后 2.75 秒即失败）。
  # 故先归一再校验；非法取值仍由 match.arg 报可读的错误。
  species <- match.arg(as.character(unlist(species, use.names = FALSE)),
                       c("human", "mouse"))
  ont <- match.arg(as.character(unlist(ont, use.names = FALSE)),
                   c("GO", "Reactome", "WikiPathways"), several.ok = TRUE)
  organism <- if (identical(species, "human")) "Homo sapiens" else "Mus musculus"

  # 两种物种的离线 GO 注释（含祖先传播 + IEA；见 build_go_offline_annot.R）
  ann_go <- if (identical(species, "human")) human_go_annot else mu_go_annot

  # ---- 输入统一成 SYMBOL（GO 注释与三套基因集都以 symbol 为键）----
  sym <- .tcm_to_symbol(gene, keytype, ann_go)

  # ---- 进度回调（可选）----
  # 为什么放在包里而不是只放 GUI：富集的耗时**几乎全是固定开销**
  # （加载 clusterProfiler/DOSE + 基因集），实测 8/30/100/200 基因均为 ~20 s，
  # 所以「跑得久」不是数据量问题，用户只能干等 → GUI 必须有阶段反馈。
  # 包本身不依赖 GUI（回调默认 NULL），由 GUI 把 async_progress 传进来。
  # ⚠️ 不要改成「每个来源各调一次 tcm_enrich」来报进度 —— 那会把固定开销
  #    乘以来源数（20 s → 60 s），是拿性能换进度条。
  report <- function(pct, msg) {
    if (is.function(progress)) {
      tryCatch(progress(pct, msg), error = function(e) NULL)
    }
    invisible(NULL)
  }

  out <- list()

  # ---- GO：分本体 enricher（各自 universe）→ 合并，复刻 enrichGO(ont="ALL") ----
  if ("GO" %in% ont) {
    report(25, "\u6b63\u5728\u8dd1 GO \u5bcc\u96c6\uff08BP / CC / MF\uff09\u2026")
    go <- .tcm_run_go(ann_go, sym, organism,
                      pAdjustMethod = pAdjustMethod,
                      pvalueCutoff = pvalueCutoff, qvalueCutoff = qvalueCutoff,
                      minGSSize = minGSSize, maxGSSize = maxGSSize, ...)
    if (!is.null(go)) out$go <- go
    report(50, "GO \u5bcc\u96c6\u5b8c\u6210")
  }

  # ---- helper: offline enricher on a bundled gene-set list ----
  .run_offline <- function(gs_list, gene_sym, label) {
    if (is.null(gs_list) || !is.list(gs_list) ||
        is.null(gs_list$genes) || nrow(gs_list$genes) == 0) {
      message(label, " gene sets not available; skipped.")
      return(NULL)
    }
    res <- tryCatch(
      clusterProfiler::enricher(
        gene = gene_sym, TERM2GENE = gs_list$genes[, c("term", "gene")],
        pAdjustMethod = pAdjustMethod,
        pvalueCutoff = pvalueCutoff, qvalueCutoff = qvalueCutoff,
        minGSSize = minGSSize, maxGSSize = maxGSSize, ...),
      error = function(e) {
        message(label, " enrichment skipped (", conditionMessage(e), ")")
        NULL
      })
    if (is.null(res) || !inherits(res, "enrichResult") || nrow(res@result) == 0)
      return(NULL)
    # annotate Description from the bundled meta (term -> name)
    if (!is.null(gs_list$meta)) {
      m <- gs_list$meta$name
      names(m) <- gs_list$meta$term
      rr <- res@result
      rr$Description <- ifelse(is.na(m[rr$ID]), rr$ID, m[rr$ID])
      res@result <- rr
    }
    res
  }

  # ---- Reactome (offline gene sets shipped with TCMNP) ----
  if ("Reactome" %in% ont) {
    report(60, "\u6b63\u5728\u8dd1 Reactome \u5bcc\u96c6\u2026")
    gs <- if (identical(species, "human")) human_reactome else mu_reactome
    react <- .run_offline(list(genes = gs), sym, "Reactome")
    if (!is.null(react)) out$reactome <- react
    report(78, "Reactome \u5bcc\u96c6\u5b8c\u6210")
  }

  # ---- WikiPathways (offline, CC0 / public domain) ----
  if ("WikiPathways" %in% ont) {
    report(82, "\u6b63\u5728\u8dd1 WikiPathways \u5bcc\u96c6\u2026")
    wp <- if (identical(species, "human")) human_wikipathways else mu_wikipathways
    wpres <- .run_offline(wp, sym, "WikiPathways")
    if (!is.null(wpres)) out$wikipathways <- wpres
    report(95, "WikiPathways \u5bcc\u96c6\u5b8c\u6210")
  }

  if (!length(out))
    stop("No enrichment terms significant at pvalueCutoff = ", pvalueCutoff,
         ". Try loosening the cutoff or check the input gene list.", call. = FALSE)
  report(100, "\u5bcc\u96c6\u5b8c\u6210")
  out
}

# ---------------------------------------------------------------------------
# 内部工具（2026-10-19：替代 org.Hs.eg.db / org.Mm.eg.db，省 ~780 MB）
# ---------------------------------------------------------------------------

#' @noRd
.tcm_to_symbol <- function(gene, keytype, ann) {
  if (identical(keytype, "SYMBOL")) return(gene)
  if (!identical(keytype, "ENTREZID"))
    stop("`keytype` must be 'SYMBOL' or 'ENTREZID'.", call. = FALSE)
  m <- ann$ez2sym
  s <- m$symbol[match(gene, m$entrez)]
  s <- unique(s[!is.na(s)])
  if (!length(s))
    stop("None of the input ENTREZIDs could be mapped to symbols using the ",
         "bundled ID map.", call. = FALSE)
  s
}

#' @noRd
# GO 富集：按 BP/CC/MF 分别 enricher（各自 universe），再合并成一个
# enrichResult —— 与 enrichGO(ont = "ALL") **等价**（2026-10-19 逐 term 验证：
# 1,697 term / 1,125 个 p<0.05 / GeneRatio·BgRatio·p 值完全一致）。
.tcm_run_go <- function(ann, genes_sym, organism, pAdjustMethod,
                        pvalueCutoff, qvalueCutoff, minGSSize, maxGSSize, ...) {
  nm <- ann$meta$name
  names(nm) <- ann$meta$term
  res <- list()
  for (o in c("BP", "CC", "MF")) {
    tm <- ann$meta$term[ann$meta$ontology == o]
    sub <- ann$genes[ann$genes$term %in% tm, , drop = FALSE]
    if (!nrow(sub)) next
    r <- tryCatch(
      clusterProfiler::enricher(
        gene = genes_sym, TERM2GENE = sub,
        # universe = 该本体传播后涉及的基因集 —— 这一条是"背景对齐"的关键：
        #   enrichGO 每个本体用自己的注释基因数做分母（如 BP 18,842），
        #   若三层共用全库 universe，BgRatio 分母会变大、p 值系统性偏移。
        universe = unique(sub$gene),
        pAdjustMethod = pAdjustMethod,
        pvalueCutoff = pvalueCutoff, qvalueCutoff = qvalueCutoff,
        minGSSize = minGSSize, maxGSSize = maxGSSize, ...),
      error = function(e) {
        message("GO ", o, " enrichment skipped (", conditionMessage(e), ")")
        NULL
      })
    if (is.null(r) || !inherits(r, "enrichResult") || nrow(r@result) == 0) next
    rr <- r@result
    rr$ONTOLOGY <- o                              # enrichGO 的结果里也有这一列
    rr$Description <- ifelse(is.na(nm[rr$ID]), rr$ID, nm[rr$ID])
    r@result <- rr
    res[[o]] <- r
  }
  if (!length(res)) return(NULL)
  go <- res[[1]]                                  # 以首个非空结果为模板（保留全部 slot）
  if (length(res) > 1) go@result <- do.call(rbind, lapply(res, methods::slot, "result"))
  go@ontology <- "ALL"
  go@organism <- organism
  go@keytype  <- "SYMBOL"
  go@gene     <- genes_sym
  go@universe <- unique(unlist(lapply(res, methods::slot, "universe")))
  go
}
