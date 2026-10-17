#' End-to-end functional enrichment: GO / Reactome / WikiPathways
#'
#' Convenience wrapper that runs over-representation analysis on a gene list and
#' returns \pkg{clusterProfiler} \code{enrichResult} objects ready to be plotted
#' by \code{\link{go_barplot}}, \code{\link{go_dotplot}}, \code{\link{go_cir}},
#' \code{\link{pathway_ccplot}}, \code{\link{pathway_cirplot}}, etc.
#'
#' GO uses \pkg{clusterProfiler} \code{enrichGO}. Reactome and WikiPathways use
#' the offline pathway gene sets shipped with TCMNP
#' (\code{human_reactome}/\code{mu_reactome} and
#' \code{human_wikipathways}/\code{mu_wikipathways}) via \code{enricher}, so they
#' need no network access and are safe for the offline exe distribution.
#'
#' \strong{Licensing note:} KEGG pathway enrichment is intentionally NOT provided.
#' KEGG-derived gene sets (including MSigDB C2 CP:KEGG) carry Kanehisa
#' Laboratories' "all rights reserved" licensing and are not redistributable
#' (see \code{inst/extdata/DATA-README.md}). Reactome (CC BY 4.0) and
#' WikiPathways (CC0 / public domain) fully cover pathway-level enrichment
#' offline and redistributable.
#'
#' @param gene character vector of gene identifiers.
#' @param keytype \code{"SYMBOL"} or \code{"ENTREZID"}. Symbol inputs are mapped
#'   to Entrez via the relevant OrgDb for GO.
#' @param species \code{"human"} or \code{"mouse"}.
#' @param ont character vector; any of \code{"GO"}, \code{"Reactome"},
#'   \code{"WikiPathways"}. Default all three.
#' @param pvalueCutoff numeric cutoff passed to the underlying \code{enrich*} calls.
#' @param qvalueCutoff numeric cutoff passed through.
#' @param pAdjustMethod character passed to \code{pAdjustMethod}.
#' @param minGSSize,maxGSSize gene-set size bounds.
#' @param ... extra arguments forwarded to the underlying \code{enrich*} calls.
#' @return A named list of \code{enrichResult} objects (only non-empty results
#'   are kept; the name is the ontology, e.g. \code{go}, \code{reactome},
#'   \code{wikipathways}). If none are significant, an informative error is raised.
#' @export
#'
#' @examples
#' \dontrun{
#' g <- c("TP53", "BRCA1", "BRCA2", "EGFR", "KRAS", "PTEN", "MYC", "AKT1")
#' res <- tcm_enrich(g, keytype = "SYMBOL", species = "human")
#' go_barplot(res$go)
#' pathway_ccplot(res$reactome)
#' pathway_ccplot(res$wikipathways)
#' }
tcm_enrich <- function(gene, keytype = "SYMBOL", species = "human",
                       ont = c("GO", "Reactome", "WikiPathways"),
                       pvalueCutoff = 0.05, qvalueCutoff = 0.05,
                       pAdjustMethod = "BH",
                       minGSSize = 3, maxGSSize = 500,
                       ...) {
  # ---- dependency guards (clusterProfiler is Imports but guard anyway) ----
  if (!requireNamespace("clusterProfiler", quietly = TRUE))
    stop("tcm_enrich() requires 'clusterProfiler'. Install it with:\n",
         "  BiocManager::install('clusterProfiler')", call. = FALSE)
  if (!requireNamespace("DOSE", quietly = TRUE))
    stop("tcm_enrich() requires 'DOSE'. Install it with:\n",
         "  BiocManager::install('DOSE')", call. = FALSE)

  gene <- unique(na.omit(as.character(gene)))
  if (!length(gene))
    stop("`gene` is empty after removing NA/duplicates.", call. = FALSE)

  species <- match.arg(species, c("human", "mouse"))
  orgdb <- if (identical(species, "human")) "org.Hs.eg.db" else "org.Mm.eg.db"
  if (!requireNamespace(orgdb, quietly = TRUE))
    stop("tcm_enrich() requires '", orgdb, "'. Install it with:\n",
         "  BiocManager::install('", orgdb, "')", call. = FALSE)
  # OrgDb object passed to clusterProfiler (:: reference keeps R CMD check happy
  # and guarantees the OrgDb is loaded at runtime for the offline exe).
  orgdb_obj <- if (identical(species, "human"))
    org.Hs.eg.db::org.Hs.eg.db else org.Mm.eg.db::org.Mm.eg.db

  ont <- match.arg(ont, c("GO", "Reactome", "WikiPathways"), several.ok = TRUE)

  # ---- SYMBOL -> ENTREZID for GO ----
  entrez <- gene
  if (identical(keytype, "SYMBOL")) {
    eg <- clusterProfiler::bitr(gene, fromType = "SYMBOL", toType = "ENTREZID",
                                OrgDb = orgdb_obj)
    entrez <- unique(eg$ENTREZID)
    entrez <- entrez[!is.na(entrez)]
    if (!length(entrez))
      stop("None of the input SYMBOLs could be mapped to Entrez IDs via ",
           orgdb, ".", call. = FALSE)
  }

  out <- list()

  # ---- GO (BP + MF + CC via ont = "ALL") ----
  if ("GO" %in% ont) {
    go <- clusterProfiler::enrichGO(
      gene = entrez, OrgDb = orgdb_obj, keyType = "ENTREZID", ont = "ALL",
      pAdjustMethod = pAdjustMethod,
      pvalueCutoff = pvalueCutoff, qvalueCutoff = qvalueCutoff,
      minGSSize = minGSSize, maxGSSize = maxGSSize, ...)
    if (!is.null(go) && inherits(go, "enrichResult") && nrow(go@result) > 0)
      out$go <- go
  }

  # ---- helper for offline enricher on a bundled gene-set list ----
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

  # symbols for the offline enrichers (gene sets are symbol-based)
  sym <- gene
  if (identical(keytype, "ENTREZID")) {
    eg2 <- clusterProfiler::bitr(gene, fromType = "ENTREZID", toType = "SYMBOL",
                                 OrgDb = orgdb_obj)
    sym <- unique(eg2$SYMBOL); sym <- sym[!is.na(sym)]
  }

  # ---- Reactome (offline gene sets shipped with TCMNP) ----
  if ("Reactome" %in% ont) {
    gs <- if (identical(species, "human")) human_reactome else mu_reactome
    react <- .run_offline(list(genes = gs), sym, "Reactome")
    if (!is.null(react)) out$reactome <- react
  }

  # ---- WikiPathways (offline, CC0 / public domain) ----
  if ("WikiPathways" %in% ont) {
    wp <- if (identical(species, "human")) human_wikipathways else mu_wikipathways
    wpres <- .run_offline(wp, sym, "WikiPathways")
    if (!is.null(wpres)) out$wikipathways <- wpres
  }

  if (!length(out))
    stop("No enrichment terms significant at pvalueCutoff = ", pvalueCutoff,
         ". Try loosening the cutoff or check the input gene list.", call. = FALSE)
  out
}
