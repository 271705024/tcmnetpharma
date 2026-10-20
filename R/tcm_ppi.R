#' Build a Protein–Protein Interaction Sub-network from a Gene List
#'
#' Given a list of gene symbols (e.g. the output of
#' \code{\link{tcm_intersect_target}}), return the edges among them from the
#' bundled \strong{STRING v12.0} human interaction network. The result is an
#' edge table (\code{node1} / \code{node2} / \code{combined_score}) that can be
#' fed straight into \code{\link{ppi_plot}}.
#'
#' @param targets character vector of gene symbols (HGNC). Duplicates and empty
#'   strings are dropped.
#' @param min_score numeric, minimum STRING \code{combined_score}
#'   (0–1000). Default \code{700} = STRING "high confidence". Use \code{400}
#'   for "medium", \code{150} for "low" — but note that low-confidence edges
#'   are mostly noise in a network-pharmacology figure.
#'
#' @return A \code{data.frame} with columns
#'   \describe{
#'     \item{node1,node2}{gene symbols; the edge is undirected and always
#'       normalized so that \code{node1 < node2} lexicographically}
#'     \item{combined_score}{STRING combined score (0–1000)}
#'   }
#'   ordered by \code{combined_score} descending. Zero edges returns a
#'   \strong{0-row data.frame with the same columns}, so \code{ppi_plot} and
#'   downstream code never have to special-case \code{NULL}.
#'
#' @details
#' \strong{Why a separate table}: PPI needs \emph{pairs} of genes, while a
#' target list (from \code{herb_target} / \code{disease_target} /
#' \code{tcm_intersect_target}) only says \emph{which} genes are involved.
#' This function bridges the two.
#'
#' \strong{What is bundled}: only edges with \code{combined_score >= 700}
#' (STRING "high confidence"), which keeps the package distributable
#' (~40 MB) while removing the low-confidence hairball. The full STRING
#' network has ~11.7 M edges; the bundled high-confidence subset has
#' substantially fewer — if you need the full network, download
#' \code{9606.protein.links.v12.0.txt.gz} from STRING and use it directly.
#'
#' \strong{Coverage}: only human genes present in STRING are covered. Genes
#' absent from STRING simply produce no edges (this is not an error).
#'
#' @section Data source and license:
#' STRING v12.0, \url{https://string-db.org}. The STRING data are licensed
#' under \strong{CC BY 4.0}, which permits redistribution with attribution.
#' Please cite:
#' \itemize{
#'   \item Szklarczyk D, et al. (2023) The STRING database in 2023: protein–protein
#'     association networks and functional enrichment analyses for any sequenced
#'     genome of interest. \emph{Nucleic Acids Research} 51(D1):D638–D645.
#'   \item von Mering C, et al. (2003) STRING: a database of predicted functional
#'     associations between proteins. \emph{Nucleic Acids Research} 31(1):258–261.
#' }
#' The bundled subset is derived (filtered by score and mapped to gene symbols);
#' it is not a verbatim copy of the STRING distribution.
#'
#' @examples
#' \dontrun{
#' hit <- tcm_intersect_target("\u9ebb\u9ec4", "Asthma")   # drug targets ∩ disease targets
#' edges <- tcm_ppi(hit$target_symbol)                     # PPI sub-network
#' nrow(edges)
#'
#' ppi_plot(edges)                                         # plot the network
#' }
#' @seealso \code{\link{ppi_plot}}, \code{\link{tcm_intersect_target}},
#'   \code{\link{herb_target}}
#' @export
tcm_ppi <- function(targets, min_score = 700) {
  # 类型归一（与 tcm_enrich 同源：GUI 的 JSON→R 可能把数组转成 list）
  targets <- as.character(unlist(targets, use.names = FALSE))
  targets <- trimws(targets)
  targets <- unique(targets[nzchar(targets) & !is.na(targets)])

  empty <- function() {
    data.frame(node1 = character(0), node2 = character(0),
               combined_score = numeric(0), stringsAsFactors = FALSE)
  }
  if (!length(targets)) return(empty())

  ms <- suppressWarnings(as.numeric(unlist(min_score, use.names = FALSE)))
  ms <- ms[!is.na(ms)]
  ms <- if (length(ms)) ms[[1]] else 700
  if (ms < 0 || ms > 1000)
    stop("`min_score` must be within 0-1000 (STRING combined_score scale).",
         call. = FALSE)

  # 只取两端都在给定基因集内的边 —— 子网而非邻居扩展。
  # 用 IN 列表而不是 join：1115 个基因时 DuckDB 走 hash 更快，且 SQL 可读。
  res <- .tcm_query(sprintf(
    "SELECT node1, node2, combined_score
       FROM read_parquet('%s')
      WHERE combined_score >= %s
        AND node1 IN (%s) AND node2 IN (%s)
      ORDER BY combined_score DESC, node1, node2",
    .tcm_parquet_path("string_ppi"),
    sprintf("%.0f", ms), .sql_in(targets), .sql_in(targets)))

  if (is.null(res) || !nrow(res)) {
    n_hit <- .tcm_query(sprintf(
      "SELECT COUNT(*) AS n FROM read_parquet('%s') WHERE combined_score >= %s",
      .tcm_parquet_path("string_ppi"), sprintf("%.0f", ms)))$n[[1]]
    message("No PPI edges among the ", length(targets),
            " input genes at combined_score >= ", ms,
            " (bundled high-confidence edges: ", format(n_hit, big.mark = ","),
            "). Try lowering `min_score`, or check that the input are HGNC symbols.")
    return(empty())
  }

  res$combined_score <- as.numeric(res$combined_score)
  rownames(res) <- NULL
  res
}
