#' Normalize a TERM2NAME payload to the 2-column data.frame `enricher()` needs
#'
#' @description
#' \strong{Regression fix 2026-10-27.} \code{clusterProfiler 4.20.0 / DOSE 4.5.0}
#' 的 \code{enricher()} 在 \code{TERM2NAME} 收到**命名向量**时会在内部按二维对象
#' 取列，报 \code{attempt to set 'colnames' on an object with less than two
#' dimensions}。传两列 \code{data.frame} 则正常。
#'
#' 该缺陷此前一直没被发现：\code{tcm_enrich(ont = "KEGG")} 从未被真正跑通过
#' （缓存对象、快照构建都正确，崩在最后一步的入参形态）。实测对照见
#' \code{docs/RELEASE-AUDIT-2026-10-27.md} 与提交说明。
#'
#' 同时兼容两种缓存形态：旧缓存里 \code{t2n} 是命名向量，新缓存是两列 df。
#' @noRd
.tcm_kegg_t2n_df <- function(t2n) {
  if (is.null(t2n)) return(NULL)
  if (is.data.frame(t2n)) {
    if (ncol(t2n) < 2L) stop("KEGG TERM2NAME data.frame must have 2 columns.", call. = FALSE)
    return(as.data.frame(t2n[, 1:2, drop = FALSE], stringsAsFactors = FALSE))
  }
  data.frame(term = as.character(names(t2n)),
             name = unname(as.character(t2n)),
             stringsAsFactors = FALSE)
}

#' KEGG pathway enrichment via the KEGG REST API (online, cached)
#'
#' @description
#' \strong{Internal helper for \code{\link{tcm_enrich}(ont = "KEGG")}.}
#' Runs KEGG pathway over-representation analysis without bundling KEGG data
#' (which cannot be redistributed — see \code{inst/extdata/DATA-README.md}).
#'
#' Gene-to-pathway memberships are fetched from \code{https://rest.kegg.jp} and
#' cached under \code{tools::R_user_dir("tcmnetpharma", "cache")/kegg/} so that
#' subsequent runs work offline and reproduce the same result within the cache
#' window.
#'
#' @param genes_sym character vector of HGNC symbols.
#' @param species \code{"human"} or \code{"mouse"}.
#' @param ann the bundled annotation list (needs \code{ann$ez2sym} with columns
#'   \code{symbol} / \code{entrez}) — avoids pulling \pkg{org.Hs.eg.db}.
#' @param cache_days integer; reuse a cached KEGG snapshot younger than this.
#' @param timeout_s numeric; per-request timeout in seconds (default 180).
#' @param ... passed to \code{clusterProfiler::enricher()}.
#'
#' @return an \code{enrichResult} with an extra attribute
#'   \code{kegg_snapshot} (the date the cached KEGG snapshot was taken), or
#'   \code{NULL} when no term is significant.
#' @noRd
.tcm_kegg_enrich <- function(genes_sym, species = c("human", "mouse"), ann,
                             cache_days = 30L, timeout_s = 180, ...) {
  species <- match.arg(species)
  code <- if (identical(species, "human")) "hsa" else "mmu"

  # ---- symbol -> entrez（用包内映射，不依赖 org.*.eg.db）----
  m <- ann$ez2sym
  if (is.null(m) || !all(c("symbol", "entrez") %in% names(m))) {
    stop("Bundled ID map is missing symbol/entrez columns.", call. = FALSE)
  }
  idx <- match(genes_sym, m$symbol)
  entrez <- unique(m$entrez[idx][!is.na(idx)])
  entrez <- unique(as.character(entrez[nzchar(as.character(entrez))]))
  if (!length(entrez)) {
    stop("None of the input symbols could be mapped to Entrez IDs, ",
         "so KEGG enrichment cannot run.", call. = FALSE)
  }

  snap <- .tcm_kegg_snapshot(code, cache_days = cache_days, timeout_s = timeout_s)

  er <- clusterProfiler::enricher(
    gene = entrez,
    TERM2GENE = snap$t2g,
    # ⚠️ 必须是两列 data.frame，**不能**直接传命名向量 —— 见 .tcm_kegg_t2n_df 注释
    TERM2NAME = .tcm_kegg_t2n_df(snap$t2n),
    ...
  )
  if (is.null(er) || !nrow(as.data.frame(er))) return(NULL)
  attr(er, "kegg_snapshot") <- snap$taken
  attr(er, "kegg_cached") <- snap$cached
  er
}

#' Fetch (or reuse) a KEGG gene-to-pathway snapshot
#'
#' @return list(t2g = data.frame(term, gene), t2n = named character,
#'   taken = "YYYY-MM-DD", cached = TRUE/FALSE)
#' @noRd
.tcm_kegg_snapshot <- function(code = c("hsa", "mmu"), cache_days = 30L,
                               timeout_s = 180) {
  code <- match.arg(code)
  dir <- file.path(tools::R_user_dir("tcmnetpharma", "cache"), "kegg")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, sprintf("%s_pathway.rds", code))

  if (file.exists(f)) {
    age <- as.numeric(difftime(Sys.time(), file.info(f)$mtime, units = "days"))
    if (is.finite(age) && age < cache_days) {
      snap <- readRDS(f)
      snap$cached <- TRUE
      return(snap)
    }
  }

  # ---- 联网拉取（KEGG REST）----
  # 用 utils::download.file(method = "libcurl")：这是几百 KB 的小请求，
  # 不适用「>1GB 用 httpx」那条规则；关键是把 timeout 放大到 180 s。
  old <- options(timeout = max(60, as.numeric(timeout_s)))
  on.exit(options(old), add = TRUE)

  base <- "https://rest.kegg.jp"
  tmp_link <- tempfile(fileext = ".tsv")
  tmp_list <- tempfile(fileext = ".tsv")
  ok <- tryCatch({
    utils::download.file(paste0(base, "/link/pathway/", code), tmp_link,
                         method = "libcurl", quiet = TRUE)
    utils::download.file(paste0(base, "/list/pathway/", code), tmp_list,
                         method = "libcurl", quiet = TRUE)
    TRUE
  }, error = function(e) {
    stop("KEGG REST request failed: ", conditionMessage(e),
         "\n  - KEGG requires network access (its data cannot be bundled).",
         "\n  - Check your connection, or run the offline sources ",
         "(GO / Reactome / WikiPathways) instead.",
         "\n  - Timeout is set to ", timeout_s, " s.", call. = FALSE)
  })
  if (!isTRUE(ok)) stop("KEGG REST request failed.", call. = FALSE)

  link <- utils::read.delim(tmp_link, header = FALSE, stringsAsFactors = FALSE)
  nmap <- utils::read.delim(tmp_list, header = FALSE, stringsAsFactors = FALSE)
  unlink(c(tmp_link, tmp_list))

  if (!nrow(link) || !nrow(nmap))
    stop("KEGG returned an empty response; try again later.", call. = FALSE)

  # 返回格式（实测）：
  #   link/pathway : "hsa:10327 \t path:hsa00010"
  #   list/pathway : "path:hsa00010 \t Glycolysis / Gluconeogenesis - Homo sapiens (human)"
  t2g <- data.frame(
    term = sub("^path:", "", link[[2]]),
    gene = sub("^[a-zA-Z]+:", "", link[[1]]),
    stringsAsFactors = FALSE
  )
  t2g <- t2g[nzchar(t2g$term) & nzchar(t2g$gene), , drop = FALSE]
  t2g <- t2g[!duplicated(t2g), , drop = FALSE]

  t2n <- stats::setNames(
    sub("\\s+-\\s+.*$", "", nmap[[2]]),      # 去掉 "- Homo sapiens (human)" 后缀
    sub("^path:", "", nmap[[1]])
  )

  snap <- list(t2g = t2g, t2n = t2n,
               taken = format(Sys.Date(), "%Y-%m-%d"),
               cached = FALSE, code = code,
               n_term = length(unique(t2g$term)), n_gene = length(unique(t2g$gene)))
  saveRDS(snap, f)
  snap$cached <- FALSE
  snap
}

#' Where is the KEGG snapshot cached (for UI / debugging)
#' @noRd
.tcm_kegg_cache_path <- function() {
  file.path(tools::R_user_dir("tcmnetpharma", "cache"), "kegg")
}
