#!/usr/bin/env Rscript
# Build bundled WikiPathways gene sets (CC0) for TCMNP offline enrichment.
#
# Source: https://data.wikipathways.org/{date}/gmt/wikipathways-{date}-gmt-{Species}.gmt
# License: WikiPathways content is CC0 (public domain) since 2018 -> redistributable.
# Replaces KEGG pathway enrichment (KEGG-derived data are NOT redistributable).
#
# Run from anywhere:
#   Rscript data-raw/build_wikipathways.R
# Requires: org.Hs.eg.db and org.Mm.eg.db (build-time only; not a runtime dep).
suppressMessages(library(methods))

# resolve package root (this script lives in data-raw/)
script_dir <- dirname(sub("--file=", "", grep("^--file=", commandArgs(FALSE),
                                              value = TRUE)[1], fixed = TRUE))
if (script_dir == "") script_dir <- "."
pkgroot <- normalizePath(file.path(script_dir, ".."))
message("[pkgroot] ", pkgroot)

DATE  <- "20261010"
BASE  <- sprintf("https://data.wikipathways.org/%s/gmt/wikipathways-%s-gmt-", DATE, DATE)

.parse_gmt <- function(url, orgdb) {
  message("[download] ", url)
  tmp <- tempfile(fileext = ".gmt")
  dl <- tryCatch(download.file(url, tmp, mode = "wb", quiet = TRUE),
                 error = function(e) -1)
  if (!is.null(attr(dl, "status")) || dl != 0)
    stop("download failed: ", url)
  lines <- readLines(tmp, warn = FALSE)
  # WikiPathways GMT layout:
  #   f[1] = "PathwayName%WikiPathways_YYYYMMDD%WPxxxx%Species"
  #   f[2] = url
  #   f[3..] = Entrez gene ids
  org_obj <- get(orgdb, envir = asNamespace(orgdb))
  term <- character(0); gsym <- character(0)
  meta_term <- character(0); meta_name <- character(0)
  for (ln in lines) {
    f <- strsplit(ln, "\t")[[1]]
    if (length(f) < 3) next
    header <- f[1]
    pname <- sub("%.*", "", header)
    wpid  <- regmatches(header, regexpr("WP\\d+", header))
    if (length(wpid) == 0) next
    genes <- f[3:length(f)]
    genes <- genes[nzchar(genes)]
    if (!length(genes)) next
    syms <- tryCatch(
      AnnotationDbi::mapIds(org_obj, keys = genes,
                            column = "SYMBOL", keytype = "ENTREZID"),
      error = function(e) rep(NA_character_, length(genes)))
    syms <- unname(syms)
    syms <- syms[!is.na(syms) & nzchar(syms)]
    if (!length(syms)) next
    term <- c(term, rep(wpid, length(syms)))
    gsym <- c(gsym, syms)
    meta_term <- c(meta_term, wpid)
    meta_name <- c(meta_name, pname)
  }
  list(genes = data.frame(term = term, gene = gsym, stringsAsFactors = FALSE),
       meta  = unique(data.frame(term = meta_term, name = meta_name,
                                 stringsAsFactors = FALSE)))
}

build_one <- function(species, orgdb, url) {
  if (!requireNamespace(orgdb, quietly = TRUE)) {
    message("[skip] ", orgdb, " not installed; skipping ", species)
    return(invisible(NULL))
  }
  x <- .parse_gmt(url, orgdb)
  message(sprintf("  %s: %d term-gene rows, %d pathways",
                  species, nrow(x$genes), nrow(x$meta)))
  list(genes = x$genes, meta = x$meta)
}

# ---- human ----
h <- build_one("human", "org.Hs.eg.db", paste0(BASE, "Homo_sapiens.gmt"))
if (!is.null(h)) {
  human_wikipathways <- h
  save(human_wikipathways, file = file.path(pkgroot, "data", "human_wikipathways.rda"),
       compress = "xz")
  message("[saved] data/human_wikipathways.rda")
}

# ---- mouse ----
m <- build_one("mouse", "org.Mm.eg.db", paste0(BASE, "Mus_musculus.gmt"))
if (!is.null(m)) {
  mu_wikipathways <- m
  save(mu_wikipathways, file = file.path(pkgroot, "data", "mu_wikipathways.rda"),
       compress = "xz")
  message("[saved] data/mu_wikipathways.rda")
}
message("DONE")
