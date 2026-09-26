# Internal utils: lazy DuckDB queries on tcm_network_compat.parquet (P3.2 2026-09-02)
# Design: functions never load the 16M-row compat table into R;
# always push WHERE filters into DuckDB and materialize small results.

# --- Case-folding column map (2026-09-22; data fixed 2026-09-26) -----------
# DuckDB matches column names case-INSENSITIVELY. When a parquet file carries
# two columns whose names differ only in case, DuckDB keeps the first and
# renames the later one to <name>_<N>. Crucially, referring to the *un-suffixed*
# name then resolves SILENTLY to the OTHER column -- so a missing mapping yields
# wrong results (typically 0 rows) instead of an error.
#
# 2026-09-26: the data layer was fixed (HERB_NAME_PIN_YIN -> herb_pinyin_raw),
# so the shipped parquet now has ZERO case-collisions and this map is normally
# the identity. The probe is kept as a permanent GUARD: it costs one schema
# read, and it re-arms automatically if a future dataset reintroduces a
# collision (the R side then keeps working while the data issue is caught by
# the pipeline gate, common.assert_no_col_case_collision).
#
# The map is PROBED from the file itself (never hard-coded), so any future
# collision is handled automatically:
#   parquet_schema()       -> real (unfolded) column names, in file order
#   DESCRIBE read_parquet  -> DuckDB's physical names, in the same order
# Position-wise pairs whose name changed are exactly the colliding columns.
# See docs/data-management-spec-2026-08-24.md (3. naming rules) and
# logs/errors-herb-target-pinyin-2026-09-22.md.
.tcm_col_probe <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  esc <- gsub("'", "''", path)
  real <- DBI::dbGetQuery(con, sprintf(
    "SELECT name FROM parquet_schema('%s') WHERE num_children IS NULL", esc))$name
  phys <- DBI::dbGetQuery(con, sprintf(
    "DESCRIBE SELECT * FROM read_parquet('%s')", esc))$column_name
  if (length(real) != length(phys)) {
    stop(sprintf("column map probe failed: parquet_schema=%d vs DESCRIBE=%d names",
                 length(real), length(phys)), call. = FALSE)
  }
  changed <- !is.na(real) & !is.na(phys) & real != phys
  stats::setNames(phys[changed], real[changed])
}

# Cached map (closure-scoped): logical column name -> DuckDB physical name.
# A missing data file yields an identity map (pure SQL builders stay usable and
# testable offline); a *probe failure on an existing file* is a hard error --
# silently falling back would reintroduce the wrong-column bug.
.tcm_col_map <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      p <- tryCatch(.tcm_parquet_path(), error = function(e) "")
      cache <<- if (!nzchar(p)) {
        stats::setNames(character(0), character(0))
      } else {
        .tcm_col_probe(p)
      }
    }
    cache
  }
})

# Logical column name -> DuckDB physical name (identity when no collision).
.tcm_col_db <- function(nm) {
  m <- .tcm_col_map()
  i <- match(nm, names(m))
  if (is.na(i)) nm else unname(m[[i]])
}

# DuckDB physical name -> logical name (strip the collision suffix).
.tcm_col_clean <- function(nm) sub("_[0-9]+$", "", nm)

.tcm_parquet_path <- function() {
  p <- system.file("extdata", "tcm_network_compat.parquet", package = "TCMNP")
  if (!nzchar(p)) {  # development mode (not installed): fall back to repo path
    p <- file.path("inst", "extdata", "tcm_network_compat.parquet")
    if (!file.exists(p)) {
      stop("tcm_network_compat.parquet not found (run devtools::install() or check inst/extdata)")
    }
  }
  p
}

.sql_in <- function(x) {
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (!length(x)) return("NULL")
  paste0("'", gsub("'", "''", as.character(x)), "'", collapse = ", ")
}

.tcm_query <- function(sql) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbGetQuery(con, sql)
}

# Filter compat table on a column and return the requested columns.
# col_aliases: named vector c(output_name = "table_column"); extra_where: SQL fragment
.tcm_filter <- function(col, vals, col_aliases, extra_where = NULL) {
  p <- .tcm_parquet_path()
  sel_parts <- vapply(names(col_aliases), function(nm) {
    sprintf("%s AS \"%s\"", .tcm_col_db(col_aliases[[nm]]), nm)
  }, character(1))
  sel <- paste(sel_parts, collapse = ", ")
  q <- sprintf("SELECT %s FROM read_parquet('%s') WHERE \"%s\" IN (%s)",
               sel, p, .tcm_col_db(col), .sql_in(vals))
  if (!is.null(extra_where)) q <- paste0(q, " AND ", extra_where)
  .tcm_query(q)
}

# DISTINCT values of a column (non-empty). `vals` optionally restricts the
# result to a value set. The WHERE clause is built HERE from `col`/`vals` so
# callers CANNOT forget the column-name mapping -- the old signature took a
# pre-built SQL fragment, which silently produced 0 rows when a caller used an
# unmapped name (see logs/errors-herb-target-pinyin-2026-09-22.md).
.tcm_distinct_where <- function(col, vals = NULL) {
  p <- .tcm_parquet_path()
  cdb <- .tcm_col_db(col)
  where <- sprintf("\"%s\" IS NOT NULL AND \"%s\" <> ''", cdb, cdb)
  if (!is.null(vals)) {
    where <- sprintf("%s AND \"%s\" IN (%s)", where, cdb, .sql_in(vals))
  }
  q <- sprintf("SELECT DISTINCT \"%s\" AS v FROM read_parquet('%s') WHERE %s",
               cdb, p, where)
  .tcm_query(q)$v
}

# DISTINCT values of a column (non-empty)
.tcm_distinct <- function(col) .tcm_distinct_where(col)

# Basic table info (tests/audit; column names cleaned back to logical names)
.tcm_meta <- function() {
  p <- .tcm_parquet_path()
  q <- sprintf("SELECT COUNT(*) AS n FROM read_parquet('%s')", p)
  n <- .tcm_query(q)$n
  q2 <- sprintf(
    "SELECT column_name, column_type FROM (DESCRIBE SELECT * FROM read_parquet('%s'))", p)
  cols <- .tcm_query(q2)
  list(rows = n,
       columns = stats::setNames(cols$column_type,
                                 vapply(cols$column_name, .tcm_col_clean, character(1))))
}

# OB/DL are VARCHAR (Python-side source trait, contains 'NA' placeholders):
# filter with an explicit CAST. Column names go through .tcm_col_db() so this
# stays collision-safe (see the map notes at the top of this file).
.tcm_adme_where <- function(OB.value, DL.value) {
  sprintf("CAST(\"%s\" AS DOUBLE) >= %s AND CAST(\"%s\" AS DOUBLE) >= %s",
          .tcm_col_db("OB"), format(OB.value, scientific = FALSE),
          .tcm_col_db("DL"), format(DL.value, scientific = FALSE))
}
