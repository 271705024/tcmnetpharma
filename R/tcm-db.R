# Internal utils: lazy DuckDB queries on tcm_network_compat.parquet (P3.2 2026-09-02)
# Design: functions never load the 16M-row compat table into R;
# always push WHERE filters into DuckDB and materialize small results.

# duckdb folds case-insensitive column names when reading parquet (duckdb 1.5.x):
# HERB_NAME_PIN_YIN and Herb_name_pin_yin collide -> the latter becomes
# Herb_name_pin_yin_1 automatically. Public-facing code always uses the
# logical column name (Herb_name_pin_yin); SQL generation maps to the db name.
.tcm_col_db <- function(nm) {
  if (nm == "Herb_name_pin_yin") "Herb_name_pin_yin_1" else nm
}
.tcm_col_clean <- function(nm) sub("_1$", "", nm)

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

# DISTINCT values of a column, restricted by a WHERE fragment
.tcm_distinct_where <- function(col, where) {
  p <- .tcm_parquet_path()
  q <- sprintf("SELECT DISTINCT \"%s\" AS v FROM read_parquet('%s') WHERE %s",
               .tcm_col_db(col), p, where)
  .tcm_query(q)$v
}

# DISTINCT values of a column (non-empty)
.tcm_distinct <- function(col) {
  cdb <- .tcm_col_db(col)
  .tcm_distinct_where(col, sprintf("\"%s\" IS NOT NULL AND \"%s\" <> ''", cdb, cdb))
}

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
# filter with an explicit CAST
.tcm_adme_where <- function(OB.value, DL.value) {
  sprintf("CAST(\"OB\" AS DOUBLE) >= %s AND CAST(\"DL\" AS DOUBLE) >= %s",
          format(OB.value, scientific = FALSE), format(DL.value, scientific = FALSE))
}
