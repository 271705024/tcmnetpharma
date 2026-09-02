# 内部工具：tcm_network_compat.parquet 惰性查询（DuckDB 直读，P3.2 2026-09-02）
# 设计：函数不整表载入 16M 行 compat，一律 SQL 过滤后取小结果进 R。

# duckdb 读 parquet 列名大小写折叠（duckdb 1.5.x 行为）：
# HERB_NAME_PIN_YIN 与 Herb_name_pin_yin 折叠冲突 → 后者被自动命名 Herb_name_pin_yin_1。
# 函数侧一律用"逻辑列名"（Herb_name_pin_yin），SQL 生成时映射为 duckdb 实际名。
.tcm_col_db <- function(nm) {
  if (nm == "Herb_name_pin_yin") "Herb_name_pin_yin_1" else nm
}
.tcm_col_clean <- function(nm) sub("_1$", "", nm)

.tcm_parquet_path <- function() {
  p <- system.file("extdata", "tcm_network_compat.parquet", package = "TCMNP")
  if (!nzchar(p)) {  # 开发模式（未安装）回退仓库路径
    p <- file.path("inst", "extdata", "tcm_network_compat.parquet")
    if (!file.exists(p)) {
      stop("找不到 tcm_network_compat.parquet（请先 devtools::install() 或确认 inst/extdata）")
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

# 按列过滤 compat 表，返回指定列（SQL 别名即输出列名）
# col_aliases: 命名向量 c(输出名 = "表列名")；extra_where: SQL 片段
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

# DISTINCT 取值（带 WHERE 子集过滤，避免全表扫描）
.tcm_distinct_where <- function(col, where) {
  p <- .tcm_parquet_path()
  q <- sprintf("SELECT DISTINCT \"%s\" AS v FROM read_parquet('%s') WHERE %s",
               .tcm_col_db(col), p, where)
  .tcm_query(q)$v
}

# 全表 DISTINCT 列值（去空）
.tcm_distinct <- function(col) {
  cdb <- .tcm_col_db(col)
  .tcm_distinct_where(col, sprintf("\"%s\" IS NOT NULL AND \"%s\" <> ''", cdb, cdb))
}

# compat 表基础信息（测试/审计用；列名经 clean 还原逻辑名）
.tcm_meta <- function() {
  p <- .tcm_parquet_path()
  q <- sprintf("SELECT COUNT(*) AS n FROM read_parquet('%s')", p)
  n <- .tcm_query(q)$n
  q2 <- sprintf(
    "SELECT column_name, column_type FROM (DESCRIBE SELECT * FROM read_parquet('%s'))", p)
  cols <- .tcm_query(q2)
  list(rows = n, columns = setNames(cols$column_type, vapply(cols$column_name, .tcm_col_clean, character(1))))
}

# OB/DL 为 VARCHAR（Python 侧源特征，含 'NA' 占位）——过滤时显式 CAST
.tcm_adme_where <- function(OB.value, DL.value) {
  sprintf("CAST(\"OB\" AS DOUBLE) >= %s AND CAST(\"DL\" AS DOUBLE) >= %s",
          format(OB.value, scientific = FALSE), format(DL.value, scientific = FALSE))
}
