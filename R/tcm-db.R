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

.tcm_parquet_path <- function(which = c("compat", "standard", "disease", "disease_target",
                                        "string_ppi")) {
  which <- match.arg(which)
  fn <- switch(which,
    compat         = "tcm_network_compat.parquet",
    standard       = "standard_herb_mol_target.parquet",
    disease        = "disease_main.parquet",
    disease_target = "disease_target.parquet",
    # STRING v12.0 人类 PPI（combined_score >= 700），见 docs/data-license-string-2026-10-20.md
    string_ppi     = "string_ppi_human.parquet"
  )
  p <- system.file("extdata", fn, package = "TCMNP")
  if (!nzchar(p)) {  # development mode (not installed): fall back to repo path
    p <- file.path("inst", "extdata", fn)
    if (!file.exists(p)) {
      stop(fn, " not found (run devtools::install() or check inst/extdata)")
    }
  }
  p
}

# ---------------------------------------------------------------------------
# 疾病层：检索条件构造 + 输入清洗（**唯一实现**，供 disease_serch / disease_target 共用）
#
# 为什么抽出来：两个函数必须用**同一套**匹配语义，否则「按同一个疾病名，
#   一个函数查得到、另一个查不到」—— 这种不一致极难排查（MEMORY 铁律 54）。
#
# 匹配语义：
#   · 名称：默认**精确**（`exact`），只有显式 `contains` 才子串放宽（名匹配精确优先）；
#   · ID（mesh/umls/disease_id）：走**边界**匹配 —— 本体 ID 字段可能是多值
#     （`;` 分隔，如 `C0342770;C2936826`），必须匹配「整段」而不是子串，
#     否则 `C034277` 会命中 `C0342770`（典型假通过）；
#   · ID 类输入只接受 `[A-Za-z0-9]+`，含正则元字符的值**显式拒绝**并提示，
#     而不是静默变成一次宽松匹配。
#
# ⚠️ DuckDB 细节：必须**逐值**生成一条完整表达式再 OR。把 `.sql_in(x)` 展开出的
#   `'a', 'b'` 直接塞进单条 `regexp_matches(col, '(^|;)' || v || '(;|$)')` 会被逗号
#   切成第三个参数 → 报 `Unrecognized Regex option 1`（2026-10-15 实测踩到）。
#
# 返回 list(where = <SQL 条件或 NULL>, x = <清洗后的输入>)
#' @noRd
.tcm_disease_where <- function(x, type = "disease_name", match = "exact") {
  type <- match.arg(type, c("disease_name", "mesh_id", "umls_id", "disease_id"))
  match <- match.arg(match, c("exact", "contains"))
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  x <- unique(trimws(x))
  if (type != "disease_name") {
    bad <- x[!grepl("^[A-Za-z0-9]+$", x)]
    if (length(bad)) {
      print(paste0(bad, " is/are not valid IDs for type = ", type, "."))
      x <- setdiff(x, bad)
    }
  }
  if (!length(x)) return(list(where = NULL, x = character()))

  where <- if (type == "disease_name") {
    if (identical(match, "exact")) {
      sprintf("lower(trim(disease_name)) IN (%s)", .sql_in(tolower(x)))
    } else {
      paste0("(", paste(sprintf("contains(lower(disease_name), %s)",
                                .sql_in(tolower(x))), collapse = " OR "), ")")
    }
  } else {
    conds <- vapply(x, function(v) {
      sprintf("regexp_matches(lower(%s), '(^|;)' || %s || '(;|$)')",
              type, .sql_in(tolower(v)))
    }, character(1), USE.NAMES = FALSE)
    paste0("(", paste(conds, collapse = " OR "), ")")
  }
  list(where = where, x = x)
}

# ---------------------------------------------------------------------------
# herb 实体层：把用户给的药名展开为**同一实体**下的全部检索键（P0-B / 2026-10-12）
#
# 背景：同一味药在源侧有多个拼法（`houpo` / `houpu`、`shenjincao` / `shenjingcao`），
#   而查询走**精确匹配**（`IN (...)`）→ 用户查其中一个只能拿到一部分行
#   （实测 厚朴：查 houpo 得 12,990 行、查 houpu 得 28,290 行，用户无从知道要查两个）。
# 做法：输入 → 命中的 `herb` 键 →（herb_key_map）→ `herb_id` → 该实体全部键。
# 默认**不启用**：`herb_target(resolve_herb = TRUE)` 才展开，保持既有语义向后兼容
#   （铁律：不改变默认口径；新能力显式开启）。
# ---------------------------------------------------------------------------
.tcm_herb_keymap_path <- function() {
  p <- system.file("extdata", "herb_key_map.parquet", package = "TCMNP")
  if (!nzchar(p)) {                      # development mode (not installed)
    p <- file.path("inst", "extdata", "herb_key_map.parquet")
    if (!file.exists(p)) {
      stop("herb_key_map.parquet not found (run 06_export.py to export the herb entity layer)")
    }
  }
  p
}

#' @noRd
.tcm_herb_resolve <- function(x, type = "Herb_cn_name") {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(character())
  p <- .tcm_herb_keymap_path()
  is_cn <- identical(type, "Herb_cn_name")
  check_col <- if (is_cn) "Herb_cn_name" else "Herb_name_pin_yin"
  # ★ 返回的键必须与**查询列同类型**：调用方随后用同一 `type` 去 WHERE 过滤，
  #   若混入另一类键，会既产生 "… is/are not in the datasets." 的误导噪音，
  #   也把「键类型」与「查询列」的对应关系搞乱（2026-10-12 实测踩到）。
  want <- if (is_cn) "cn" else "pinyin"
  # ① 用户输入命中的行 → 取 pinyin 键（compat 的 herb 键列）
  hit <- .tcm_filter(check_col, x, c(k = "Herb_name_pin_yin"))$k
  hit <- unique(hit[!is.na(hit) & nzchar(hit)])
  if (!length(hit)) return(character())
  # ② 键 → herb_id
  ids <- .tcm_query(sprintf(
    "SELECT DISTINCT herb_id FROM read_parquet('%s') WHERE herb_key IN (%s)",
    p, .sql_in(hit)))$herb_id
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) return(hit)
  # ③ herb_id → 该实体**同类型**的全部键
  ks <- .tcm_query(sprintf(
    "SELECT DISTINCT herb_key FROM read_parquet('%s')
     WHERE herb_id IN (%s) AND key_type = '%s'",
    p, .sql_in(ids), want))$herb_key
  ks <- ks[!is.na(ks) & nzchar(ks)]
  if (!length(ks)) hit else ks
}

.sql_in <- function(x) {
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (!length(x)) return("NULL")
  paste0("'", gsub("'", "''", as.character(x)), "'", collapse = ", ")
}

# ---------------------------------------------------------------------------
# herb 实体层：查询结果接实体（加 herb_id + 规范键 + 折叠同实体重复行）
#
# 为什么需要（2026-10-12）：四个查询函数（herb_target / target_herb /
#   molecule_serch / target_serch）此前一律以 `Herb_name_pin_yin`（拼音键）作为
#   药材身份 → 同一味药的两种源侧拼法（houpo / houpu）会**各占一行**，
#   用户看到"两味药"，按实体统计时被重复计数。
# 口径：`resolve_herb = TRUE` 时才生效（默认 FALSE → 返回列契约完全不变，
#   既有下游与测试不受影响）。
# ---------------------------------------------------------------------------
.tcm_keymap <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- .tcm_query(sprintf(
        "SELECT herb_key, herb_id, key_type, id_source FROM read_parquet('%s')",
        .tcm_herb_keymap_path()))
    }
    cache
  }
})

# herb_id → 规范**拼音**键（优先 id_source='HERB'，并列取字典序最小 → 确定性）
.tcm_canonical_key <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      km <- .tcm_keymap()
      km <- km[km$key_type == "pinyin", , drop = FALSE]
      km <- km[order(km$herb_id, km$id_source != "HERB", km$herb_key), , drop = FALSE]
      km <- km[!duplicated(km$herb_id), , drop = FALSE]
      cache <<- stats::setNames(km$herb_key, km$herb_id)
    }
    cache
  }
})

# herb_id → 规范**中文**名（herb_main.herb_cn；空则回退到 herb_id 本身）
.tcm_canonical_cn <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      p <- system.file("extdata", "herb_main.parquet", package = "TCMNP")
      if (!nzchar(p)) p <- file.path("inst", "extdata", "herb_main.parquet")
      if (!file.exists(p)) stop("herb_main.parquet not found (run 06_export.py)")
      mn <- .tcm_query(sprintf("SELECT herb_id, herb_cn FROM read_parquet('%s')", p))
      cn <- ifelse(is.na(mn$herb_cn) | !nzchar(mn$herb_cn), mn$herb_id, mn$herb_cn)
      cache <<- stats::setNames(cn, mn$herb_id)
    }
    cache
  }
})

#' @noRd
.tcm_attach_entity <- function(df) {
  if (!nrow(df)) {
    df$herb_id <- character(0)
    return(df)
  }
  km <- .tcm_keymap()
  id <- km$herb_id[match(df$herb, km$herb_key)]
  ck <- .tcm_canonical_key()
  canon <- unname(ck[id])
  df$herb <- ifelse(is.na(canon), df$herb, canon)   # 规范键（同实体不再多行）
  df$herb_id <- id
  df[!duplicated(df), , drop = FALSE]               # 折叠 houpo/houpu 这类同内容重复行
}

# ---------------------------------------------------------------------------
# 信任与溯源列通道（P1 的 qc_level 等只在 standard，compat 守 10 列契约）
#
# 为什么要开这个口（2026-10-12）：信任列若 R 侧完全取不到，等于"交付了但用不上"
#   —— 属半成品。做法：按**已有的查询结果**回查 standard，取最差（最保守）评级。
# 为什么取最差：compat 按 10 列判重，一行可能对应 standard 多行（如不同 Entrez）；
#   此时报最差评级才不误导（宁可让用户谨慎）。
# ---------------------------------------------------------------------------
.TCM_TRUST_COLS <- c("qc_level", "qc_level_reason", "cas_verified",
                     "latin_name_std", "latin_match_status")

#' @noRd
.tcm_attach_trust <- function(df) {
  if (!nrow(df)) {
    for (cc in .TCM_TRUST_COLS) df[[cc]] <- character(0)
    return(df)
  }
  std <- .tcm_parquet_path("standard")
  herbs <- unique(df$herb)
  q <- sprintf(
    "SELECT \"herb\" AS herb, \"mol_name\" AS mol, \"target_gene\" AS tgt,
            \"qc_level\" AS qc_level, \"qc_level_reason\" AS qc_reason,
            CAST(\"cas_verified\" AS VARCHAR) AS cas_verified,
            \"latin_name_std\" AS latin_name_std,
            \"latin_match_status\" AS latin_match_status,
            CASE \"qc_level\" WHEN 'Untrusted' THEN 0 WHEN 'Low' THEN 1
                              WHEN 'Medium' THEN 2 ELSE 3 END AS _rank
     FROM read_parquet('%s') WHERE \"herb\" IN (%s)",
    std, .sql_in(herbs))
  tr <- .tcm_query(q)
  if (!nrow(tr)) {
    for (cc in .TCM_TRUST_COLS) df[[cc]] <- NA_character_
    return(df)
  }
  # 取最差评级：按 (herb, mol, tgt) 排序后保留 rank 最小的一行
  tr <- tr[order(tr$herb, tr$mol, tr$tgt, tr$`_rank`), , drop = FALSE]
  tr <- tr[!duplicated(tr[, c("herb", "mol", "tgt")]), , drop = FALSE]
  idx <- match(paste(df$herb, df$molecule_id, df$target, sep = "\r"),
               paste(tr$herb, tr$mol, tr$tgt, sep = "\r"))
  df$qc_level <- tr$qc_level[idx]
  df$qc_level_reason <- tr$qc_reason[idx]
  df$cas_verified <- tr$cas_verified[idx]
  df$latin_name_std <- tr$latin_name_std[idx]
  df$latin_match_status <- tr$latin_match_status[idx]
  df
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
  # 大小写不敏感匹配（2026-10-16）：DuckDB 的 IN 字面匹配区分大小写，
  # 导致 molecule_serch("quercetin") 因库内为 "Quercetin" 而返回 0 行。
  # 两侧同包 lower()：中文无大小写（恒等，不影响），英文覆盖大小写变体。
  q <- sprintf("SELECT %s FROM read_parquet('%s') WHERE lower(\"%s\") IN (%s)",
               sel, p, .tcm_col_db(col), .sql_in(tolower(vals)))
  if (!is.null(extra_where)) q <- paste0(q, " AND ", extra_where)
  .tcm_query(q)
}

# 同 .tcm_filter，但允许追加**原生 SQL 表达式**列（如 CASE 计算列）。
# extra_select: named chr，name = 输出列名，value = 原始 SQL 表达式。
.tcm_filter_ex <- function(col, vals, col_aliases, extra_where = NULL, extra_select = NULL) {
  p <- .tcm_parquet_path()
  sel_parts <- vapply(names(col_aliases), function(nm) {
    sprintf("%s AS \"%s\"", .tcm_col_db(col_aliases[[nm]]), nm)
  }, character(1))
  if (!is.null(extra_select)) {
    sel_parts <- c(sel_parts, vapply(names(extra_select), function(nm) {
      sprintf("%s AS \"%s\"", extra_select[[nm]], nm)
    }, character(1)))
  }
  sel <- paste(sel_parts, collapse = ", ")
  # 大小写不敏感匹配（2026-10-16）：同 .tcm_filter，两侧 lower() 覆盖大小写变体。
  q <- sprintf("SELECT %s FROM read_parquet('%s') WHERE lower(\"%s\") IN (%s)",
               sel, p, .tcm_col_db(col), .sql_in(tolower(vals)))
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
    # 大小写不敏感（2026-10-16）：lower 两侧，让用户小写输入也能命中规范值；
    # 返回的 v 仍是数据集里的原始（规范）大小写。
    where <- sprintf("%s AND lower(\"%s\") IN (%s)", where, cdb, .sql_in(tolower(vals)))
  }
  # ORDER BY 不可省：DuckDB 对 parquet 是并行扫描，无 ORDER BY 时
  # `SELECT DISTINCT` 的行序**每次运行都可能不同**。上游一旦用 head(n) 取子集
  # （如 GUI 的查询候选项），同一输入两次就会给不同结果 —— 实测 flaky。
  # 别名 v 在此处可用；distinct 值量级仅 7k~34k，排序开销可忽略。
  q <- sprintf("SELECT DISTINCT \"%s\" AS v FROM read_parquet('%s') WHERE %s ORDER BY v",
               cdb, p, where)
  .tcm_query(q)$v
}

# DISTINCT values of a column (non-empty)
.tcm_distinct <- function(col) .tcm_distinct_where(col)

# 大小写不敏感地把用户输入 vals 归一到数据集里的规范（原始大小写）取值。
# 用于查询函数：用户可能输入 quercetin，而库内存的是 Quercetin。
# 返回 list(canonical = 规范值向量, unmatched = 未匹配的输入向量)。
.tcm_ci_match <- function(vals, hay) {
  vlower <- tolower(vals)
  hlower <- tolower(hay)
  idx <- match(vlower, hlower)          # NA 表示未匹配
  matched <- !is.na(idx)
  list(
    canonical = hay[idx[matched]],
    unmatched = vals[!matched]
  )
}

# 清洗多值 MOL ID 字段（铁律 28/54：单一真相源）。
# 源 parquet 的 MOL_ID 是 ';' / '|' 分隔的多值串，未映射成分处混入字面量 "NA"
# 或上游拼接残留 "NA|" 前缀（可出现在开头或中间）。只保留真实编号（非空且非 "NA"），
# 以 ';' 重连；纯 "NA"/空串收敛为 ""（保留行，符合「尽量全」口径）。
.tcm_clean_mol_id <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || s == "") return("")
    toks <- strsplit(s, "[;|]")[[1]]
    toks <- toks[toks != "" & toks != "NA"]
    paste(toks, collapse = ";")
  }, character(1))
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
# filter with an explicit CAST. Column names go through .tcm_col_db() so this
# stays collision-safe (see the map notes at the top of this file).
#
# ★ 口径变更（boss 2026-09-28 拍板「尽量全」）------------------------------
# 旧行为：只保留 OB>=x 且 DL>=y 的行 → 那些**源侧没有 OB/DL**的行（实测占
#   compat 的 80.33%，几乎都是 BATMAN predicted 边）被静默丢弃，用户看到的是
#   「少而准」但并非全部证据。
# 新行为（include_missing = TRUE，默认）：**缺失 OB/DL 的行一并保留**，并在结果
#   里以 `adme` 列标注（'ok' = 通过阈值；'no_adme' = 源侧无 OB/DL）。
#   传 include_missing = FALSE 可回到旧口径。
.tcm_adme_missing_sql <- function() {
  ob <- sprintf('"%s"', .tcm_col_db("OB"))
  dl <- sprintf('"%s"', .tcm_col_db("DL"))
  miss <- function(c) sprintf("(%1$s IS NULL OR UPPER(TRIM(%1$s)) IN ('', 'NA', 'NAN'))", c)
  sprintf("%s OR %s", miss(ob), miss(dl))
}

# 结果里的 adme 标注列表达式（'ok' / 'no_adme'）
.tcm_adme_flag_sql <- function() {
  sprintf("CASE WHEN (%s) THEN 'no_adme' ELSE 'ok' END", .tcm_adme_missing_sql())
}

.tcm_adme_where <- function(OB.value, DL.value, include_missing = TRUE) {
  base <- sprintf("CAST(\"%s\" AS DOUBLE) >= %s AND CAST(\"%s\" AS DOUBLE) >= %s",
                  .tcm_col_db("OB"), format(OB.value, scientific = FALSE),
                  .tcm_col_db("DL"), format(DL.value, scientific = FALSE))
  if (isTRUE(include_missing)) {
    sprintf("((%s) OR (%s))", base, .tcm_adme_missing_sql())
  } else {
    base
  }
}
