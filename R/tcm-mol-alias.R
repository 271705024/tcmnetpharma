# ---------------------------------------------------------------------------
# 分子实体别名通道（R3 修复）
#
# 为什么需要这一层（实测取证，2026-10-21）
# -----------------------------------------
# BATMAN 与 HERB 对**同一分子**使用不同立体形式的 InChIKey
# （前 14 位连接性层相同 → 同分子异立体）。于是：
#
#   BATMAN CID 137706164 → ASZIGQFYGXSPCO-PMNGFKOQSA-N → std 里有 67 行
#   HERB  CID 9970151   → ASZIGQFYGXSPCO-UBVLEBKSSA-N → std 里 0 行
#   mol_name = 'Britannilactone'                      → std 里 0 行
#
# → `molecule_serch("Britannilactone")` 查不到（检索走 compat 的 Molecule_name）。
#
# ⚠️ 为什么**不在数据侧回填** HERB 规范名（实测已否决）：
#   std 中 mol_name 为空的行 = 0 → 回填不是填空，是**覆盖** 7,912,535 行既有名称；
#   compat 行数 16,202,145 → 16,107,440（塌缩 94,705 行）；
#   按 HERB 名可检索的 entity 20,740 → 18,320（**反降 2,420**）。
#   → 改为另开别名层：standard / compat 一格不改，别名解析放在**检索层**。
#
# 机制
# ----
# 别名 → entity_key → 该 entity 在 standard 下的**全部** InChIKey
#      → 展开为该实体的所有 mol_name → 走原查询。
# 结果：用户输入一个名字（无论来自哪一库），拿到该分子的**全部**行。
#
# 契约：别名表 mol_entity_alias.parquet 列
#   entity_key / alias_name / alias_type / n_inkey / n_cid / n_rows / has_herb_named
# ---------------------------------------------------------------------------

#' @noRd
.tcm_mol_alias_path <- function() {
  fn <- "mol_entity_alias.parquet"
  p <- system.file("extdata", fn, package = "TCMNP")
  if (!nzchar(p)) {
    p <- file.path("inst", "extdata", fn)
    if (!file.exists(p)) return("")
  }
  p
}

# 别名 → 该实体的全部 std 内名字（mol_name）。
# 返回命名 list：
#   names    展开后的名字向量（可安全传给 .tcm_filter）
#   matched  命中的用户输入
#   unmatched 未命中的用户输入
#   detail   每个输入的解析明细（data.frame）
#
# 大小写不敏感：别名表与用户输入两侧 lower() 比对（与 .tcm_ci_match 同口径）。
.tcm_mol_alias_lookup <- function(vals) {
  empty <- list(names = character(0), matched = character(0),
                unmatched = character(0), detail = NULL)
  vals <- as.character(vals)
  vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
  if (!length(vals)) return(empty)
  p <- .tcm_mol_alias_path()
  if (!nzchar(p)) return(empty)
  std_p <- .tcm_parquet_path("standard")
  if (!nzchar(std_p)) return(empty)

  # ① 别名 → entity_key（**一个别名可对应多个 entity**：同名异物）
  q <- sprintf(
    "SELECT DISTINCT lower(\"alias_name\") AS a, \"entity_key\" AS ek
     FROM read_parquet('%s')", p)
  amap <- .tcm_query(q)
  if (!nrow(amap)) return(empty)

  vlower <- tolower(trimws(vals))
  hit <- vlower %in% amap$a
  if (!any(hit)) {
    return(list(names = character(0), matched = character(0),
                unmatched = vals, detail = NULL))
  }
  # 一个别名可能命中多个 entity_key（同名异物）→ 全部取回（不是取第一个）
  eks <- unique(amap$ek[amap$a %in% vlower[hit]])
  eks <- eks[!is.na(eks) & nzchar(eks)]

  # ② entity_key → 该实体在 standard 下的全部 mol_name
  #    InChIKey 前 14 位 = entity_key（连接性层）→ 同分子异立体一并纳入
  q2 <- sprintf(
    "SELECT DISTINCT \"mol_name\" AS m
     FROM read_parquet('%s')
     WHERE \"InChIKey\" IS NOT NULL AND \"mol_name\" IS NOT NULL
       AND \"mol_name\" <> ''
       AND regexp_extract(\"InChIKey\", '^[A-Z]{14}') IN (%s)",
    std_p, .sql_in(eks))
  nm <- .tcm_query(q2)$m
  nm <- nm[!is.na(nm) & nzchar(trimws(nm))]

  detail <- data.frame(
    input = vals,
    matched = hit,
    stringsAsFactors = FALSE
  )
  list(names = nm, matched = vals[hit], unmatched = vals[!hit], detail = detail)
}
