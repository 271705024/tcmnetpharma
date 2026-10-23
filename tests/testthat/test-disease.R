# ===========================================================================
# 疾病维度查询与疾病主表契约（2026-10-15）
#
# 背景：`layer_a_disease_ontology_map`（7,157 行）自 2026-10-14 进包后
#   **在 R 侧零引用** —— 「资产造得再对，消费侧取不到就是没接」（MEMORY 铁律 66）。
#   本文件锁三件事：
#   ① `disease_serch()` 四种检索口径都可用，且**默认精确**（不做静默放宽）；
#   ② 本体 ID 是**多值字段**（`;` 分隔）→ 成员匹配必须走**边界**，不得退化成子串；
#   ③ `disease_main` 与 `layer_a_disease_ontology_map` **两表自洽**
#      （前者的 matched 子集 == 后者的 disease_id 全集）。
# ===========================================================================

.tcm_query        <- get(".tcm_query",        envir = getNamespace("tcmnetpharma"))
.sql_in           <- get(".sql_in",           envir = getNamespace("tcmnetpharma"))
.tcm_parquet_path <- get(".tcm_parquet_path", envir = getNamespace("tcmnetpharma"))

disease_main_path <- function() {
  p <- system.file("extdata", "disease_main.parquet", package = "tcmnetpharma")
  if (!nzchar(p)) p <- file.path("inst", "extdata", "disease_main.parquet")
  if (!file.exists(p)) {
    # 缺数据必须**硬失败**：静默 skip 会让「没检查项」看起来像「通过」（铁律 28）
    stop("disease_main.parquet 未导出（先跑 data-raw/cleaning/06_export.py）")
  }
  p
}

test_that("disease_serch 四种口径可用，返回列契约固定", {
  expect_named(disease_serch("Cataract 13 With Adult I Phenotype"),
               c("disease_id", "disease_name", "mesh_id", "umls_id", "matched"))
  expect_gt(nrow(disease_serch("Cataract 13 With Adult I Phenotype")), 0)

  # 按 SymMap disease_id
  by_id <- disease_serch(c("5", "16"), type = "disease_id")
  expect_equal(sort(by_id$disease_id), c("16", "5"))
  expect_true(all(by_id$disease_name != ""))

  # 按 MeSH / UMLS
  expect_equal(nrow(disease_serch("D021921", type = "mesh_id")), 1L)
  expect_gt(nrow(disease_serch("C3805373", type = "umls_id")), 0L)
})

test_that("默认精确匹配，只有 match = 'contains' 才放宽", {
  exact <- disease_serch("thalassemia")
  loose <- disease_serch("thalassemia", match = "contains")
  # ⚠️ 不得断言「精确匹配必然 0 行」—— 库里**确实**存在名为 `thalassemia` 的疾病条目
  #    （2026-10-15 实测 nrow(exact) = 1）。那种断言是**拿偶然事实当契约**，
  #    检查写严会造假失败（MEMORY 铁律 68）。此处只锁语义：
  #    精确 = 命中行名与输入等价；模糊 = 范围更宽且含非等价名。
  if (nrow(exact)) {
    expect_true(all(tolower(exact$disease_name) == "thalassemia"))
  }
  expect_gt(nrow(loose), 0L)
  expect_gte(nrow(loose), nrow(exact))
  expect_true(any(tolower(loose$disease_name) != "thalassemia"))
})

test_that("多值本体 ID 走边界匹配，不退化成子串", {
  # `C0342770;C2936826` —— 成员应能查到（多值字段的成员匹配）
  hit <- disease_serch("C2936826", type = "umls_id")
  expect_gt(nrow(hit), 0L)
  expect_true(any(grepl("C2936826", hit$umls_id)))
  # 但**前缀不得命中**（'C034277' 是 'C0342770' 的真前缀 → 子串匹配会误命中）
  expect_equal(nrow(disease_serch("C034277", type = "umls_id")), 0L)
  # disease_id 同理：'1' 不得命中 '16' / '1234'
  expect_equal(disease_serch("1", type = "disease_id")$disease_id, "1")
})

test_that("未命中时返回 0 行且列契约不变", {
  r <- disease_serch("NoSuchDiseaseXYZ")
  expect_equal(nrow(r), 0L)
  expect_named(r, c("disease_id", "disease_name", "mesh_id", "umls_id", "matched"))
  # 非法 ID（含非字母数字）应被拒绝而不是变成宽松匹配
  r2 <- disease_serch("C123%", type = "umls_id")
  expect_equal(nrow(r2), 0L)
})

test_that("disease_main 契约：身份唯一、多值已归一、占位已清", {
  p <- disease_main_path()
  agg <- .tcm_query(sprintf("
    SELECT count(*) AS n, count(DISTINCT disease_id) AS n_id,
      sum(CASE WHEN matched THEN 1 ELSE 0 END) AS n_matched,
      sum(CASE WHEN mesh_id <> '' THEN 1 ELSE 0 END) AS n_mesh,
      sum(CASE WHEN umls_id <> '' THEN 1 ELSE 0 END) AS n_umls
    FROM read_parquet('%s')", p))
  expect_equal(agg$n, 14434L)
  expect_equal(agg$n_id, agg$n)          # disease_id 唯一（否则查询会返回重复疾病）
  expect_gt(agg$n_matched, 0L)
  expect_gt(agg$n_mesh, 0L)
  expect_gt(agg$n_umls, 0L)

  # 多值字段已**去重 + 排序**：同一 ID 不得在字段内出现两次
  dup_in_field <- .tcm_query(sprintf("
    SELECT count(*) AS n FROM read_parquet('%s')
    WHERE umls_id LIKE '%%;%%' AND (
      list_count(list_distinct(string_split(umls_id, ';'))) <> list_count(string_split(umls_id, ';')))", p))
  expect_equal(dup_in_field$n, 0L)

  # 占位字符串不得冒充 ID（源侧是 'NAN'，已在导出时清成空）
  ph <- .tcm_query(sprintf("
    SELECT
      sum(CASE WHEN upper(mesh_id) IN ('NAN','NA','NONE','NULL') THEN 1 ELSE 0 END) AS m,
      sum(CASE WHEN upper(umls_id) IN ('NAN','NA','NONE','NULL') THEN 1 ELSE 0 END) AS u
    FROM read_parquet('%s')", p))
  expect_equal(ph$m + ph$u, 0L)
})

test_that("disease_main 与 layer_a_disease_ontology_map 两表自洽", {
  dm <- disease_main_path()
  om <- system.file("extdata", "layer_a_disease_ontology_map.parquet", package = "tcmnetpharma")
  if (!nzchar(om)) om <- file.path("inst", "extdata", "layer_a_disease_ontology_map.parquet")
  expect_true(file.exists(om))

  # matched = TRUE 的子集，其 disease_id 集合应与 ontology_map 完全一致
  #   （ontology_map 是「已对齐本体」的权威子集；两者若漂移，用户按任一表查都会不一致）
  cmp <- .tcm_query(sprintf("
    SELECT
      (SELECT count(*) FROM read_parquet('%s') WHERE matched) AS dm_matched,
      (SELECT count(*) FROM read_parquet('%s')) AS om_rows,
      (SELECT count(*) FROM (
         SELECT disease_id FROM read_parquet('%s') WHERE matched
         EXCEPT SELECT disease_id FROM read_parquet('%s'))) AS only_dm,
      (SELECT count(*) FROM (
         SELECT disease_id FROM read_parquet('%s')
         EXCEPT SELECT disease_id FROM read_parquet('%s') WHERE matched)) AS only_om",
    dm, om, dm, om, om, dm))
  expect_equal(cmp$dm_matched, cmp$om_rows)
  expect_equal(cmp$only_dm, 0L)
  expect_equal(cmp$only_om, 0L)
})

# ===========================================================================
# 疾病 → 靶点关联（2026-10-15，boss「纳入」）
# ===========================================================================
dt_path <- function() {
  p <- system.file("extdata", "disease_target.parquet", package = "tcmnetpharma")
  if (!nzchar(p)) p <- file.path("inst", "extdata", "disease_target.parquet")
  if (!file.exists(p)) {
    stop("disease_target.parquet 未导出（先跑 data-raw/cleaning/02e_disease_target.py）")
  }
  p
}

test_that("disease_target 返回固定列契约，且能查到已知关联", {
  r <- disease_target("Thalassemia")
  expect_named(r, c("disease_id", "disease_name", "ot_disease_id", "target_symbol",
                    "target_entrez", "target_id", "target_ensembl", "score",
                    "evidence_count", "n_ot_disease"))
  expect_gt(nrow(r), 0)
  # 内容锚点（领域事实，非偶然）：地中海贫血与 HBB 的关联必然存在且分数不高不了
  expect_true("HBB" %in% r$target_symbol)
  expect_gt(max(r$score[r$target_symbol %in% "HBB"], na.rm = TRUE), 0.1)
  # 分数是 OpenTargets 的口径：0–1
  expect_true(all(r$score >= 0 & r$score <= 1))
  expect_true(all(r$n_ot_disease >= 1))
})

test_that("min_score 单调：阈值越高行数不增；过高时明确提示且返回空表", {
  a <- disease_target("Thalassemia")
  b <- disease_target("Thalassemia", min_score = 0.5)
  expect_lte(nrow(b), nrow(a))
  expect_true(all(b$score >= 0.5))
  c <- disease_target("Thalassemia", min_score = 0.99)
  expect_equal(nrow(c), 0L)
  expect_named(c, names(a))          # 空表列契约一致
})

test_that("按本体 ID 查（与 disease_serch 同一套匹配语义）", {
  r <- disease_target("C1845055", type = "umls_id")
  expect_gt(nrow(r), 0L)
  # 前缀不得误命中（边界匹配，同 disease_serch）
  expect_equal(nrow(disease_target("C184505", type = "umls_id")), 0L)
  # 非法 ID 显式拒绝
  expect_equal(nrow(disease_target("C184%", type = "umls_id")), 0L)
  # 未命中疾病
  r2 <- disease_target("NoSuchDiseaseXYZ")
  expect_equal(nrow(r2), 0L)
})

test_that("disease_target 表契约：行数、引用完整性、去重键唯一", {
  p <- dt_path()
  agg <- .tcm_query(sprintf("
    SELECT count(*) AS n,
           count(DISTINCT disease_id) AS n_disease,
           sum(CASE WHEN target_entrez IS NOT NULL AND target_entrez <> '' THEN 1 ELSE 0 END) AS n_ez,
           sum(CASE WHEN score < 0 OR score > 1 THEN 1 ELSE 0 END) AS bad_score,
           min(score) AS min_s, max(score) AS max_s
    FROM read_parquet('%s')", p))
  # 行数与 DATA-README 版本表一致（06_export 的 verify_data_readme 会双向校验）
  # 2026-10-15：3,134,586 → **3,134,397**，因为修掉了 target_dict.Ensembl_id 多义
  #   导致的 LEFT JOIN 放大（189 组 / 378 行 → 唯一）。
  expect_equal(agg$n, 3134397L)
  expect_gt(agg$n_disease, 0L)
  expect_gt(agg$n_ez, 0L)
  expect_equal(agg$bad_score, 0L)              # score 必须落在 OpenTargets 的 0–1 区间
  expect_lte(agg$max_s, 1.0)

  # 去重键 (disease_id, target_ensembl) 唯一（否则同一疾病-靶点会重复下发）
  dup <- .tcm_query(sprintf("
    SELECT count(*) AS n FROM (
      SELECT disease_id, target_ensembl, count(*) AS c FROM read_parquet('%s')
      GROUP BY 1, 2 HAVING c > 1)", p))
  expect_equal(dup$n, 0L)

  # 引用完整性：疾病-靶点的疾病必须都在疾病主表里（两表不得漂移）
  orphan <- .tcm_query(sprintf("
    SELECT count(*) AS n FROM (
      SELECT DISTINCT disease_id FROM read_parquet('%s')
      EXCEPT SELECT disease_id FROM read_parquet('%s'))", p, disease_main_path()))
  expect_equal(orphan$n, 0L)
})
