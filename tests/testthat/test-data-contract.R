# 数据集契约测试
# 目的：防止 P2 数据更新后结构静默变化（列名/维度/类型漂移）导致下游函数失效。
# 依据：2026-08-22 实测结构（data-raw 探查 log_probe_data.txt）。

test_that("核心数据集存在且结构稳定", {
  # xfbdf —— 示例处方数据
  expect_true(is.data.frame(xfbdf))
  expect_gt(nrow(xfbdf), 1000)
  expect_named(xfbdf, c("herb", "molecule_id", "molecule", "target"))

  # herb_pm —— tcm_compound() 依赖列（拼音/性味归经）；2026-08-24 更新：409 → HERB 全量 6,860
  # 2026-09-22 更新：新增首列 herb_key（E3 修复产物，见下）。
  #   E3 根因：herb_pm 原以 Herb_name_pinyin 作跨表连接键，但各源拼音格式不一致
  #   （空格/连字符/大小写差异），导致 tcm_compound() 的 join 静默全 miss。
  #   方案：物化规范化键 herb_key 作为唯一连接列。
  #   回归锚点：logs/errors-xuanfuhua-e2e-2026-09-21.md §E3；data-raw/tools/export_herbpm.R
  # 2026-10-14 更新：新增 herb_id（实体身份）——缘由与 compoundfinal 同：
  #   只有拼音键时，查询结果里的**异拼**（houpu 之于 houpo）拿不到性味归经且**不报错**。
  expect_named(herb_pm, c("herb_key", "Herb_cn_name", "Herb_name_pinyin",
                          "Property", "Flavor", "Meridian", "herb_id"))
  expect_gt(nrow(herb_pm), 6000)
  # herb_key 必须非空且唯一（E3 修复的核心契约：可作连接键）
  expect_false(any(is.na(herb_pm$herb_key) | !nzchar(herb_pm$herb_key)))
  expect_false(any(duplicated(herb_pm$herb_key)))
  # herb_id 全量覆盖（实体层接入的前提条件；缺了就不是"接入了"）
  expect_false(any(is.na(herb_pm$herb_id) | !nzchar(herb_pm$herb_id)))

  # trrust —— tf_filter() 依赖列
  expect_true(all(c("TF", "Target") %in% colnames(trrust)))
  expect_gt(nrow(trrust), 10000)

  # compoundfinal —— tcm_prescription() 依赖（复方-草药背景集）
  expect_true(all(c("herb", "compound", "type") %in% colnames(compoundfinal)))
  expect_gt(nrow(compoundfinal), 1000)
  # 2026-10-14：必须带 herb_key / herb_id —— 否则 tcm_prescription 的超几何检验
  #   只能在"名字空间"里做，实测仅 15.8% 的复方药材能进入统计（见 06_export 注释）。
  expect_true(all(c("herb_key", "herb_id") %in% colnames(compoundfinal)))
  expect_gt(mean(nzchar(compoundfinal$herb_id)), 0.8)   # 行级解析率下限（实测 0.908）

  # disease_data —— 示例疾病基因集
  expect_equal(nrow(disease_data), 790)

  # 可再分发基因集（GO + Reactome，替换 KEGG/MSigDB）—— 统一 term/gene 两列
  gs_sets <- list(human_reactome = human_reactome, mu_reactome = mu_reactome,
                  human_go = human_go, mu_go = mu_go)
  for (d in gs_sets) {
    expect_true(is.data.frame(d))
    expect_named(d, c("term", "gene"))
    expect_gt(nrow(d), 1000)
    expect_gt(length(unique(d$gene)), 1000)          # 基因覆盖
    expect_true(all(nzchar(d$term) & nzchar(d$gene))) # 无空值
    expect_false(any(duplicated(d)))                  # 无重复行
  }
  # term 命名空间校验：GO 前缀 GO:；Reactome 前缀 R-HSA-/R-MMU-
  expect_true(all(grepl("^GO:", human_go$term)))
  expect_true(all(grepl("^GO:", mu_go$term)))
  expect_true(all(grepl("^R-HSA-", human_reactome$term)))
  expect_true(all(grepl("^R-MMU-", mu_reactome$term)))
  # 经典通路抽查（同源人类/小鼠应都覆盖）
  expect_true(any(human_reactome$term == "R-HSA-70171")) # Glycolysis
  expect_true(any(mu_reactome$term == "R-MMU-70171"))
  expect_true(any(human_go$term == "GO:0006915"))        # apoptotic process
  expect_true(any(mu_go$term == "GO:0006915"))

  # mahuang —— etcm() 输入契约（ETCM 下载格式）
  expect_named(mahuang, c("Chemical Component", "Candidate Target genes"))
})

test_that("内部数据 tcm_network 惰性查询契约（P3.2，compat parquet 经 DuckDB）", {
  meta <- TCMNP:::.tcm_meta()
  # 行数 = 16,269,155（S3-S4 合并 + Layer A 融合后落盘值；dedup 后；> 1.5e7 阈值满足）
  expect_gt(meta$rows, 1.5e7)
  # 9 列契约（与 sysdata 时代一致的列集）
  want <- c("Herb_cn_name", "herb_pinyin_raw", "Herb_name_pin_yin",
            "Molecule_name", "MOL_ID", "OB", "DL", "Target_full_name", "Target")
  expect_true(all(want %in% names(meta$columns)))
  # OB/DL 类型：VARCHAR（Python 侧源特征，含 'NA' 占位）——过滤用 CAST 解析
  expect_true(meta$columns[["OB"]] %in% c("VARCHAR", "DOUBLE", "FLOAT", "INTEGER", "BIGINT"))
  expect_true(meta$columns[["DL"]] %in% c("VARCHAR", "DOUBLE", "FLOAT", "INTEGER", "BIGINT"))
  # 关键链可查（Target=PTGS2 且 ADME 达标——CAST 后过滤）
  hit <- TCMNP:::.tcm_query(sprintf(
    "SELECT COUNT(*) AS n FROM read_parquet('%s') WHERE Target IN ('PTGS2') AND %s",
    TCMNP:::.tcm_parquet_path(), TCMNP:::.tcm_adme_where(30, 0.18)))
  expect_gt(hit$n, 0)
  # DISTINCT 草药数（背景集规模，应显著大于旧版 499）
  hb <- TCMNP:::.tcm_distinct("Herb_cn_name")
  expect_gt(length(hb), 1000)
})

test_that("Layer A 一次源疾病本体映射表存在且结构稳定 (2026-10-14 并入)", {
  # 与上方 tcm_network 惰性查询契约同款定位：已安装走 system.file，未安装走 inst/extdata
  p <- tryCatch(
    system.file("extdata", "layer_a_disease_ontology_map.parquet", package = "TCMNP"),
    error = function(e) ""
  )
  if (is.null(p) || !nzchar(p)) {
    p <- file.path("inst", "extdata", "layer_a_disease_ontology_map.parquet")
  }
  testthat::skip_if_not(file.exists(p),
                        "layer_a_disease_ontology_map.parquet 不可用（开发期未构建）")
  # duckdb 在 Imports 中，读取比 arrow 在 C-locale 下更稳（避免 string 列读数异常）
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tbl <- DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", p))
  # 实测 7,157 行；阈值留缓冲
  expect_gt(nrow(tbl), 1000)
  # 本体 ID 列（铁律 28：关键列必须显式守护，不能「没检查项 = 通过」）
  expect_true(all(c("mesh_id", "umls_id") %in% colnames(tbl)))
  # 排除字面占位符（NA/NAN/NONE）后仍有真实本体 ID（否则该表无实际价值）
  has_id <- function(x) {
    s <- as.character(x)
    nzchar(s) & !(toupper(trimws(s)) %in% c("NA", "NAN", "NONE", "NULL", ""))
  }
  n_id <- sum(has_id(tbl$mesh_id) | has_id(tbl$umls_id))
  # 实测 7,157 行均含真实本体 ID
  expect_gt(n_id, 1000)
})

# ===========================================================================
# 宽表 / 兼容表 schema 契约（P0-B + P1 接入，2026-10-14）
# ===========================================================================
test_that("compat 恒 10 列，standard 已接实体层与信任/溯源列", {
  # compat = 10 列是**硬契约**（D-compat=A）：5 个查询函数与 GUI 都按它写，
  #   新增列会影响 `expect_named` 断言与下游构造 → 必须显式改，不能悄悄长。
  cc <- TCMNP:::.tcm_query(sprintf("SELECT * FROM read_parquet('%s') LIMIT 0",
                                    TCMNP:::.tcm_parquet_path("compat")))
  expect_equal(ncol(cc), 10L)

  sc <- TCMNP:::.tcm_query(sprintf("SELECT * FROM read_parquet('%s') LIMIT 0",
                                    TCMNP:::.tcm_parquet_path("standard")))
  expect_true(all(c("herb_entity_id", "herb_entity_id_source",
                    "qc_level", "qc_level_reason", "cas_verified", "cas_source",
                    "latin_name_std", "latin_match_status",
                    # Layer A 一次源交叉引用（2026-10-15 纳入）
                    "layer_a_latin", "layer_a_hit_id") %in% names(sc)))
  # ⚠️ 不得存在「仅以大小写区分」的列名：DuckDB 列名匹配不区分大小写，
  #    查"不带后缀的名字"会**静默命中另一列**（2026-10-12 实测：herb_id vs Herb_id
  #    被 common.assert_no_col_case_collision 在落盘前拦下）。此处再加一道。
  expect_false(any(duplicated(tolower(names(sc)))))
  expect_false(any(duplicated(tolower(names(cc)))))
})

# ===========================================================================
# Layer A 两列契约（2026-10-15 纳入）
# ===========================================================================
test_that("standard 的 Layer A 两列：覆盖 > 0、hit 形态合规、键→值函数性", {
  sp <- TCMNP:::.tcm_parquet_path("standard")
  agg <- TCMNP:::.tcm_query(sprintf("
    SELECT count(*) AS n,
      sum(CASE WHEN layer_a_latin  IS NOT NULL AND trim(layer_a_latin)  <> '' THEN 1 ELSE 0 END) AS n_lat,
      sum(CASE WHEN layer_a_hit_id IS NOT NULL AND trim(layer_a_hit_id) <> '' THEN 1 ELSE 0 END) AS n_hit
    FROM read_parquet('%s')", sp))
  # left join 语义：未命中为 NA **合法** → 只要求「不是整列空」
  #   （防「列在但全空」这种看起来正常的失败模式，铁律 28）
  expect_gt(agg$n_lat, 0)
  expect_gt(agg$n_hit, 0)

  # hit_id 形态：SymMap 靶点实验标记（T + 数字，实测值如 T1237）
  bad <- TCMNP:::.tcm_query(sprintf("
    SELECT count(*) AS n FROM read_parquet('%s')
    WHERE layer_a_hit_id IS NOT NULL AND trim(layer_a_hit_id) <> ''
      AND NOT regexp_matches(trim(layer_a_hit_id), '^T[0-9]+$')", sp))
  expect_equal(bad$n, 0L)

  # 键→latin 函数性：left join `.map` 的前提（否则同一键取值随机）
  multi <- TCMNP:::.tcm_query(sprintf("
    SELECT count(*) AS n FROM (
      SELECT herb, count(DISTINCT layer_a_latin) AS c FROM read_parquet('%s')
      WHERE layer_a_latin IS NOT NULL GROUP BY 1 HAVING c > 1)", sp))
  expect_equal(multi$n, 0L)

  # ⚠️ 语义区分（防误用）：`layer_a_latin` 是**拉丁药材名**（`Radix Et Rhizoma …`），
  #    与 `latin_name_std`（实体层植物学二名法）**不是同一回事**。
  #    断言二者取值几乎不重合（实测 11.7%）—— 若有人把两列搞混、互相赋值，这条会红。
  pair <- TCMNP:::.tcm_query(sprintf("
    SELECT
      sum(CASE WHEN layer_a_latin IS NOT NULL AND latin_name_std IS NOT NULL THEN 1 ELSE 0 END) AS both_nn,
      sum(CASE WHEN layer_a_latin IS NOT NULL AND latin_name_std IS NOT NULL
               AND lower(trim(layer_a_latin)) = lower(trim(latin_name_std)) THEN 1 ELSE 0 END) AS same
    FROM read_parquet('%s')", sp))
  expect_gt(pair$both_nn, 0)                 # 两列确有同时有值的行
  expect_lt(pair$same / pair$both_nn, 0.9)   # 但取值不重合 → 语义确实不同（阈值宽松只防事故）

  # ⚠️ **归一化**（无内嵌换行 / 纯 ASCII）**不在本文件断言** —— 由 `data-raw/cleaning/05_qc.py`
  #    的 **F 层**守（同一事实只在一处声明，铁律 54）。当前实测有真实缺陷
  #    （内嵌换行 8 值 / 94,535 行；非 ASCII 5 值 / 93,717 行，如 `Ⅴladimiriae Radix`、
  #    `Chuanxiong Rhizoma,…；Rhizoma Chuanxiong`），F 层已如实 FAIL ⛔。
  #    数据侧归一后，此处再收紧为硬断言。
})
