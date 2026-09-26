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
  expect_named(herb_pm, c("herb_key", "Herb_cn_name", "Herb_name_pinyin",
                          "Property", "Flavor", "Meridian"))
  expect_gt(nrow(herb_pm), 6000)
  # herb_key 必须非空且唯一（E3 修复的核心契约：可作连接键）
  expect_false(any(is.na(herb_pm$herb_key) | !nzchar(herb_pm$herb_key)))
  expect_false(any(duplicated(herb_pm$herb_key)))

  # trrust —— tf_filter() 依赖列
  expect_true(all(c("TF", "Target") %in% colnames(trrust)))
  expect_gt(nrow(trrust), 10000)

  # compoundfinal —— tcm_prescription() 依赖（复方-草药背景集）
  expect_true(all(c("herb", "compound", "type") %in% colnames(compoundfinal)))
  expect_gt(nrow(compoundfinal), 1000)

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
  # 行数 = 16,347,058（S3-S4 合并产出）
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
