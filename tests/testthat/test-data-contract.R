# 数据集契约测试
# 目的：防止 P2 数据更新后结构静默变化（列名/维度/类型漂移）导致下游函数失效。
# 依据：2026-08-22 实测结构（data-raw 探查 log_probe_data.txt）。

test_that("核心数据集存在且结构稳定", {
  # xfbdf —— 示例处方数据
  expect_true(is.data.frame(xfbdf))
  expect_gt(nrow(xfbdf), 1000)
  expect_named(xfbdf, c("herb", "molecule_id", "molecule", "target"))

  # herb_pm —— tcm_compound() 依赖列（拼音/性味归经）；2026-08-24 更新：409 → HERB 全量 6,860
  expect_named(herb_pm, c("Herb_cn_name", "Herb_name_pinyin", "Property", "Flavor", "Meridian"))
  expect_gt(nrow(herb_pm), 6000)

  # trrust —— tf_filter() 依赖列
  expect_true(all(c("TF", "Target") %in% colnames(trrust)))
  expect_gt(nrow(trrust), 10000)

  # compoundfinal —— tcm_prescription() 依赖（复方-草药背景集）
  expect_true(all(c("herb", "compound", "type") %in% colnames(compoundfinal)))
  expect_gt(nrow(compoundfinal), 1000)

  # disease_data —— 示例疾病基因集
  expect_equal(nrow(disease_data), 790)

  # 通路基因集（KEGG/GSEA）—— 统一 term/gene 两列
  for (d in list(human_kegg = human_kegg, mu_kegg = mu_kegg,
                 human_gsea = human_gsea, mu_gsea = mu_gsea)) {
    expect_true(is.data.frame(d))
    expect_named(d, c("term", "gene"))
    expect_gt(nrow(d), 1000)
  }

  # mahuang —— etcm() 输入契约（ETCM 下载格式）
  expect_named(mahuang, c("Chemical Component", "Candidate Target genes"))
})

test_that("内部数据 tcm_network 惰性查询契约（P3.2，compat parquet 经 DuckDB）", {
  meta <- TCMNP:::.tcm_meta()
  # 行数 = 16,347,058（S3-S4 合并产出）
  expect_gt(meta$rows, 1.5e7)
  # 9 列契约（与 sysdata 时代一致的列集）
  want <- c("Herb_cn_name", "HERB_NAME_PIN_YIN", "Herb_name_pin_yin",
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
