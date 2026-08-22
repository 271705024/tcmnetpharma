# 数据集契约测试
# 目的：防止 P2 数据更新后结构静默变化（列名/维度/类型漂移）导致下游函数失效。
# 依据：2026-08-22 实测结构（data-raw 探查 log_probe_data.txt）。

test_that("核心数据集存在且结构稳定", {
  # xfbdf —— 示例处方数据
  expect_true(is.data.frame(xfbdf))
  expect_gt(nrow(xfbdf), 1000)
  expect_named(xfbdf, c("herb", "molecule_id", "molecule", "target"))

  # herb_pm —— tcm_compound() 依赖列（拼音/性味归经）
  expect_named(herb_pm, c("Herb_cn_name", "Herb_name_pinyin", "Property", "Flavor", "Meridian"))
  expect_equal(nrow(herb_pm), 409)

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

test_that("内部数据 tcmsp 结构稳定（核心依赖）", {
  tcmsp <- TCMNP:::tcmsp
  expect_true(is.data.frame(tcmsp))
  expect_gt(nrow(tcmsp), 400000)
  expect_named(tcmsp, c("Herb_cn_name", "HERB_NAME_PIN_YIN", "Herb_name_pin_yin",
                        "Molecule_name", "MOL_ID", "OB", "DL",
                        "Target_full_name", "Target"))
  expect_true(is.numeric(tcmsp$OB))
  expect_true(is.numeric(tcmsp$DL))
  # 关键列无全空
  expect_gt(sum(!is.na(tcmsp$Target)), 0)
  expect_gt(length(unique(tcmsp$Herb_cn_name)), 100)
})
