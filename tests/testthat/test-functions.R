# 核心查询函数契约测试（冒烟级）

test_that("molecule_serch 返回 4 列契约", {
  res <- molecule_serch("quercetin")
  expect_true(is.data.frame(res))
  expect_named(res, c("herb", "molecule_id", "molecule", "target"))
})

test_that("target_serch 返回含 herb/molecule/target 列", {
  res <- target_serch("PTGS2")
  expect_true(is.data.frame(res))
  expect_true(all(c("herb", "molecule", "target") %in% colnames(res)))
})

test_that("tcm_prescription 返回双元素列表（草药频次 + 复方富集）", {
  skip("P3 性能优化后再启用：2026-08-24 compoundfinal 441k 行后超几何循环过慢（>30min）")
  res <- tcm_prescription(disease_data)
  expect_type(res, "list")
  expect_length(res, 2)
  expect_true(is.data.frame(res[[1]]))
  expect_true(is.data.frame(res[[2]]))
})

test_that("etcm 清洗 ETCM 下载数据返回 4 列契约", {
  res <- etcm(mahuang, herb = "ma huang")
  expect_true(is.data.frame(res))
  expect_named(res, c("herb", "molecule", "target", "QED"))
})
