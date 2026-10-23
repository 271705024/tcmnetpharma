# ===========================================================================
# 分子实体别名通道契约测试（R3 修复，2026-10-21）
#
# 背景
#   BATMAN 与 HERB 对**同一分子**使用不同立体形式 InChIKey（前 14 位连接性层
#   相同 → 同分子异立体）。于是 HERB 规范名（如 `Britannilactone`）在 standard
#   表中按名查不到 → `molecule_serch()` 返回 0 行 = 名字级假阴性。
#
#   R3 修复 = 新增独立别名表 `mol_entity_alias.parquet` + 检索层别名解析
#   （`molecule_serch(..., resolve_alias = TRUE)`，默认开启）。
#
# 本文件守护的契约
#   1. 别名表存在、列契约稳定、行数下限（防止「表没了但测试还绿」）
#   2. entity_key 形态合规（InChIKey 前 14 位）
#   3. alias_name 非空；alias_type 取值域受限
#   4. 别名通道**真的**能救回假阴性（正向）
#   5. 关闭别名通道时行为可复现（旧行为可回退）
#   6. 哨兵分子**不**误命中（防「万能匹配」）
#   7. 原有可查的名字**不被改写**（向后兼容）
#
# 回归锚点：docs/xuanfuhua-uc-precheck/CID桥接假阴性_修复进展与R3决策.md §7
# ===========================================================================

alias_tbl <- function() {
  p <- system.file("extdata", "mol_entity_alias.parquet", package = "tcmnetpharma")
  if (!nzchar(p)) return(NULL)
  arrow <- asNamespace("tcmnetpharma")$.tcm_query(sprintf(
    "SELECT * FROM read_parquet('%s')", p))
  arrow
}

test_that("别名表存在且列契约稳定", {
  p <- system.file("extdata", "mol_entity_alias.parquet", package = "tcmnetpharma")
  expect_true(nzchar(p), "mol_entity_alias.parquet 缺失 —— R3 别名通道不可用")
  expect_true(file.exists(p))
})

test_that("别名表内容合规：entity_key 形态 / alias_name 非空 / alias_type 取值域", {
  a <- alias_tbl()
  skip_if(is.null(a), "别名表不可读")

  expect_true(all(c("entity_key", "alias_name", "alias_type",
                    "n_inkey", "n_cid", "n_rows", "has_herb_named") %in% names(a)),
              info = "别名表列契约变化")

  # entity_key 必须是 InChIKey 前 14 位（大写字母）
  ek <- as.character(a$entity_key)
  expect_true(all(grepl("^[A-Z]{14}$", ek)),
              info = "entity_key 形态不合规（应为 InChIKey 前 14 位）")

  # alias_name 不得为 NA / 空串
  an <- as.character(a$alias_name)
  expect_false(any(is.na(an)))
  expect_true(all(nzchar(trimws(an))))

  # alias_type 只允许两种来源
  expect_true(all(as.character(a$alias_type) %in% c("std_name", "herb_norm")))

  # 两种来源都必须有（否则别名表无意义）
  expect_gt(sum(a$alias_type == "std_name"), 0)
  expect_gt(sum(a$alias_type == "herb_norm"), 0)

  # 行数下限（实测 39,388；设宽松下限防「静默截断」）
  expect_gt(nrow(a), 30000)
})

test_that("别名通道救回名字级假阴性（Britannilactone）", {
  # 该名字在 standard 表中按名查不到，但经别名通道必须查到
  r_on <- suppressWarnings(suppressMessages(
    molecule_serch("Britannilactone", resolve_alias = TRUE)))
  expect_gt(nrow(r_on), 0, "Britannilactone 仍查不到 —— 别名通道未生效")
  expect_true(all(c("herb", "molecule_id", "molecule", "target") %in% names(r_on)))
})

test_that("Inulicin 可查（曾因「唯一名过滤」被误剔除）", {
  # 回归锚点：初版 02g 按「同 entity 唯一名」过滤 → 把 Inulicin 连带剔除
  #          → 反而制造了新的假阴性。修正后必须可查。
  r_on <- suppressWarnings(suppressMessages(
    molecule_serch("Inulicin", resolve_alias = TRUE)))
  expect_gt(nrow(r_on), 0, "Inulicin 查不到 —— 唯一名过滤是否已去除？")
})

test_that("关闭别名通道时旧行为可复现（可回退）", {
  r_off <- suppressWarnings(suppressMessages(
    molecule_serch("Britannilactone", resolve_alias = FALSE)))
  expect_equal(nrow(r_off), 0,
               info = "resolve_alias=FALSE 时 Britannilactone 不应命中（原路径查不到）")
})

test_that("哨兵分子不误命中（防万能匹配）", {
  r <- suppressWarnings(suppressMessages(
    molecule_serch("__NOT_A_REAL_MOLECULE_XYZ__", resolve_alias = TRUE)))
  expect_equal(nrow(r), 0)
})

test_that("原有可查的名字不被改写（向后兼容）", {
  # quercetin 原本就能逐字查到 → 别名通道不得改变其返回集合规模
  r <- suppressWarnings(suppressMessages(
    molecule_serch("quercetin", resolve_alias = TRUE)))
  expect_gt(nrow(r), 0)
  r_off <- suppressWarnings(suppressMessages(
    molecule_serch("quercetin", resolve_alias = FALSE)))
  expect_equal(nrow(r), nrow(r_off),
               info = "quercetin 的返回行数在别名通道开关下不一致（向后兼容被破坏）")
})
