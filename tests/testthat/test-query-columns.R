# 查询函数列语义回归（2026-10-15 V2 修复，2026-10-16 补大小写兼容 + NA| 清洗）
#
# 背景：原 4 个查询函数（herb_target / molecule_serch / target_herb / target_serch）
#   把输出列 molecule_id 映射到源字段 Molecule_name（分子*名*），
#   把 molecule 映射到 MOL_ID（*编号*）——列名与内容整反。
#   铁律 28：既有 expect_named() 只查列名，抓不到此 bug；本次新增取值级断言。
#   4 个函数共用同一段 .tcm_filter_ex 映射，故以 molecule_serch 为探针即可覆盖。
#
# 2026-10-16 追加：
#   (a) 大小写兼容——查询走 .tcm_filter 的 lower() 包裹，molecule_serch("quercetin")
#       应与 "Quercetin" 命中同一批行；
#   (b) NA| 拼接残留清洗——molecule_id 不得出现 "NA|MOL..." 形式，统一收敛为 "MOL..."。

test_that("查询函数：molecule_id 装 MOL 编号、molecule 装分子名", {
  res <- utils::head(molecule_serch("Quercetin"), 50)
  expect_named(res, c("herb", "molecule_id", "molecule", "target"))

  # molecule_id 非缺失值须形如 MOL...（TCMSP 标准编号；Quercetin -> MOL000098）
  mid <- res$molecule_id[!is.na(res$molecule_id) & res$molecule_id != ""]
  expect_true(length(mid) > 0, info = "Quercetin 应有可映射的 MOL 编号")
  expect_true(all(grepl("^MOL", mid)),
    info = "molecule_id 应装 MOL 编号，不应是分子名（V2 修复前此处为分子名，列名/内容反了）")

  # molecule 装分子名，不应形如 MOL...
  mol <- res$molecule[!is.na(res$molecule) & res$molecule != ""]
  expect_true(all(!grepl("^MOL", mol)),
    info = "molecule 应装分子名，不应是 MOL 编号（V2 修复前此处为编号）")
})

# 大小写兼容（2026-10-16）：库内为 "Quercetin"，查小写 "quercetin" 须命中同一批。
test_that("查询函数：分子名大小写不敏感（quercetin == Quercetin）", {
  canon <- molecule_serch("Quercetin")
  lower <- molecule_serch("quercetin")
  expect_true(nrow(lower) > 0,
    info = "小写 quercetin 应命中（修复前因 DuckDB IN 区分大小写返回 0 行）")
  expect_equal(
    sort(unique(lower$molecule_id[lower$molecule_id != ""])),
    sort(unique(canon$molecule_id[canon$molecule_id != ""])),
    info = "小写查询命中的 MOL 编号集合应与规范值一致")
})

# 缺失 MOL ID 表示统一（2026-10-15 na_if 修复，2026-10-16 补 NA| 清洗）
#
# 背景：源 parquet 中未映射成分的 MOL_ID 混用字面量 "NA"、空串 ""、以及上游拼接
#   残留的 "NA|MOL..." 多值前缀；查询函数现统一为单一空串。若不收敛，tidyr
#   下游会对 "NA"/"NA|" 误判（铁律 28/54：单一真相源）。
# mahuang 同时含已映射(MOL...)与未映射(缺失)成分，是稳定探针。
test_that("查询函数：缺失 MOL ID 统一表示为空串（无 'NA' 字面量 / 'NA|' 前缀泄漏）", {
  res <- herb_target("mahuang", type = "Herb_name_pin_yin")
  mid <- res$molecule_id

  expect_false(any(mid == "NA", na.rm = TRUE),
    info = "molecule_id 不得含字面量 'NA'（应已被 na_if 收敛为空串）")
  expect_false(any(grepl("NA\\|", mid), na.rm = TRUE),
    info = "molecule_id 不得含 'NA|' 拼接残留（应已被 sub('^NA\\\\|') 剥离）")
  expect_true(all(mid == "" | grepl("^MOL", mid)),
    info = "molecule_id 只能是 '' 或 'MOL...' 两种取值，不得混入其他表示")
  # 证明统一逻辑真的跑到：存在缺失（空串）行，而非全部已映射
  expect_true(any(mid == ""),
    info = "mahuang 应含未映射成分（缺失 MOL ID 统一为空串），证明收敛逻辑生效")
  # 已映射行仍正确装 MOL 编号
  expect_true(any(grepl("^MOL", mid)),
    info = "mahuang 应含已映射成分（MOL 编号）")
})
