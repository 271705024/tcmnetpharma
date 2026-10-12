# ===========================================================================
# herb 实体层契约测试（P0-B，2026-10-12）
#
# 背景：拼音键此前同时充当「检索入口」与「实体身份」——
#   同一味药在源侧有多种拼法（houpo / houpu、shenjincao / shenjingcao），
#   而查询走精确匹配 → 用户查一个键只能拿到一部分行（实测 厚朴 12,990 vs 28,290）。
#
# 本文件锁三件事：
#   ① `herb_target(resolve_herb = FALSE)`（默认）**行为不变**（向后兼容）；
#   ② `resolve_herb = TRUE` 时能把输入展开到同一实体的全部键，且**只会多不会少**；
#   ③ herb_key_map 的**键→herb_id 是函数**（一对多即数据事故，必须硬失败）。
# ===========================================================================

# 内部函数取用器：testthat 的 test_file() 不会把包命名空间注入测试环境，
# 故内部函数必须显式从 TCMNP 命名空间取（与 test-tcm-db.R 的 .tdb_get 同构）。
# 2026-10-12 实测：不这样做会以 "could not find function .tcm_query" 报 ERROR（err=3）。
.tcm_query        <- get(".tcm_query",        envir = getNamespace("TCMNP"))
.sql_in           <- get(".sql_in",           envir = getNamespace("TCMNP"))
.tcm_herb_resolve <- get(".tcm_herb_resolve", envir = getNamespace("TCMNP"))

kmap_path <- function() {
  cands <- c(
    system.file("extdata", "herb_key_map.parquet", package = "TCMNP"),
    file.path("inst", "extdata", "herb_key_map.parquet"),
    file.path("..", "..", "inst", "extdata", "herb_key_map.parquet")
  )
  cands <- cands[nzchar(cands)]
  hit <- cands[file.exists(cands)]
  if (length(hit)) hit[[1]] else ""
}

# ⚠️ 缺数据必须**硬失败**，不能 silent skip：
#   2026-10-12 实测 —— 初版用 skip_if_not()，而测试跑的是**已安装副本**（早于本次导出），
#   于是 4 条断言全部被跳过，汇总仍显示「全绿」。这正是 MEMORY 铁律 28
#   「没有检查项 ≠ 通过」的复发路径（假通过比报错危险）。
need_km <- function() {
  p <- kmap_path()
  if (!nzchar(p)) {
    testthat::fail(paste0(
      "herb_key_map.parquet 未找到（先跑 data-raw/cleaning/06_export.py 导出实体层，",
      "并重装包：R CMD INSTALL TCMNP-main）"))
  }
  p
}

test_that("herb_key_map: 键 -> herb_id 必须是函数（无一对多）", {
  p <- need_km()
  km <- .tcm_query(sprintf(
    "SELECT herb_key, COUNT(DISTINCT herb_id) AS k FROM read_parquet('%s')
     GROUP BY herb_key HAVING k > 1", p))
  expect_equal(nrow(km), 0L)
})

test_that("resolve_herb 默认不启用，口径与旧版一致", {
  need_km()
  # 默认参数签名必须保留（不破坏既有调用）
  expect_true("resolve_herb" %in% names(formals(herb_target)))
  expect_identical(eval(formals(herb_target)$resolve_herb), FALSE)
  base <- herb_target("厚朴")
  expect_s3_class(base, "data.frame")
  expect_gt(nrow(base), 0)
})

test_that("resolve_herb 展开：键类型与查询列一致，且落到同一实体", {
  p <- need_km()
  # ① cn 口径：返回**中文键**，不混纯拼音（否则 WHERE Herb_cn_name 语义错位 + 误导噪音）
  k_cn <- .tcm_herb_resolve("厚朴", type = "Herb_cn_name")
  expect_true(length(k_cn) >= 1L)
  expect_false(any(grepl("^[a-z]+$", k_cn)))
  expect_true("厚朴" %in% k_cn)
  # ② pinyin 口径：houpo / houpu 被归一为**同一实体的两个键** ← 本改动的核心价值
  k_py <- .tcm_herb_resolve("houpo", type = "Herb_name_pin_yin")
  expect_true(all(c("houpo", "houpu") %in% k_py))
  expect_true(all(grepl("^[a-z]+$", k_py)))
  # ③ 展开后的键必须落到同一个 herb_id（实体归一成立）
  ids <- .tcm_query(sprintf(
    "SELECT DISTINCT herb_id FROM read_parquet('%s') WHERE herb_key IN (%s)",
    p, .sql_in(k_py)))$herb_id
  expect_equal(length(unique(ids)), 1L)
  # ④ 端到端：开启后行数**严格增加**（houpu 那 28,290 行被补进来）
  n_base <- nrow(herb_target("houpo", type = "Herb_name_pin_yin"))
  n_res <- nrow(herb_target("houpo", type = "Herb_name_pin_yin", resolve_herb = TRUE))
  expect_gt(n_base, 0)
  expect_gt(n_res, n_base)
})

test_that("herb_alias.csv: 键别名表与 herb_key_map 行数一致", {
  ka <- c(system.file("extdata", "herb_alias.csv", package = "TCMNP"),
          file.path("inst", "extdata", "herb_alias.csv"),
          file.path("..", "..", "inst", "extdata", "herb_alias.csv"))
  ka <- ka[nzchar(ka)]
  ka <- ka[file.exists(ka)]
  if (!length(ka)) testthat::fail("herb_alias.csv 未找到（先跑 06_export.py）")
  al <- utils::read.csv(ka[[1]], stringsAsFactors = FALSE)
  expect_true(all(c("herb_key", "key_type", "herb_id", "herb_cn",
                    "id_source", "is_canonical") %in% names(al)))
  km <- .tcm_query(sprintf(
    "SELECT COUNT(*) AS n FROM read_parquet('%s')", need_km()))
  expect_equal(nrow(al), km$n)
})
