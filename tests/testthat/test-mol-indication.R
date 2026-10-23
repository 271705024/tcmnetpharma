# ===========================================================================
# ChEMBL 37 消费层契约：mol_indication() / mol_moa()（2026-10-21）
#
# 背景：`tcm_mol_indication`（60,055 行）与 `tcm_target_moa`（7,561 行）接入后，
#   若 R 侧零引用 = 没接（MEMORY 铁律 66）。本文件锁消费层三件事：
#   ① 列契约固定（返回列名/顺序稳定，空契约列齐全）；
#   ② 匹配语义：name 默认**精确**（大小写不敏感），只有 contains 才子串；
#      ID 类（chembl_id / inchikey）**精确不子串**，避免假阳性；
#   ③ 过滤器真生效：min_phase 单调收紧、natural_product / human_only 有界。
#
# 遵循铁律 68：只断言**真正承诺的性质**（语义/契约），不拿偶然事实当契约
#   （不断言「某药必有 N 行」这类随数据版本漂移的数字）。
# ===========================================================================

.tcm_query        <- get(".tcm_query",        envir = getNamespace("tcmnetpharma"))
.tcm_parquet_path <- get(".tcm_parquet_path", envir = getNamespace("tcmnetpharma"))

ind_path <- function() {
  p <- system.file("extdata", "tcm_mol_indication.parquet", package = "tcmnetpharma")
  if (!nzchar(p)) p <- file.path("inst", "extdata", "tcm_mol_indication.parquet")
  if (!file.exists(p)) {
    # 缺数据硬失败，不静默 skip（铁律 28：没有检查项 ≠ 通过）
    stop("tcm_mol_indication.parquet 未导出（先跑 data-raw/cleaning/06_export.py）")
  }
  p
}

moa_path <- function() {
  p <- system.file("extdata", "tcm_target_moa.parquet", package = "tcmnetpharma")
  if (!nzchar(p)) p <- file.path("inst", "extdata", "tcm_target_moa.parquet")
  if (!file.exists(p)) stop("tcm_target_moa.parquet 未导出")
  p
}

IND_COLS <- c("mol_chembl_id", "mol_name", "inchikey", "natural_product",
              "mol_max_phase", "max_phase_for_ind", "mesh_id", "mesh_heading",
              "efo_id", "efo_term")
MOA_COLS <- c("mol_chembl_id", "mol_name", "inchikey", "max_phase",
              "mechanism_of_action", "action_type", "target_name", "target_type",
              "gene_symbol", "uniprot", "organism", "is_human")

test_that("mol_indication 列契约固定，三种入口一致", {
  r <- mol_indication("Aspirin")
  expect_named(r, IND_COLS)
  expect_gt(nrow(r), 0L)

  # 同一药物三种入口应命中同一实体（行数一致；不断言绝对行数——随数据版本漂移）
  n_name <- nrow(r)
  n_id   <- nrow(mol_indication("CHEMBL25", type = "mol_chembl_id"))
  n_ik   <- nrow(mol_indication("BSYNRYMUTXBXSQ-UHFFFAOYSA-N", type = "inchikey"))
  expect_equal(n_id, n_name)
  expect_equal(n_ik, n_name)
  # InChIKey 是跨库主键，命中行都应带上它
  expect_true(all(nzchar(r$inchikey)))
})

test_that("min_phase 单调收紧（阈值越高行数越少，且行内 phase 达标）", {
  base <- mol_indication("Aspirin")
  p2   <- mol_indication("Aspirin", min_phase = 2)
  p4   <- mol_indication("Aspirin", min_phase = 4)
  expect_lte(nrow(p2), nrow(base))
  expect_lte(nrow(p4), nrow(p2))
  # 过滤真生效：返回行的 max_phase_for_ind 全部 >= 阈值
  if (nrow(p2)) expect_true(all(p2$max_phase_for_ind >= 2))
  if (nrow(p4)) expect_true(all(p4$max_phase_for_ind >= 4))
})

test_that("natural_product 过滤有界（TRUE 子集行均标志=1）", {
  np <- mol_indication("Aspirin", natural_product = TRUE)
  if (nrow(np)) expect_true(all(np$natural_product == 1L | np$natural_product == 1))
  # FALSE 排除天然产物：返回行不得有 natural_product=1
  non <- mol_indication("Aspirin", natural_product = FALSE)
  if (nrow(non)) expect_false(any(non$natural_product == 1L | non$natural_product == 1))
})

test_that("name 默认精确（大小写不敏感），只有 contains 才子串", {
  # 大小写不敏感：库内 'ASPIRIN'，查 'aspirin' 应命中
  expect_gt(nrow(mol_indication("aspirin")), 0L)
  exact <- mol_indication("aspirin")
  loose <- mol_indication("aspirin", match = "contains")
  # contains 范围不小于精确（可等，不得更小）
  expect_gte(nrow(loose), nrow(exact))
})

test_that("ID 类入口精确不子串（inchikey 前缀不得命中）", {
  # 真实 InChIKey 能命中
  expect_gt(nrow(mol_indication("BSYNRYMUTXBXSQ-UHFFFAOYSA-N", type = "inchikey")), 0L)
  # 截断的前缀（子串）不得命中 —— ID 精确匹配，避免假阳性
  expect_equal(nrow(mol_indication("BSYNRYMUTXBXSQ", type = "inchikey")), 0L)
  # 非法 ID（含正则元字符）应被拒绝返回 0 行，而不是变成宽松匹配
  expect_equal(nrow(mol_indication("CHEMBL25%DROP", type = "mol_chembl_id")), 0L)
})

test_that("未命中返回 0 行且列契约不变", {
  e <- mol_indication("NoSuchDrugXYZ")
  expect_equal(nrow(e), 0L)
  expect_named(e, IND_COLS)
})

test_that("mol_moa 列契约固定，human_only 收紧", {
  r <- mol_moa("Aspirin")
  expect_named(r, MOA_COLS)
  expect_gt(nrow(r), 0L)

  allsp <- mol_moa("Aspirin", human_only = FALSE)
  expect_gte(nrow(allsp), nrow(r))
  # human_only=TRUE 时返回行 is_human 必为 TRUE
  expect_true(all(r$is_human))
  # 未命中列契约不变
  e <- mol_moa("NoSuchDrugXYZ")
  expect_equal(nrow(e), 0L)
  expect_named(e, MOA_COLS)
})

test_that("消费层数据表自洽（行数/唯一性守卫，铁律 55）", {
  agg <- .tcm_query(sprintf("
    SELECT count(*) AS n, count(DISTINCT molregno) AS n_mol,
      sum(CASE WHEN inchikey IS NOT NULL AND inchikey <> '' THEN 1 ELSE 0 END) AS n_ik,
      sum(CASE WHEN max_phase_for_ind >= 4 THEN 1 ELSE 0 END) AS n_phase4
    FROM read_parquet('%s')", ind_path()))
  expect_equal(agg$n, 60055L)     # drug_indication 全量条数守卫
  expect_gt(agg$n_mol, 0L)
  expect_gt(agg$n_ik, 0L)         # InChIKey 非空（跨库对齐主通道）
  expect_gt(agg$n_phase4, 0L)     # 有已上市适应症

  # mol_moa 表行数守卫
  m <- .tcm_query(sprintf("SELECT count(*) AS n FROM read_parquet('%s')", moa_path()))
  expect_equal(m$n, 7561L)        # drug_mechanism 全量条数守卫
})
