# 交集靶点 tcm_intersect_target() 契约测试
#
# 定位：这是网络药理学的**主线第一步**（药靶 ∩ 病靶），下游富集/PPI/拓扑都吃它的输出。
# 所以断言不能只查"能跑"，要查：
#   ① 列契约稳定（空结果也必须同列，否则下游 data.frame 拼接会崩）
#   ② **真的是交集**（任一行的 target_symbol 必须同时存在于两侧）
#   ③ 聚合语义（同一靶点多分子 -> n_molecule 计数；多 OT 疾病 -> 取 max 不取 sum）
#   ④ 排序契约（disease_score 降序）

test_that("tcm_intersect_target: 列契约与基本形态", {
  hit <- suppressWarnings(tcm_intersect_target("\u9ebb\u9ec4", "Asthma"))  # 麻黄
  expect_true(is.data.frame(hit))
  expect_named(hit, c("target_symbol", "herb", "n_molecule", "disease_score",
                      "evidence_count", "n_ot_disease", "target_entrez",
                      "target_ensembl"))
  expect_gt(nrow(hit), 100L)
  expect_false(anyDuplicated(hit$target_symbol) > 0L)  # 每个靶点一行
  expect_true(all(nzchar(hit$target_symbol)))
  expect_true(all(is.na(hit$disease_score) | (hit$disease_score >= 0 & hit$disease_score <= 1)))
})

test_that("tcm_intersect_target: 结果确实是「药物靶点 ∩ 疾病靶点」", {
  hit <- suppressWarnings(tcm_intersect_target("\u9ebb\u9ec4", "Asthma"))
  ht <- herb_target("\u9ebb\u9ec4")
  dt <- disease_target("Asthma")

  herb_set <- unique(as.character(ht$target))
  dis_set  <- unique(as.character(dt$target_symbol))
  expect_true(all(hit$target_symbol %in% herb_set))
  expect_true(all(hit$target_symbol %in% dis_set))
  # 必须是真交集：等于两侧集合的交集（不多不少）
  expect_setequal(hit$target_symbol, intersect(herb_set, dis_set))
  # 且确实小于任一单侧（防"其实只取了单边"）
  expect_lt(nrow(hit), length(herb_set))
  expect_lt(nrow(hit), length(dis_set))
})

test_that("tcm_intersect_target: 内容锚点（生物学事实，非偶然）", {
  hit <- suppressWarnings(tcm_intersect_target("\u9ebb\u9ec4", "Asthma"))
  # ADORA1（腺苷 A1 受体）是哮喘气道炎症的既有靶点，且麻黄类生物碱可作用其上
  expect_true("ADORA1" %in% hit$target_symbol)
  # 排序契约：disease_score 非增
  sc <- hit$disease_score
  expect_true(all(diff(sc) <= 1e-12))
})

test_that("tcm_intersect_target: 多味药 -> herb 列用分号聚合", {
  hit <- suppressWarnings(tcm_intersect_target(c("\u9ebb\u9ec4", "\u6842\u679d"), "Asthma"))
  expect_true(any(grepl(";", hit$herb, fixed = TRUE)))
  # 多味药的靶点集应是单味药的超集（并集后再交集）
  one <- suppressWarnings(tcm_intersect_target("\u9ebb\u9ec4", "Asthma"))
  expect_gte(nrow(hit), nrow(one))
})

test_that("tcm_intersect_target: 空结果仍返回同结构 0 行（下游不崩）", {
  ref <- suppressWarnings(tcm_intersect_target("\u9ebb\u9ec4", "Asthma"))
  e1 <- suppressWarnings(tcm_intersect_target("\u8fd9\u4e0d\u662f\u836f", "Asthma"))
  e2 <- suppressWarnings(tcm_intersect_target("\u9ebb\u9ec4", "no-such-disease-XYZ"))
  for (e in list(e1, e2)) {
    expect_true(is.data.frame(e))
    expect_equal(nrow(e), 0L)
    expect_named(e, names(ref))
  }
})

test_that("tcm_intersect_target: min_score 过滤是单调的", {
  # 注意 min_score 作用在病侧；ADORA1 的 score 约 0.73
  hi <- suppressWarnings(tcm_intersect_target("\u9ebb\u9ec4", "Asthma", min_score = 0.6))
  lo <- suppressWarnings(tcm_intersect_target("\u9ebb\u9ec4", "Asthma", min_score = 0))
  expect_lte(nrow(hi), nrow(lo))
  expect_true(all(hi$disease_score >= 0.6))
})

test_that("tcm_enrich: progress 回调按阶段上报且不改变结果", {
  genes <- c("TP53", "BRCA1", "EGFR", "KRAS", "PTEN")
  seen <- numeric(0)
  r1 <- tcm_enrich(genes, keytype = "SYMBOL", species = "human",
                   pvalueCutoff = 1,
                   progress = function(p, m) seen <<- c(seen, p))
  # 至少上报 3 个阶段，且单调不减、末值 100
  expect_gte(length(seen), 3L)
  expect_true(all(diff(seen) >= 0))
  expect_equal(tail(seen, 1), 100)

  # 回调不改变统计结果（同样的输入，带/不带回调应一致）
  r2 <- tcm_enrich(genes, keytype = "SYMBOL", species = "human", pvalueCutoff = 1)
  expect_equal(nrow(r1$go@result), nrow(r2$go@result))
  expect_equal(sum(r1$go@result$pvalue), sum(r2$go@result$pvalue), tolerance = 1e-12)

  # 回调抛错不得影响主流程（GUI 里 async_progress 可能因窗口关闭而失败）
  r3 <- tcm_enrich(genes, keytype = "SYMBOL", species = "human", pvalueCutoff = 1,
                   progress = function(p, m) stop("callback boom"))
  expect_s4_class(r3$go, "enrichResult")
})
