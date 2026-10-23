# go_barplot / go_dotplot 对「无 ONTOLOGY / 部分 ONTOLOGY 缺失」数据的退化
# （2026-10-23，boss 反馈「KEGG 跑通却画不了图」的包层修复）
#
# 背景：这两个函数的历史实现假设输入必有 ONTOLOGY 列（GO 的 BP/CC/MF），
# 用它做 group_by / fill / facet。但 KEGG / Reactome / WikiPathways 的富集
# 结果没有 ONTOLOGY（只有 source 列），直接喂会在 group_by(ONTOLOGY) 处报
# "Column `ONTOLOGY` is not found"。修复方式：.tcm_inject_group_key 在 ONTOLOGY
# 缺失（或用 bind_rows 合并后部分行为 NA）时用 source 回填，退化为「按来源
# 单色 / 单面」，条目与统计口径不变（设计 doc §2）。
#
# 铁律 41/52：用「KEGG 无 ONTOLOGY」与「GO+KEGG 混合」两类真实缺陷形态证伪。

make_enrich_df <- function(has_ontology, n = 12L, src = "kegg", prefix = "id") {
  d <- data.frame(
    ID = paste0(prefix, seq_len(n)),
    Description = paste0(prefix, " term ", seq_len(n)),
    GeneRatio = paste0(seq_len(n), "/50"),
    BgRatio = paste0(seq_len(n), "/200"),
    pvalue = runif(n, 1e-6, 0.05),
    p.adjust = runif(n, 1e-4, 0.05),
    qvalue = runif(n, 1e-4, 0.05),
    geneID = "TP53/BRCA1/EGFR",
    Count = sample(3:20, n, replace = TRUE),
    source = src,
    stringsAsFactors = FALSE
  )
  if (has_ontology) {
    d$ONTOLOGY <- rep(c("BP", "CC", "MF"), length.out = n)
  } else {
    d$ONTOLOGY <- rep(NA_character_, n)
  }
  d
}

test_that("go_barplot 对无 ONTOLOGY 的 KEGG 数据可出图（退化单色）", {
  df <- make_enrich_df(has_ontology = FALSE, src = "kegg")
  # 无 ONTOLOGY 列（模拟 KEGG enricher 直接产出）
  df$ONTOLOGY <- NULL
  p <- go_barplot(df, top = 5, style = "classic")
  expect_s3_class(p, "ggplot")
  # 注入的 ONTOLOGY = source（单值 kegg），fill 用单色，仍出图
  expect_true("ONTOLOGY" %in% names(p$data))
  expect_setequal(as.character(p$data$ONTOLOGY), "kegg")
})

test_that("go_dotplot 对无 ONTOLOGY 的 KEGG 数据可出图（退化不分面）", {
  df <- make_enrich_df(has_ontology = FALSE, src = "kegg")
  df$ONTOLOGY <- NULL
  p <- go_dotplot(df, top = 5, style = "genratio")
  expect_s3_class(p, "ggplot")
  expect_true("ONTOLOGY" %in% names(p$data))
})

test_that("GO+KEGG 混合（ONTOLOGY 部分 NA）不丢非 GO 行", {
  # 模拟 flatten_enrich_to_df(bind_rows) 的产物：GO 行有 ONTOLOGY，KEGG 行 NA
  go <- make_enrich_df(has_ontology = TRUE, src = "go", prefix = "GO")
  kegg <- make_enrich_df(has_ontology = FALSE, src = "kegg", prefix = "KEGG")
  mixed <- rbind(go, kegg)
  # 修复前：drop_na() 会把 KEGG 行（ONTOLOGY=NA）整批删掉 -> 只剩 GO
  p <- go_barplot(mixed, top = 5, style = "grouped")
  expect_s3_class(p, "ggplot")
  ont <- as.character(p$data$ONTOLOGY)
  # kegg 来源必须保留（修复前会丢）
  expect_true("kegg" %in% ont,
              info = "混合数据里 kegg 来源应被保留（修复前被 drop_na 删光）")
  # GO 三本体也保留
  expect_true(all(c("BP", "CC", "MF") %in% ont))
})

test_that("纯 GO 数据（ONTOLOGY 完整）行为不变（向后兼容）", {
  go <- make_enrich_df(has_ontology = TRUE, src = "go")
  p <- go_barplot(go, top = 3, style = "grouped")
  expect_s3_class(p, "ggplot")
  # GO 数据的 ONTOLOGY 保持 BP/CC/MF 三本体，不被 source 覆盖
  expect_setequal(as.character(p$data$ONTOLOGY), c("BP", "CC", "MF"))
})
