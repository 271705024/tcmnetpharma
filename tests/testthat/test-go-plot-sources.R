# go_barplot / go_dotplot / go_lollipop 的「来源 vs 本体」语义
#
# 历史（2026-10-23）：这三个函数假设输入必有 ONTOLOGY 列（GO 的 BP/CC/MF），
# 用它做 group_by / fill / facet。KEGG / Reactome / WikiPathways 的富集结果
# **没有 ONTOLOGY 列**，直接喂会在 group_by(ONTOLOGY) 处报错。
# 当时的修法是 .tcm_inject_group_key()：把 source 回填进 ONTOLOGY，
# 「让下游逻辑完全不用改」。
#
# 该妥协已被证伪（2026-10-24 boss 实测）：图例出现 "Ontology: wikipathways"、
# 标题写 "GO enrichment of cluster"。省下的改动成本最终由用户支付。
#
# 现行契约（R/enrich-source.R）：
#   source       机器键 go/kegg/reactome/wikipathways
#   source_label 显示名 GO/KEGG/Reactome/WikiPathways
#   ontology     BP/CC/MF，**仅 GO 行有值**，其余来源一律 NA
#   .grp         分组键（由 .tcm_resolve_group 决定：单源 GO 用本体，多源用来源）
#
# 铁律 41/52：用「KEGG 无 ONTOLOGY」「GO+KEGG 混合」「跨来源撞名」三类真实
# 缺陷形态证伪，不用「能出图」当判据。

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

test_that("go_barplot 对无 ONTOLOGY 的 KEGG 数据可出图，且本体保持 NA", {
  df <- make_enrich_df(has_ontology = FALSE, src = "kegg")
  df$ONTOLOGY <- NULL                       # 模拟 KEGG enricher 直接产出
  p <- go_barplot(df, top = 5, style = "classic")
  expect_s3_class(p, "ggplot")
  # 来源被正确识别
  expect_true("source_label" %in% names(p$data))
  expect_setequal(unique(as.character(p$data$source_label)), "KEGG")
  # 关键：ontology 必须是 NA —— 不再被 source 回填（修复前这里是 "kegg"）
  expect_true(all(is.na(p$data$ontology)),
              info = "KEGG 行 ontology 必须为 NA（修复前被 source 回填成 'kegg'）")
  # 分组键退化为单一来源
  expect_setequal(unique(as.character(p$data$.grp)), "KEGG")
})

test_that("go_dotplot 对无 ONTOLOGY 的 KEGG 数据可出图，且本体保持 NA", {
  df <- make_enrich_df(has_ontology = FALSE, src = "kegg")
  df$ONTOLOGY <- NULL
  p <- go_dotplot(df, top = 5, style = "genratio")
  expect_s3_class(p, "ggplot")
  expect_setequal(unique(as.character(p$data$source_label)), "KEGG")
  expect_true(all(is.na(p$data$ontology)))
})

test_that("go_lollipop 对无 ONTOLOGY 的 KEGG 数据可出图（此前会崩）", {
  # 缺陷形态：go_lollipop 未接 .tcm_inject_group_key，直接 group_by(ONTOLOGY)
  # -> KEGG 数据必崩在 "Column `ONTOLOGY` is not found"
  df <- make_enrich_df(has_ontology = FALSE, src = "reactome", prefix = "RE")
  df$ONTOLOGY <- NULL
  expect_error(go_lollipop(df, top = 5), NA)
  p <- go_lollipop(df, top = 5)
  expect_s3_class(p, "ggplot")
  expect_setequal(unique(as.character(p$data$source_label)), "Reactome")
})

test_that("GO+KEGG 混合（ONTOLOGY 部分 NA）不丢非 GO 行", {
  go <- make_enrich_df(has_ontology = TRUE, src = "go", prefix = "GO")
  kegg <- make_enrich_df(has_ontology = FALSE, src = "kegg", prefix = "KEGG")
  mixed <- rbind(go, kegg)
  # 修复前：drop_na() 会把 KEGG 行（ONTOLOGY=NA）整批删掉 -> 只剩 GO
  p <- go_barplot(mixed, top = 5, style = "grouped")
  expect_s3_class(p, "ggplot")
  expect_true("KEGG" %in% as.character(p$data$source_label),
              info = "混合数据里 KEGG 来源应被保留（修复前被 drop_na 删光）")
  # GO 三本体仍在，且只有 GO 行有本体
  expect_true(all(c("BP", "CC", "MF") %in% as.character(p$data$ontology)))
  expect_true(all(is.na(p$data$ontology[p$data$source == "kegg"])))
})

test_that("纯 GO 数据（ONTOLOGY 完整）本体不被来源污染（向后兼容）", {
  go <- make_enrich_df(has_ontology = TRUE, src = "go")
  p <- go_barplot(go, top = 3, style = "grouped")
  expect_s3_class(p, "ggplot")
  expect_setequal(as.character(p$data$ontology), c("BP", "CC", "MF"))
  expect_false(any(as.character(p$data$ontology) == "go"))
})

# ---- Description 跨来源/本体撞名 ----
# 缺陷形态：GO 与 KEGG 各有一条同名 term，bind_rows 合并后 Description 重复，
# 而旧实现用 factor(Description, levels = 含重复的向量) -> "duplicated levels"。
# 现用「分组 + 控制字符 + 描述」做唯一 level 键，撞名时仍是两条独立条形。
test_that("Description 撞名（GO 与 KEGG 同名 term）不崩且两条都保留", {
  go <- make_enrich_df(has_ontology = TRUE, src = "go", prefix = "GO")
  kegg <- make_enrich_df(has_ontology = FALSE, src = "kegg", prefix = "KEGG")
  kegg$Description[1:3] <- go$Description[1:3]     # 人为撞名
  mixed <- rbind(go, kegg)

  expect_error(go_barplot(mixed, top = 5, style = "grouped"), NA)
  expect_error(go_dotplot(mixed, top = 5, style = "genratio"), NA)
  expect_error(go_lollipop(mixed, top = 5), NA)

  p <- go_barplot(mixed, top = 5, style = "classic")
  expect_s3_class(p, "ggplot")
  # 撞名的 term 仍是两个独立 y 位置（唯一 level 键含分组前缀）
  expect_equal(length(unique(as.character(p$data$.ykey))), nrow(p$data))
})

# ---- 标题：身份信息不来自字面量默认值（2026-10-24 核心修复）----
test_that("标题从数据派生，非 GO 数据不再写 GO", {
  kegg <- make_enrich_df(has_ontology = FALSE, src = "kegg")
  kegg$ONTOLOGY <- NULL
  p <- go_barplot(kegg, style = "classic")
  expect_identical(p$labels$title, "KEGG pathway enrichment")
  expect_false(grepl("GO", p$labels$title))

  reac <- make_enrich_df(has_ontology = FALSE, src = "reactome", prefix = "RE")
  reac$ONTOLOGY <- NULL
  expect_identical(go_barplot(reac)$labels$title, "Reactome pathway enrichment")
  expect_identical(go_dotplot(reac, style = "genratio")$labels$title,
                   "Reactome pathway enrichment")
  expect_identical(go_lollipop(reac)$labels$title, "Reactome pathway enrichment")
})

test_that("legacy_title 恢复旧字面量（兼容已存档脚本）", {
  go <- make_enrich_df(has_ontology = TRUE, src = "go")
  for (f in list(go_barplot, go_dotplot, go_lollipop)) {
    p <- f(go, legacy_title = TRUE)
    expect_identical(p$labels$title, "GO enrichment of cluster")
  }
})

test_that("GO 数据的派生标题带本体范围", {
  go <- make_enrich_df(has_ontology = TRUE, src = "go")
  expect_identical(go_barplot(go)$labels$title, "GO enrichment (BP / CC / MF)")
})

# ---- y 轴标签必须是条目名，不能漏出内部分组键（2026-10-24 实测 bug）----
# 缺陷形态：曾把「level -> 显示名」存成带 names 的向量放进 data.frame，
# 而 data.frame 会**剥离 names 属性** -> labels 函数查不到映射、全部走 fallback，
# 于是 y 轴显示 "BP\u0001term 1" 这种带控制字符的原始 key。
# 该 bug 不报错、图照出，只能靠断言抓。
test_that("y 轴标签是条目名，且不含内部分组键控制字符", {
  go <- make_enrich_df(has_ontology = TRUE, src = "go")
  for (nm in c("go_barplot", "go_dotplot", "go_lollipop")) {
    f <- get(nm)
    p <- f(go, top = 3)
    b <- ggplot2::ggplot_build(p)
    ylabs <- b$layout$panel_params[[1]]$y$get_labels()
    expect_false(any(grepl("\u0001", ylabs, fixed = TRUE)),
                 info = paste(nm, "的 y 轴漏出了分组键控制字符"))
    expect_true(any(grepl("term", ylabs)),
                info = paste(nm, "的 y 轴应显示真实条目名"))
  }
})

# ---- 不混排（2026-10-24 核心修复）----
test_that("go_barplot classic 不再把 BP/CC/MF 按数值混排", {
  # 造一个「Count 与本体强交错」的数据：Count 递增而本体循环，
  # 旧实现 order(Count, ONTOLOGY) 会得到大量本体切换。
  n <- 30L
  d <- data.frame(
    ID = paste0("T", seq_len(n)),
    Description = paste0("term ", seq_len(n)),
    GeneRatio = "5/50", BgRatio = "5/200",
    pvalue = seq(1e-8, 1e-3, length.out = n),
    p.adjust = seq(1e-8, 1e-3, length.out = n),
    qvalue = seq(1e-8, 1e-3, length.out = n),
    geneID = "TP53/BRCA1",
    Count = seq_len(n),
    source = "go",
    ONTOLOGY = rep(c("BP", "CC", "MF"), length.out = n),
    stringsAsFactors = FALSE
  )
  p <- go_barplot(d, top = 10, style = "classic")
  grp <- sub("\u0001.*$", "", levels(p$data$.ykey))
  if (length(grp) >= 2L) {
    changes <- sum(head(grp, -1) != tail(grp, -1))
    n_grp <- length(unique(grp))
    expect_equal(changes, n_grp - 1L,
                 info = "每组必须只出现一个连续块（修复前按 Count 全局排序会大量交错）")
  }
})
