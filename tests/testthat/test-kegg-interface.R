# KEGG 接口回归测试（2026-10-27）
#
# 背景：`tcm_enrich(ont = "KEGG")` 上线以来**从未跑通过** —— 缓存与快照构建都正确，
# 崩在最后一步：`clusterProfiler::enricher(TERM2NAME = <命名向量>)` 在
# clusterProfiler 4.20.0 / DOSE 4.5.0 下按二维对象取列，报
# `attempt to set 'colnames' on an object with less than two dimensions`。
#
# 实测对照（20 个基因，本地缓存 hsa_pathway.rds）：
#   TERM2GENE only                    -> OK  185 行
#   TERM2GENE + TERM2NAME(命名向量)    -> ERR colnames
#   TERM2GENE + TERM2NAME(两列 df)     -> OK  185 行
#
# 本测试**不联网**：只验入参归一函数，联网路径由手动探针覆盖（见
# logs/_probe_kegg_iface.R）。这样 R CMD check 不依赖 rest.kegg.jp 可达性。

test_that("TERM2NAME 归一：命名向量 -> 两列 data.frame", {
  t2n <- c(hsa00010 = "Glycolysis", hsa00020 = "TCA cycle")
  df <- tcmnetpharma:::.tcm_kegg_t2n_df(t2n)

  expect_s3_class(df, "data.frame")
  expect_identical(dim(df), c(2L, 2L))
  expect_identical(names(df), c("term", "name"))
  expect_identical(df$term, c("hsa00010", "hsa00020"))
  expect_identical(df$name, c("Glycolysis", "TCA cycle"))
  # 名称不能残留进 name 列（否则通路名会带 term 前缀，图上显示错）
  expect_false(any(grepl("hsa", df$name)))
})

test_that("TERM2NAME 归一：已是两列 data.frame 时原样保留前两列", {
  df0 <- data.frame(term = "hsa00010", name = "Glycolysis",
                    extra = "x", stringsAsFactors = FALSE)
  df <- tcmnetpharma:::.tcm_kegg_t2n_df(df0)
  expect_identical(names(df), c("term", "name"))
  expect_identical(nrow(df), 1L)
})

test_that("TERM2NAME 归一：NULL 透传，单列 df 报错（不静默放行）", {
  expect_null(tcmnetpharma:::.tcm_kegg_t2n_df(NULL))
  bad <- data.frame(term = "hsa00010", stringsAsFactors = FALSE)
  expect_error(tcmnetpharma:::.tcm_kegg_t2n_df(bad), "2 columns")
})

test_that("归一后的 TERM2NAME 能真正喂进 enricher（离线，合成 TERM2GENE）", {
  skip_if_not_installed("clusterProfiler")
  # 设计要点（踩过三次坑，别再简化）：
  #   ① 必须**真有富集信号**，否则 enricher 返回 NULL -> 断言变成「没报错」的假通过；
  #   ② enricher 默认 minGSSize = 10，太小的合成通路会被静默过滤；
  #   ③ qvalue 计算需要足够多的 p 值，通路太少会报
  #      「missing values and NaN's not allowed」。
  # 故：1 个 5 基因的命中通路 + 10 个背景通路（universe ≈ 105），放开 GSSize 下界。
  hit_genes <- 1:5
  bg_terms <- lapply(0:9, function(i) 101L + i * 10L + (0:9))
  t2g <- do.call(rbind, c(
    list(data.frame(term = "hsa00001", gene = as.character(hit_genes),
                    stringsAsFactors = FALSE)),
    lapply(seq_along(bg_terms), function(i) {
      data.frame(term = sprintf("hsa%05d", i + 1L),
                 gene = as.character(bg_terms[[i]]), stringsAsFactors = FALSE)
    })
  ))
  t2n <- stats::setNames(c("synthetic pathway A", sprintf("background pathway %02d", 1:10)),
                         unique(t2g$term))

  # minGSSize 必须放开：enricher 默认 10，会把 5 个基因的合成命中通路静默过滤。
  # qvalue 在「多数 p = 1」的合成数据上会告警（qvalue::qvalue 返回 NA）——
  # 本测试验的是 TERM2NAME 管线，不是 q 值估计，故只静音该告警。
  er <- suppressWarnings(clusterProfiler::enricher(
    gene = as.character(1:20),
    TERM2GENE = t2g,
    TERM2NAME = tcmnetpharma:::.tcm_kegg_t2n_df(t2n),
    pvalueCutoff = 1, qvalueCutoff = 1,
    minGSSize = 2, maxGSSize = 5000
  ))

  expect_s4_class(er, "enrichResult")
  res <- as.data.frame(er)
  expect_true(nrow(res) >= 1L)
  # 通路名来自 t2n —— 证明 TERM2NAME 真生效（不是被忽略后回退成 hsa00001）
  expect_true(any(grepl("synthetic pathway", res$Description)))
})
