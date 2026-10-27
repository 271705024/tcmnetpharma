# 富集结果合成夹具（2026-10-27 从 test-smoke-all-exports.R 提为共享 helper）
#
# 为什么提出来：原先多处测试直接依赖 `kegg.filter` / `KK` 两个数据对象，
# 而这两个对象是 KEGG 富集结果，随 KEGG 数据一并移除（不可再分发）。
# 富集图型测试只需「同形的富集表」，不需要真实通路内容 -> 统一用合成夹具。
# testthat 会自动 source 本目录下 helper*.R，故所有测试文件均可直接调用。

# 33 行的合成富集结果（GO 形制），列集覆盖：
#   ID, Description, GeneRatio, BgRatio, pvalue, p.adjust, qvalue,
#   geneID（"/" 分隔）, Count, ONTOLOGY（BP/CC/MF 三值）, richFactor
# 覆盖函数：go_barplot/go_cir/go_dotplot/go_lollipop（需 ONTOLOGY）、
#           dock_plot（需 ID + pvalue 数值列）、pathway_cirplot。
make_enrich_fixture <- function() {
  set.seed(42)
  n_per <- 11L                                  # 11 × 3 = 33 行
  onto <- rep(c("BP", "CC", "MF"), each = n_per)
  n <- length(onto)
  genes <- sprintf("GENE%03d", seq_len(30))
  gene_id <- vapply(seq_len(n), function(i) {
    gr <- genes[((i * 2 - 1) %% 29) + c(0, 1, 2)]
    paste(gr, collapse = "/")
  }, character(1))
  data.frame(
    ONTOLOGY    = onto,
    ID          = sprintf("GO:%07d", 1:n),
    Description = sprintf("synthetic enrichment term number %02d", 1:n),
    GeneRatio   = paste0(3, "/", 20 + (1:n %% 15)),
    BgRatio     = paste0(50 + (1:n %% 30), "/", 2000 + (1:n %% 100)),
    pvalue      = round(stats::runif(n, 1e-6, 0.05), 6),
    p.adjust    = round(stats::runif(n, 1e-5, 0.2), 6),
    qvalue      = round(stats::runif(n, 1e-5, 0.25), 6),
    geneID      = gene_id,
    Count       = rep(c(3L, 4L, 5L, 6L), length.out = n),
    stringsAsFactors = FALSE
  )
}

# 带 ONTOLOGY 的 GO 结果（make_enrich_fixture 已含 ONTOLOGY，语义上等价）
make_go_fixture <- function() {
  make_enrich_fixture()
}
