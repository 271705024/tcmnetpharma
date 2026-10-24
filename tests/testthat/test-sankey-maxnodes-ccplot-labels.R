# ===========================================================================
# tcm_sankey max_nodes 总量守卫 + pathway_ccplot max_gene_labels（2026-10-24）
#
# 背景（boss 反馈的两个「图爆炸/重叠」问题）：
#   1. 桑基图爆炸：旧实现 make_long(colnames(data_sankey)) 把**所有列**当层级，
#      内置 sankey_data 有 4 列（herb / molecule_id / molecule / target），
#      molecule_id 是 MOL 编号、频率=1，使 max_per_level 的 top-N 过滤失效、
#      节点数爆炸。修法：包层新增 max_nodes 总量硬闸，过滤后端点仍超限则逐层
#      收紧（strict 硬截）。
#   2. 弦图标签重叠：pathway_ccplot 把最外层（leaf）所有基因标签全画。
#      修法：新增 max_gene_labels，按基因度数只标 top N 个 leaf 标签。
#
# ⚠️ 断言写法教训（2026-10-24 实测踩到，铁律 28）：
#   最初这几条只断言「传了参数不报错 / 能出图」——**旧版本同样通过**，因为
#   未声明的参数会被 `...` 吞掉，图照常画出来。这是**假通过**，比报错危险。
#   现改为**行为证伪**：
#     · tcm_sankey：直接数 ggplot 数据里的 node 唯一值。max_nodes=60 必须
#       <= 60，且严格小于 max_nodes=Inf 的数量（证明参数真被消费）。
#     · pathway_ccplot：从 leaf 标签图层的 filter quosure 环境里读 label_genes
#       长度，必须随 max_gene_labels 变化（2 -> 2 个、Inf -> 0 表示全标）。
# ===========================================================================

# ---------------------------------------------------------------------------
# 工具
# ---------------------------------------------------------------------------
endpoint_count <- function(df) {
  sum(vapply(df, function(x) length(unique(as.character(x))), integer(1)))
}

# 桑基 ggplot 里的节点数（node 列唯一值）
sankey_nodes <- function(p) length(unique(p$data$node))

# pathway_ccplot：读 leaf 标签层 filter 环境里的 label_genes 长度
#   NA  = 找不到该图层（结构变了）
#   0   = 全部标注（max_gene_labels = Inf 的标记）
#   n>0 = 只标 n 个基因
cc_label_genes_n <- function(p) {
  q <- NULL
  for (l in p$layers) {
    f <- l$mapping$filter
    if (!is.null(f) &&
        grepl("label_genes", paste(deparse(f), collapse = ""), fixed = TRUE)) {
      q <- f
      break
    }
  }
  if (is.null(q)) return(NA_integer_)
  env <- tryCatch(rlang::quo_get_env(q), error = function(e) NULL)
  if (is.null(env)) return(NA_integer_)
  v <- tryCatch(get("label_genes", envir = env), error = function(e) NULL)
  if (is.null(v)) NA_integer_ else length(v)
}

# ---------------------------------------------------------------------------
# 1. 端点计数 / 分层过滤（含 strict 硬截）
# ---------------------------------------------------------------------------
test_that(".sankey_endpoint_count 与手工端点计数一致", {
  df <- data.frame(a = c("x", "y", "x"), b = c("1", "2", "2"),
                   stringsAsFactors = FALSE)
  expect_equal(tcmnetpharma:::.sankey_endpoint_count(df), 4L)   # a: x,y  b: 1,2
  expect_equal(tcmnetpharma:::.sankey_endpoint_count(df[0, ]), 0L)
})

test_that(".sankey_filter_levels 宽松模式保留并列项（不随机丢弃）", {
  # 每个值频率都是 1；top_n = 1 时宽松模式仍保留全部 3 个
  df <- data.frame(a = c("x", "y", "z"), stringsAsFactors = FALSE)
  out <- tcmnetpharma:::.sankey_filter_levels(df, 1L)
  expect_equal(length(unique(out$a)), 3L)
})

test_that(".sankey_filter_levels strict 模式按位置硬截（全并列也能收）", {
  # 全并列（每值频率 1）是 MOL 编号列的典型形态 —— 宽松模式在这里完全失效，
  # strict 必须真的把值个数压到 top_n。
  df <- data.frame(a = c("x", "y", "z"), stringsAsFactors = FALSE)
  out <- tcmnetpharma:::.sankey_filter_levels(df, 1L, strict = TRUE)
  expect_equal(length(unique(out$a)), 1L)
  out2 <- tcmnetpharma:::.sankey_filter_levels(df, 2L, strict = TRUE)
  expect_equal(length(unique(out2$a)), 2L)
})

# ---------------------------------------------------------------------------
# 2. max_nodes 总量硬闸（行为证伪）
# ---------------------------------------------------------------------------
test_that("tcm_sankey 的 max_nodes 形参存在且默认 60", {
  expect_true("max_nodes" %in% names(formals(tcmnetpharma::tcm_sankey)))
  expect_equal(as.numeric(formals(tcmnetpharma::tcm_sankey)$max_nodes), 60)
})

test_that("tcm_sankey max_nodes 真收紧（不是被 ... 吞掉）", {
  # 3 列各 100 唯一值 = 300 端点。max_nodes=60 必须真收下来，Inf 必须保持 300。
  set.seed(42)
  df <- data.frame(herb = paste0("H", 1:100), mol = paste0("M", 1:100),
                   tgt = paste0("T", 1:100), stringsAsFactors = FALSE)
  p60  <- suppressWarnings(tcmnetpharma::tcm_sankey(
    df, max_per_level = 50, max_nodes = 60))
  pInf <- suppressWarnings(tcmnetpharma::tcm_sankey(
    df, max_per_level = 50, max_nodes = Inf))
  expect_s3_class(p60, "ggplot")
  expect_lte(sankey_nodes(p60), 60L)
  expect_equal(sankey_nodes(pInf), 300L)
  # 参数真被消费的铁证：两种设置的结果必须不同
  expect_lt(sankey_nodes(p60), sankey_nodes(pInf))
})

# ---------------------------------------------------------------------------
# 3. pathway_ccplot max_gene_labels（行为证伪）
# ---------------------------------------------------------------------------
make_cc_fixture2 <- function() {
  set.seed(7)
  n <- 10L
  data.frame(
    ID          = sprintf("hsa%05d", 1:n),
    Description = sprintf("synthetic pathway %02d", 1:n),
    geneID      = vapply(seq_len(n), function(i) {
      paste(sprintf("G%03d", (i %% 5) + c(0, 1, 2)), collapse = "/")
    }, character(1)),
    p.adjust    = round(stats::runif(n, 1e-5, 0.2), 6),
    stringsAsFactors = FALSE
  )
}

test_that("pathway_ccplot 的 max_gene_labels 形参存在且默认 40", {
  expect_true("max_gene_labels" %in% names(formals(tcmnetpharma::pathway_ccplot)))
  expect_equal(as.numeric(formals(tcmnetpharma::pathway_ccplot)$max_gene_labels), 40)
})

test_that("pathway_ccplot max_gene_labels 真控制标注数（不是被 ... 吞掉）", {
  d <- make_cc_fixture2()
  p2   <- tcmnetpharma::pathway_ccplot(d, top = 8, max_gene_labels = 2)
  p4   <- tcmnetpharma::pathway_ccplot(d, top = 8, max_gene_labels = 4)
  pInf <- tcmnetpharma::pathway_ccplot(d, top = 8, max_gene_labels = Inf)
  expect_equal(cc_label_genes_n(p2), 2L)
  expect_equal(cc_label_genes_n(p4), 4L)
  expect_equal(cc_label_genes_n(pInf), 0L)   # 0 = 不截，全部标注
})

test_that("pathway_ccplot 带 max_gene_labels 渲染非空白", {
  d <- make_cc_fixture2()
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  grDevices::png(f, width = 1200, height = 800, res = 96)
  ok <- TRUE
  err <- NULL
  tryCatch(
    print(tcmnetpharma::pathway_ccplot(d, top = 8, max_gene_labels = 20)),
    error = function(e) { ok <<- FALSE; err <<- conditionMessage(e) }
  )
  grDevices::dev.off()
  expect_true(ok, info = paste("render error:", if (is.null(err)) "(none)" else err))
  expect_gt(file.info(f)$size, 3000L)
})
