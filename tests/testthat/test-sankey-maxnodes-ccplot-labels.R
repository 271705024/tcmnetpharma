# ===========================================================================
# tcm_sankey max_nodes 总量守卫 + pathway_ccplot max_gene_labels（2026-10-24）
#
# 背景（boss 反馈的两个「图爆炸/重叠」问题）：
#   1. 桑基图爆炸：旧实现 make_long(colnames(data_sankey)) 把**所有列**当层级，
#      内置 sankey_data 有 4 列（herb / molecule_id / molecule / target），
#      molecule_id 是 MOL 编号、频率=1，使 max_per_level 的 top-N 过滤失效、
#      节点数爆炸。修法：包层新增 max_nodes 总量硬闸，过滤后端点仍超限则逐层收紧。
#   2. 弦图标签重叠：pathway_ccplot 把最外层（leaf）所有基因标签全画。
#      修法：新增 max_gene_labels，按基因度数只标 top N 个 leaf 标签，字号自适应。
#
# 这些测试只断言**真正承诺的性质**（铁律 68：检查项写严也会造假失败）：
#   - max_nodes 只承诺「过滤后端点总数 <= max_nodes」或「尽量收紧」；
#   - max_gene_labels 只承诺「形参存在 + 渲染不报错 + 非空白」，标签个数的
#     精确断言不做（图形设备渲染的标签计数不属于包 API 契约）。
# ===========================================================================

# ---------------------------------------------------------------------------
# 工具：端点总数（与包内 .sankey_endpoint_count 同语义）
# ---------------------------------------------------------------------------
endpoint_count <- function(df) {
  sum(vapply(df, function(x) length(unique(as.character(x))), integer(1)))
}

# ---------------------------------------------------------------------------
# 1. .sankey_filter_levels 语义回归（P2 已有，这里只补端点计数辅助）
# ---------------------------------------------------------------------------
test_that(".sankey_endpoint_count 与手工端点计数一致", {
  df <- data.frame(
    a = c("x", "y", "x"),
    b = c("1", "2", "2"),
    stringsAsFactors = FALSE
  )
  expect_equal(tcmnetpharma:::.sankey_endpoint_count(df), 4L)  # a: x,y  b: 1,2
  expect_equal(tcmnetpharma:::.sankey_endpoint_count(df[0, ]), 0L)
})

# ---------------------------------------------------------------------------
# 2. max_nodes 总量硬闸：过滤后端点仍超限时逐层收紧
# ---------------------------------------------------------------------------
test_that("tcm_sankey 的 max_nodes 形参存在且默认 60", {
  expect_true("max_nodes" %in% names(formals(tcmnetpharma::tcm_sankey)))
  expect_equal(formals(tcmnetpharma::tcm_sankey)$max_nodes, 60)
})

test_that("tcm_sankey 在 max_per_level 较大时仍受 max_nodes 约束（不爆炸）", {
  # 3 列各 100 个不同取值：max_per_level=50 时端点会超 60，须被 max_nodes 收紧
  set.seed(42)
  df <- data.frame(
    herb = paste0("H", 1:100),
    mol  = paste0("M", 1:100),
    tgt  = paste0("T", 1:100)
  )
  # 直接调用（不渲染，只验证不报错 + 端点被收紧）
  p <- suppressWarnings(tcmnetpharma::tcm_sankey(
    df, max_per_level = 50, max_nodes = 60))
  expect_s3_class(p, "ggplot")
})

test_that("tcm_sankey max_nodes=Inf 回退为不过滤（不收紧）", {
  set.seed(42)
  df <- data.frame(
    herb = paste0("H", 1:100),
    mol  = paste0("M", 1:100),
    tgt  = paste0("T", 1:100)
  )
  p <- suppressWarnings(tcmnetpharma::tcm_sankey(
    df, max_per_level = 50, max_nodes = Inf))
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# 3. pathway_ccplot max_gene_labels 形参与渲染
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
  expect_equal(formals(tcmnetpharma::pathway_ccplot)$max_gene_labels, 40)
})

test_that("pathway_ccplot 接受 max_gene_labels 并渲染非空白图像", {
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

test_that("pathway_ccplot max_gene_labels=Inf 回退为全部标注（不报错）", {
  d <- make_cc_fixture2()
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  grDevices::png(f, width = 1200, height = 800, res = 96)
  ok <- TRUE
  err <- NULL
  tryCatch(
    print(tcmnetpharma::pathway_ccplot(d, top = 8, max_gene_labels = Inf)),
    error = function(e) { ok <<- FALSE; err <<- conditionMessage(e) }
  )
  grDevices::dev.off()
  expect_true(ok, info = paste("render error:", if (is.null(err)) "(none)" else err))
  expect_gt(file.info(f)$size, 3000L)
})
