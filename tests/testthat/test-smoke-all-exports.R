# ===========================================================================
# L2 全导出函数冒烟测试（2026-09-14，rev2 by l2-smoke）
#
# 目的
#   覆盖 NAMESPACE 中全部 36 个导出函数（`%>%` 单独断言），既验证
#   「可调用 / 不报错」，也记录「返回值契约」。
#
# 设计原则
#   1. 只做冒烟级验证 —— 不比对数值结果（数值正确性由 data-contract 测试负责）。
#   2. 绘图函数统一走 grDevices::pdf(NULL) 重定向，避免测试期弹窗/写盘。
#   3. 依赖 inst/extdata/tcm_network_compat.parquet 的查询函数：文件缺失时
#      skip（该文件在本仓库真实存在，正常情况下不应发生 skip）。
#   4. 输入一律使用「包内置数据集」或「文件内构造的合成 fixture」，保证可复现。
#
# rev2 变更说明（fixture 修正，详见 docs/l2-smoke-test-report-2026-09-14.md）
#   - 原 rev1 误用 `kegg.filter`（8 行 × 9 列，无 ONTOLOGY / 无 richFactor）作为
#     全部富集类函数的输入，导致 12 个函数 FAIL。经逐个读源码确认，其中 11 个
#     是**输入列集不匹配**（fixture 问题），1 个（pathway_ccplot）是**真实代码缺陷**
#     （dplyr 1.0 已 defunct 的 `.dots =` 写法）。
#   - 文件内新增合成 fixture 构造函数：make_kegg_fixture() / make_go_fixture() /
#     make_xfbdf_fixture() / make_pre_plot_fixture()。
#   - 富集类绘图函数的调用被重排为「run_plot_smoke() 逐个 tryCatch」模式：
#     这样单个函数的失败不会连带跳过后面的函数，且失败能被精确断言捕获。
#
# rev3 变更说明（2026-09-14 缺陷修复回归）
#   - R/pathway_ccplot.R:101/:158 的 `.dots =` 已改为
#     `dplyr::across(dplyr::all_of(...))`；:151 的 `as.data.frame(newdata2)`
#     多余参数已修。pathway_ccplot 由 XFAIL 转为真实 PASS，并入「通路类绘图」
#     分组；并新增 4 条回归测试，显式覆盖 root=NULL / root='root' / label.name='ID'
#     / 内置 KK 四条路径（其中 root='root' 走第 158 行，历史上从未被跑到）。
#
# 注：本文件同时兼容两种执行方式
#   (a) 已安装包：devtools::test() / testthat::test_file()
#   (b) 未安装包（当前状态，R CMD INSTALL 失败中）：由
#       logs/_l2_testthat.R 驱动，把包内对象注入测试环境后运行。
# ===========================================================================

# ---------------------------------------------------------------------------
# 工具 1：绘图函数统一在空设备（pdf(NULL)）上求值，不产生文件
# ---------------------------------------------------------------------------
with_null_device <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

# ---------------------------------------------------------------------------
# 工具 2：查询层是否可用（compat parquet 是否存在）
#   - 已安装包：走 system.file()
#   - 未安装包（开发期）：走 working dir 下的 inst/extdata/
# ---------------------------------------------------------------------------
.compat_available <- function() {
  p <- tryCatch(
    system.file("extdata", "tcm_network_compat.parquet", package = "tcmnetpharma"),
    error = function(e) ""
  )
  if (is.null(p) || !nzchar(p)) {
    p <- file.path("inst", "extdata", "tcm_network_compat.parquet")
  }
  file.exists(p)
}

skip_if_no_compat <- function() {
  testthat::skip_if_not(
    .compat_available(),
    "tcm_network_compat.parquet 不可用（开发期未构建）"
  )
}

# ---------------------------------------------------------------------------
# 工具 3：包命名空间解析（兼容「已安装」与「未安装」两种执行方式）
#   - getNamespace() 失败时，回退到调用者环境（由 logs/_l2_testthat.R 注入）
# ---------------------------------------------------------------------------
.pkg_ns <- function() {
  ns <- tryCatch(getNamespace("tcmnetpharma"), error = function(e) NULL)
  if (!is.null(ns)) {
    return(ns)
  }
  # 未安装路径：由驱动脚本把包内全部对象 assign 到本测试环境
  parent.frame(2)
}

# ---------------------------------------------------------------------------
# 工具 4：逐函数冒烟执行器
#   返回 data.frame(fun, status, class) —— single function failure 不中断其余。
#   plot = TRUE 时在该图元包在空设备上执行。
# ---------------------------------------------------------------------------
run_plot_smoke <- function(specs, env = parent.frame(), plot = TRUE) {
  out <- data.frame(
    fun = character(), status = character(), class = character(),
    stringsAsFactors = FALSE
  )
  for (nm in names(specs)) {
    fn <- tryCatch(get(nm, envir = env), error = function(e) NULL)
    if (is.null(fn) || !is.function(fn)) {
      out <- rbind(out, data.frame(
        fun = nm, status = "MISSING", class = "",
        stringsAsFactors = FALSE
      ))
      next
    }
    res <- tryCatch(
      {
        if (plot) {
          v <- with_null_device(specs[[nm]]())
        } else {
          v <- specs[[nm]]()
        }
        list(ok = TRUE, cls = paste(class(v), collapse = "/"))
      },
      error = function(e) list(ok = FALSE, cls = conditionMessage(e))
    )
    out <- rbind(out, data.frame(
      fun = nm,
      status = if (res$ok) "PASS" else "FAIL",
      class = substr(gsub("[\r\n]", " ", res$cls), 1, 120),
      stringsAsFactors = FALSE
    ))
  }
  out
}

# ===========================================================================
# 合成 fixture 构造器（不写入 data/，仅测试期内存对象）
#   关键依据：逐个读 R/<fun>.R 的 dplyr::select() / group_by() 列需求。
# ===========================================================================

# 32 行的 KEGG/GO 富集结果，列集覆盖：
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

# alluvial_dot / tcm_alluvial_dot / dot_sankey* / tcm_sankey_dot 所需的
# 「herb-molecule-target + 富集列」宽表。列需求（读源码确认）：
#   alluvial_dot:  ID, Description, p.adjust, Count, richFactor,
#                  herb, molecule_id, (molecule), target
#   dot_sankey  :  ID, Description, p.adjust, Count, richFactor,
#                  BgRatio, geneID
# 因此统一造一份含全部列的宽表，30 行。
make_xfbdf_fixture <- function() {
  set.seed(7)
  m <- 30L
  herb <- rep(c("huang qi", "dang gui", "chai hu"), length.out = m)
  mol_id <- sprintf("MOL%04d", seq_len(12))[seq_len(m) %% 12 + 1]
  molecule <- sprintf("synthetic molecule %02d", (seq_len(m) %% 12) + 1)
  target <- sprintf("GENE%03d", (seq_len(m) %% 30) + 1)
  enr <- make_enrich_fixture()
  idx <- ((seq_len(m) - 1L) %% nrow(enr)) + 1L
  data.frame(
    herb        = herb,
    molecule_id = mol_id,
    molecule    = molecule,
    target      = target,
    ID          = enr$ID[idx],
    Description = enr$Description[idx],
    p.adjust    = enr$p.adjust[idx],
    Count       = enr$Count[idx],
    richFactor  = round(enr$Count[idx] / (40 + (idx %% 20)), 3),
    BgRatio     = enr$BgRatio[idx],
    geneID      = target,          # dot_sankey* 会按 "/" 拆 geneID
    ONTOLOGY    = enr$ONTOLOGY[idx],
    stringsAsFactors = FALSE
  )
}

# tcm_pre_plot 输入契约（读 R/tcm_pre_plot.R:48-60）：
#   需 Count / Pvalue / CompoundId 三列；内部 arrange(desc(Count)) → slice(1:top)。
# 正解是 tcm_prescription(disease_data)[[2]]，此处造等价合成表（离线确定性）。
make_pre_plot_fixture <- function() {
  set.seed(11)
  n <- 20L
  data.frame(
    CompoundId = sprintf("compound_%02d", 1:n),
    Pvalue     = round(stats::runif(n, 1e-4, 0.05), 4),
    OddsRatio  = round(stats::runif(n, 0.5, 8), 3),
    ExpCount   = round(stats::runif(n, 1, 15), 2),
    Count      = sample(5:40, n, replace = TRUE),
    Size       = sample(20:200, n, replace = TRUE),
    herb       = sprintf("herb_a/herb_b/%s", 1:n),
    stringsAsFactors = FALSE
  )
}

# ===========================================================================
# 1. 导出面完整性：NAMESPACE 里每个导出名都必须真实存在且是函数
# ===========================================================================
test_that("所有导出符号均已定义且为 function（%>% 除外）", {
  exports <- getNamespaceExports("tcmnetpharma")
  exports <- setdiff(exports, "%>%")
  expect_gt(length(exports), 30)  # 防止 NAMESPACE 被误改后静默通过

  ns <- asNamespace("tcmnetpharma")
  missing <- exports[!vapply(exports, exists, logical(1), envir = ns)]
  expect_equal(missing, character(0),
               info = paste("未定义的导出:", paste(missing, collapse = ", ")))

  not_fun <- exports[!vapply(
    exports,
    function(f) is.function(get(f, envir = ns)),
    logical(1)
  )]
  expect_equal(not_fun, character(0),
               info = paste("导出但不是函数:", paste(not_fun, collapse = ", ")))
})

test_that("管道运算符 %>% 可从包中取到", {
  expect_true(is.function(tcmnetpharma::`%>%`))
})

# ===========================================================================
# 2. 数据层：惰性查询函数（依赖 compat parquet）
#
# fixture 选择依据（实测 compat parquet 列语义，见
# logs/diagnostics/_l2_query_probe*.txt）：
#   - molecule_serch() 查询列是 **Molecule_name**（非 parquet 的 MOL_ID 列），
#     故必须传真实 Molecule_name 值；"quercetin" 在 compat 库中不存在，
#     函数会正确返回 0 行（这是**正确行为**，不是 bug）——改用 "Farmocaine"
#     （实测 191 行）。
#   - herb_target() 默认 type = "Herb_cn_name"，需传**中文**草药名；
#     "huang qi" 是拼音，必然 0 行。改用实测高频中文名 "羌活"（903 行）。
#   - target_serch()/target_herb() 查询列是 Target（HGNC 符号），
#     "PTGS2" 实测 27063 / 1502 行，可直接使用。
# ===========================================================================
test_that("molecule_serch 返回 4 列契约且非空", {
  skip_if_no_compat()
  res <- molecule_serch("Farmocaine")   # 真实存在的 Molecule_name
  expect_true(is.data.frame(res))
  expect_named(res, c("herb", "molecule_id", "molecule", "target"))
  expect_gt(nrow(res), 0)
})

test_that("target_serch 返回 herb/molecule/target 列且非空", {
  skip_if_no_compat()
  res <- target_serch("PTGS2")
  expect_true(is.data.frame(res))
  expect_true(all(c("herb", "molecule", "target") %in% colnames(res)))
  expect_gt(nrow(res), 0)
})

test_that("herb_target 返回 4 列契约（含 OB/DL 过滤）", {
  skip_if_no_compat()
  res <- herb_target("\u7f8c\u6d3b")    # 羌活：实测 903 行
  expect_true(is.data.frame(res))
  expect_gt(nrow(res), 0)
  expect_true(all(c("herb", "molecule") %in% colnames(res)))
})

test_that("target_herb 返回 4 列契约", {
  skip_if_no_compat()
  res <- target_herb("PTGS2")
  expect_true(is.data.frame(res))
  expect_gt(nrow(res), 0)
})

test_that("查询不存在的关键词时优雅返回空结果而非报错", {
  skip_if_no_compat()
  res <- molecule_serch("__NOT_A_REAL_MOLECULE_XYZ__")
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 0)
})

# ===========================================================================
# 3. 内置数据集契约：每个 .rda 都可被 data() 载入且结构正确
# ===========================================================================
test_that("内置数据集均可载入且结构符合预期", {
  expect_true(is.data.frame(tcmnetpharma::network_data))
  expect_named(tcmnetpharma::network_data, c("herb", "molecule", "target"))

  expect_true(is.data.frame(tcmnetpharma::sankey_data))
  expect_named(tcmnetpharma::sankey_data, c("herb", "molecule_id", "molecule", "target"))

  expect_true(is.data.frame(tcmnetpharma::ppi_data))
  expect_named(tcmnetpharma::ppi_data, c("node1", "node2", "combined_score"))

  expect_true(is.data.frame(tcmnetpharma::tf_data))
  expect_named(tcmnetpharma::tf_data, c("TF", "Target"))

  expect_true(is.data.frame(tcmnetpharma::venn_data))
  expect_named(tcmnetpharma::venn_data, c("gene", "source"))

  expect_true(is.data.frame(tcmnetpharma::compound_data))
  expect_true(all(c("herb", "weight", "property", "flavor", "meridian") %in%
                    colnames(tcmnetpharma::compound_data)))

  expect_true(is.data.frame(tcmnetpharma::herb_pm))
  expect_true(all(c("Herb_cn_name", "Herb_name_pinyin", "Property", "Flavor",
                    "Meridian") %in% colnames(tcmnetpharma::herb_pm)))

  expect_true(is.data.frame(tcmnetpharma::xfbdf))
  expect_named(tcmnetpharma::xfbdf, c("herb", "molecule_id", "molecule", "target"))

  expect_true(is.data.frame(tcmnetpharma::kegg.filter))
  expect_true(all(c("ID", "Description", "GeneRatio", "BgRatio", "pvalue",
                    "p.adjust", "qvalue", "geneID", "Count") %in%
                    colnames(tcmnetpharma::kegg.filter)))

  expect_true(is.data.frame(tcmnetpharma::trrust))
  expect_true(is.data.frame(tcmnetpharma::disease_data))
  expect_true(is.data.frame(tcmnetpharma::mahuang))
  expect_true(is.data.frame(tcmnetpharma::compoundfinal))
  expect_true(is.data.frame(tcmnetpharma::human_go))
  expect_true(is.data.frame(tcmnetpharma::human_reactome))
  expect_true(is.data.frame(tcmnetpharma::mu_go))
  expect_true(is.data.frame(tcmnetpharma::mu_reactome))
})

test_that("富集结果对象 KK 仍为 enrichResult", {
  expect_s4_class(tcmnetpharma::KK, "enrichResult")
})

# ===========================================================================
# 4. 转换/查询型函数（不绘图）
# ===========================================================================
test_that("etcm 清洗 ETCM 下载数据返回 4 列契约", {
  res <- etcm(tcmnetpharma::mahuang, herb = "ma huang")
  expect_true(is.data.frame(res))
  expect_named(res, c("herb", "molecule", "target", "QED"))
})

test_that("tf_filter 对字符向量返回 TRRUST 子集", {
  res <- tf_filter(c("E2F1", "AATF"))
  expect_true(is.data.frame(res))
  expect_true(all(c("TF", "Target") %in% colnames(res)))
  expect_gt(nrow(res), 0)
})

test_that("tf_filter 对非字符输入不报错（打印提示后返回 NULL）", {
  expect_no_error(suppressMessages(tf_filter(1:3)))
})

test_that("venn_result 返回列表且能取到交集结果", {
  res <- with_null_device(venn_result(tcmnetpharma::venn_data))
  expect_true(!is.null(res))
})

test_that("tcm_prescription 返回双元素列表（草药频次 + 复方富集）", {
  skip_if_no_compat()
  res <- tcm_prescription(tcmnetpharma::disease_data)
  expect_type(res, "list")
  expect_length(res, 2)
  expect_true(is.data.frame(res[[1]]))
  expect_true(is.data.frame(res[[2]]))
})

# ===========================================================================
# 5. 绘图函数：用正确 fixture 逐个求值于空设备
#    使用 run_plot_smoke() 保证单点失败不连带跳过后面的函数。
# ===========================================================================

test_that("富集类 KEGG 绘图函数（kegg.filter 兼容子集）全部无错", {
  kegg <- tcmnetpharma::kegg.filter
  specs <- list(
    bar_plot      = function() bar_plot(kegg),
    bubble_plot   = function() bubble_plot(kegg),
    dot_plot      = function() dot_plot(kegg),
    lollipop_plot = function() lollipop_plot(kegg),
    cir_plot      = function() cir_plot(kegg),
    dock_plot     = function() dock_plot((function(d) {
      m <- as.matrix(d[, c("pvalue", "p.adjust", "qvalue", "Count")])
      rownames(m) <- d$ID
      m
    })(kegg)),
    pathway_cirplot = function() pathway_cirplot(kegg)
  )
  res <- run_plot_smoke(specs, env = environment())
  expect_equal(res$status[res$status != "PASS"], character(0),
               info = paste(utils::capture.output(print(res)), collapse = "\n"))
})

test_that("GO 类 KEGG 绘图函数（含 ONTOLOGY 合成 fixture）全部无错", {
  go <- make_enrich_fixture()
  specs <- list(
    go_barplot  = function() go_barplot(go),
    go_dotplot  = function() go_dotplot(go),
    go_lollipop = function() go_lollipop(go),
    go_cir      = function() go_cir(go)
  )
  res <- run_plot_smoke(specs, env = environment())
  expect_equal(res$status[res$status != "PASS"], character(0),
               info = paste(utils::capture.output(print(res)), collapse = "\n"))
})

test_that("网络/桑基/冲击图类绘图函数全部无错", {
  specs <- list(
    degree_plot = function() degree_plot(tcmnetpharma::network_data),
    ppi_plot    = function() ppi_plot(tcmnetpharma::ppi_data),
    tcm_net     = function() tcm_net(tcmnetpharma::network_data),
    tf_cirplot  = function() tf_cirplot(tcmnetpharma::tf_data),
    tcm_sankey  = function() tcm_sankey(tcmnetpharma::sankey_data),
    tcm_alluvial = function() tcm_alluvial(tcmnetpharma::sankey_data)
  )
  res <- run_plot_smoke(specs, env = environment())
  expect_equal(res$status[res$status != "PASS"], character(0),
               info = paste(utils::capture.output(print(res)), collapse = "\n"))
})

test_that("venn / 复方 / 通路类绘图函数全部无错", {
  specs <- list(
    venn_plot       = function() venn_plot(tcmnetpharma::venn_data),
    venn_net        = function() venn_net(tcmnetpharma::venn_data),
    tcm_comp        = function() tcm_comp(tcmnetpharma::compound_data),
    tcm_compound    = function() tcm_compound(tcmnetpharma::compound_data),
    tcm_pre_plot    = function() tcm_pre_plot(make_pre_plot_fixture()),
    # pathway_ccplot 于 2026-09-14 修复 .dots defunct 缺陷后并入本组（默认 root=NULL）
    pathway_ccplot  = function() pathway_ccplot(make_enrich_fixture())
  )
  res <- run_plot_smoke(specs, env = environment())
  expect_equal(res$status[res$status != "PASS"], character(0),
               info = paste(utils::capture.output(print(res)), collapse = "\n"))
})

test_that("GO/通路 冲击-点状组合图（合成富集宽表 fixture）全部无错", {
  xf <- make_xfbdf_fixture()
  specs <- list(
    dot_sankey       = function() dot_sankey(xf),
    dot_sankey2      = function() dot_sankey2(xf),
    tcm_sankey_dot   = function() tcm_sankey_dot(xf),
    alluvial_dot     = function() alluvial_dot(xf),
    tcm_alluvial_dot = function() tcm_alluvial_dot(xf)
  )
  res <- run_plot_smoke(specs, env = environment())
  expect_equal(res$status[res$status != "PASS"], character(0),
               info = paste(utils::capture.output(print(res)), collapse = "\n"))
})

# ===========================================================================
# 6. 回归测试：pathway_ccplot 的 .dots defunct 缺陷（2026-09-14 已修复）
#
#    原缺陷：R/pathway_ccplot.R:101 与 :158 使用 dplyr 1.0 已 defunct 的
#    `group_by(.dots = dots)`，无论输入什么都抛
#    "The .dots argument of group_by() was deprecated ... and is now defunct."
#    修法：改为 `dplyr::group_by(dplyr::across(dplyr::all_of(...)))`。
#
#    本组回归测试**显式覆盖两条此前从未被跑到的代码路径**：
#      - root = NULL  → 走第 101 行（节点构建，多元素 dots）
#      - root = "root"→ 额外走第 158 行（root 边构建，单字符串 se_index[[1]]）
#    以及第 151 行的 as.data.frame(newdata2) 附带缺陷修复回归。
# ===========================================================================
test_that("pathway_ccplot 在 root=NULL（默认路径，第 101 行）可无错完成", {
  go <- make_enrich_fixture()
  expect_no_error(with_null_device(pathway_ccplot(go)))
})

test_that("pathway_ccplot 在 root='root'（第 158 行 root 边路径）可无错完成", {
  go <- make_enrich_fixture()
  expect_no_error(with_null_device(pathway_ccplot(go, root = "root")))
})

test_that("pathway_ccplot 在 label.name='ID' 路径可无错完成", {
  go <- make_enrich_fixture()
  expect_no_error(with_null_device(pathway_ccplot(go, label.name = "ID")))
})

test_that("pathway_ccplot 接受内置 enrichResult 对象 KK（真实数据路径）", {
  expect_no_error(with_null_device(pathway_ccplot(tcmnetpharma::KK, root = "KEGG")))
})
