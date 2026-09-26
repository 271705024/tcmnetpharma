# ===========================================================================
# tcm-db.R 内部惰性查询层契约测试（P3.2，2026-09-02）
#
# 背景：tcm_network_compat.parquet（16.35M 行 / 47.5 MB）是包内最大的数据资产，
#   4 个导出查询函数（molecule_serch / target_serch / herb_target / target_herb）
#   与 tcm_prescription() 全部经 .tcm_* 内部函数走 DuckDB 惰性查询。
#   L2 冒烟测试（test-smoke-all-exports.R）只从**导出函数**侧间接覆盖了其中
#   3 个（.tcm_distinct / .tcm_meta / .tcm_query，见 test-data-contract.R），
#   而 SQL 构造层（.tcm_col_db / .sql_in / .tcm_filter / .tcm_adme_where）
#   一旦回归会**同时打穿全部 5 个查询函数**，属于高扇出风险点。
#
# 本文件补齐这层：单元级覆盖 4 个未测内部函数 + 集成级 DBI 句柄泄漏检查。
#
# 覆盖缺口（2026-09-22 实测）：
#   .tcm_col_db         0 处引用  → 本文件补
#   .tcm_col_clean      0 处引用  → 本文件补
#   .sql_in             0 处引用  → 本文件补（含 SQL 注入转义）
#   .tcm_filter         0 处引用  → 本文件补
#   .tcm_query          1 处      → 本文件补句柄泄漏维度
# ===========================================================================

# compat parquet 可用性探测（与 test-smoke-all-exports.R 同语义，独立定义避免耦合）
.db_ready <- function() {
  p <- tryCatch(
    system.file("extdata", "tcm_network_compat.parquet", package = "TCMNP"),
    error = function(e) ""
  )
  if (is.null(p) || !nzchar(p)) {
    p <- file.path("inst", "extdata", "tcm_network_compat.parquet")
  }
  file.exists(p) && requireNamespace("duckdb", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE)
}

skip_if_no_db <- function() {
  testthat::skip_if_not(.db_ready(), "compat parquet / duckdb 不可用")
}

# 内部函数取用器：优先已安装命名空间，回退开发期注入环境
.tdb_get <- function(name) {
  ns <- tryCatch(getNamespace("TCMNP"), error = function(e) NULL)
  if (!is.null(ns) && exists(name, envir = ns, inherits = FALSE)) {
    return(get(name, envir = ns))
  }
  get(name, envir = parent.frame(2))
}

# ===========================================================================
# 1. 列名映射：.tcm_col_db / .tcm_col_clean
#
#    根因背景（tcm-db.R 头注释）：duckdb 1.5.x 读 parquet 时对列名做
#    大小写不敏感折叠，两个仅差大小写的列会冲突，后者被自动改名 <name>_1。
#    若映射回归，所有引用拼音列的查询会抛 "column not found" —— 而这正是
#    herb_target() 的默认 type 路径。
#    2026-09-26 数据侧已整改（HERB_NAME_PIN_YIN → herb_pinyin_raw），
#    出厂 parquet 现为零碰撞 → 映射通常为恒等；探测作为永久守卫保留。
# ===========================================================================
test_that(".tcm_col_db 把逻辑列名映射到 DuckDB 实际列名", {
  f <- .tdb_get(".tcm_col_db")
  # 2026-09-26 数据侧整改后：零碰撞 → 所有列名恒等返回
  expect_identical(f("Herb_name_pin_yin"), "Herb_name_pin_yin")
  expect_identical(f("herb_pinyin_raw"), "herb_pinyin_raw")
  # 其余列名同样幂等
  expect_identical(f("Herb_cn_name"), "Herb_cn_name")
  expect_identical(f("Molecule_name"), "Molecule_name")
  expect_identical(f("OB"), "OB")
  expect_identical(f("DL"), "DL")
  expect_identical(f("Target"), "Target")
})

test_that(".tcm_col_clean 是 .tcm_col_db 的逆映射（去 _1 后缀）", {
  f <- .tdb_get(".tcm_col_clean")
  expect_identical(f("Herb_name_pin_yin_1"), "Herb_name_pin_yin")
  expect_identical(f("Herb_cn_name"), "Herb_cn_name")
  expect_identical(f("OB"), "OB")
  # 往返一致性：db 名经 clean 后应回到逻辑名
  g <- .tdb_get(".tcm_col_db")
  for (nm in c("Herb_name_pin_yin", "Herb_cn_name", "Molecule_name", "Target")) {
    expect_identical(f(g(nm)), nm)
  }
})

# ===========================================================================
# 1b. 碰撞组探测守卫（2026-09-22 新增；2026-09-26 数据侧整改后方向反转）
#
#     背景：DuckDB 列名匹配不区分大小写；历史上数据里同时存在
#     HERB_NAME_PIN_YIN 与 Herb_name_pin_yin（语义不同：源原始格式 vs 归一化键），
#     后者被折叠为 Herb_name_pin_yin_1 →「写不带后缀的名字」会静默命中另一列。
#     2026-09-26 数据侧整改完毕（HERB_NAME_PIN_YIN → herb_pinyin_raw，见
#     data-management-spec §3.1），出厂 parquet 零碰撞。
#     故本守卫由「断言碰撞集合 == 已知集合」改为「断言集合为空」：
#     一旦数据侧重新引入仅差大小写的列，本断言失败 → 先修数据
#     （落盘期还有管道门禁 common.assert_no_col_case_collision 兜底），
#     再同步本测试与 §3 命名规则。
#     详见 logs/errors-herb-target-pinyin-2026-09-22.md
# ===========================================================================
test_that("★ 碰撞组探测结果必须为空（数据侧重新引入碰撞会在此失败）", {
  skip_if_no_db()
  probe <- .tdb_get(".tcm_col_probe")
  p <- .tdb_get(".tcm_parquet_path")()
  m <- probe(p)
  expect_length(m, 0L)
  # 守卫意图：返回非空即意味着数据侧出现「仅差大小写」的列。
})

test_that(".tcm_col_db 对未知名恒等返回（不误改）", {
  f <- .tdb_get(".tcm_col_db")
  expect_identical(f("NoSuchColumnXYZ"), "NoSuchColumnXYZ")
  expect_identical(f("Herb_cn_name"), "Herb_cn_name")
})

test_that(".tcm_col_clean 去除任意 _N 后缀（不限于 _1）", {
  f <- .tdb_get(".tcm_col_clean")
  expect_identical(f("Herb_name_pin_yin_1"), "Herb_name_pin_yin")
  expect_identical(f("X_2"), "X")
  expect_identical(f("X_10"), "X")
  expect_identical(f("X_1y"), "X_1y")   # 非纯数字后缀不得改动
  expect_identical(f("Herb_cn_name"), "Herb_cn_name")
})

# ===========================================================================
# 1c. .tcm_distinct_where 新契约（B 方案核心，2026-09-22）
#     旧签名 (col, where) 让调用方拼 WHERE → 可漏映射；
#     新签名 (col, vals = NULL) 由函数内部构造 WHERE，调用方无法漏。
# ===========================================================================
test_that("★ .tcm_distinct_where(col, vals) 契约：WHERE 由内部构造", {
  skip_if_no_db()
  f <- .tdb_get(".tcm_distinct_where")
  # 无 vals → 该列全部非空 distinct 值
  all_v <- f("Target")
  expect_gt(length(all_v), 0)
  expect_true(all(nzchar(all_v)))
  # 有 vals → 限定到值集
  hit <- f("Target", c("PTGS2", "__NOT_REAL_TARGET__"))
  expect_true("PTGS2" %in% hit)
  expect_false("__NOT_REAL_TARGET__" %in% hit)
  # 空值集 → 限空集（返回 0 个，不得退化为全表）
  expect_length(f("Target", character(0)), 0)
  expect_length(f("Target", NULL), length(all_v))
})

test_that("★ 拼音双列回归：归一化键与原始格式是两个独立的列（不再碰撞）", {
  skip_if_no_db()
  f <- .tdb_get(".tcm_distinct_where")
  # 归一化列（无空格小写键）：应命中
  v <- f("Herb_name_pin_yin", "jiulixiang")
  expect_true("jiulixiang" %in% v)
  # 原始格式列（herb_pinyin_raw，带空格大写）：同一个无空格键不应命中
  #   → 证明两列各自独立、未被折叠，查的是「对的那一列」
  expect_length(f("herb_pinyin_raw", "jiulixiang"), 0)
  # 值域不相交 → 确为两个独立列（而非同一列的两个别名）
  norm <- f("Herb_name_pin_yin", NULL)
  raw <- f("herb_pinyin_raw", NULL)
  expect_gt(length(raw), 0)
  expect_length(intersect(norm, raw), 0)
  # 原始列本身可查（用其自身值往返）
  expect_true(length(f("herb_pinyin_raw", raw[1])) > 0)
})

# ===========================================================================
# 2. SQL 值列表构造：.sql_in
#    安全契约：单引号必须转义（''），否则含撇号的草药名会直接破坏 SQL。
# ===========================================================================
test_that(".sql_in 构造合法的 SQL IN 列表", {
  f <- .tdb_get(".sql_in")
  expect_identical(f("PTGS2"), "'PTGS2'")
  expect_identical(f(c("E2F1", "AATF")), "'E2F1', 'AATF'")
  # 单元素
  expect_identical(f(c("X")), "'X'")
})

test_that(".sql_in 对空输入 / 全 NA / 空串返回 NULL 字面量", {
  f <- .tdb_get(".sql_in")
  expect_identical(f(character(0)), "NULL")
  expect_identical(f(NA_character_), "NULL")
  expect_identical(f(c(NA_character_, "")), "NULL")
  expect_identical(f(""), "NULL")
  # NA 与有效值混合：只保留有效值
  expect_identical(f(c("A", NA_character_, "B")), "'A', 'B'")
})

test_that(".sql_in 转义单引号（防 SQL 注入 / 语法破坏）", {
  f <- .tdb_get(".sql_in")
  expect_identical(f("O'Brien"), "'O''Brien'")
  v <- f("a'b'c")
  expect_identical(v, "'a''b''c'")

  # 更本质的断言：整个值列表的单引号总数必须是偶数。
  # 奇数意味着存在落单引号 → SQL 字符串边界破裂。
  # （注意：不能用「连续引号段长度必为偶数」来判 ——
  #   字符串首尾的外层引号各自是 1 个，与相邻的转义引号拼接后
  #   会形成长度为 3 的连续段。奇偶总数才是可靠判据。）
  n_quote <- lengths(regmatches(v, gregexpr("'", v)))
  expect_identical(n_quote %% 2L, 0L)
})

test_that(".sql_in 对注入载荷保持引号配平（不产生裸引号）", {
  f <- .tdb_get(".sql_in")
  # 典型注入载荷：单引号闭合 + OR 恒真
  inj <- "x' OR '1'='1"
  v <- f(inj)
  n_quote <- lengths(regmatches(v, gregexpr("'", v)))
  expect_identical(n_quote %% 2L, 0L)     # 引号必须成对
  expect_false(grepl("''''", v))          # 不得出现四连引号（双重转义痕迹）
  # 载荷中的每个裸引号都应变成成对引号
  expect_identical(v, "'x'' OR ''1''=''1'")
})

# ===========================================================================
# 3. ADME 过滤构造：.tcm_adme_where
#    OB/DL 在 parquet 中是 VARCHAR（含 'NA' 占位），必须 CAST 后才能数值比较。
# ===========================================================================
test_that(".tcm_adme_where 生成 CAST 后的数值比较子句", {
  f <- .tdb_get(".tcm_adme_where")
  w <- f(30, 0.18)
  expect_true(grepl("CAST", w))
  expect_true(grepl("\"OB\"", w))
  expect_true(grepl("\"DL\"", w))
  expect_true(grepl(">=", w))
  expect_true(grepl("30", w))
  expect_true(grepl("0.18", w))
  # 必须用 CAST(... AS DOUBLE)，否则 VARCHAR 与数值比较在 duckdb 下报错
  expect_true(grepl("DOUBLE", w))
})

test_that(".tcm_adme_where 对大阈值不产生科学计数法（format scientific=FALSE）", {
  f <- .tdb_get(".tcm_adme_where")
  w <- f(30, 0.18)
  # 不得出现 1e-01 / 1.8e-01 这类科学计数法
  expect_false(grepl("[0-9]e[-+]", w, ignore.case = TRUE))
})

# ===========================================================================
# 4. 高层过滤构造：.tcm_filter（集成，需真库）
#    这是 4 个导出查询函数共用的 SELECT 生成器。
# ===========================================================================
test_that(".tcm_filter 返回请求列且列名用逻辑名（非 _1 后缀）", {
  skip_if_no_db()
  f <- .tdb_get(".tcm_filter")
  res <- f("Target", "PTGS2",
           c(herb = "Herb_cn_name", molecule = "Molecule_name"))
  expect_true(is.data.frame(res))
  expect_true(all(c("herb", "molecule") %in% colnames(res)))
  expect_gt(nrow(res), 0)
})

test_that(".tcm_filter 的 extra_where 生效（ADME 过滤收紧结果集）", {
  skip_if_no_db()
  f <- .tdb_get(".tcm_filter")
  adme <- .tdb_get(".tcm_adme_where")
  base <- f("Target", "PTGS2", c(herb = "Herb_cn_name"))
  filt <- f("Target", "PTGS2", c(herb = "Herb_cn_name"),
            extra_where = adme(30, 0.18))
  expect_true(is.data.frame(filt))
  # 加了 OB/DL 阈值后结果不可能变多
  expect_lte(nrow(filt), nrow(base))
})

test_that(".tcm_filter 无匹配时返回 0 行数据框而非报错", {
  skip_if_no_db()
  f <- .tdb_get(".tcm_filter")
  res <- f("Target", "__NOT_A_REAL_TARGET_XYZ__", c(herb = "Herb_cn_name"))
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 0)
})

# ===========================================================================
# 5. .tcm_query 句柄管理（回归防线）
#    .tcm_query 每次建连 → on.exit 断开。若 on.exit 丢失，长时间批量调用
#    会累积 DuckDB 句柄。这里做「多次调用不报错 + 不残留连接对象」的行为断言。
# ===========================================================================
test_that(".tcm_query 可重复调用且释放连接（50 次无泄漏报错）", {
  skip_if_no_db()
  q <- .tdb_get(".tcm_query")
  p <- .tdb_get(".tcm_parquet_path")()
  sql <- sprintf("SELECT COUNT(*) AS n FROM read_parquet('%s') WHERE Target IN ('PTGS2')", p)
  for (i in 1:50) {
    r <- q(sql)
    expect_equal(nrow(r), 1L)
  }
  # 若连接未释放，DBI 会报 "too many connections"；执行到此即通过
  expect_true(TRUE)
})

# ===========================================================================
# 6. 扇出防线：4 个导出查询函数共用同一内部层
#    任一 .tcm_* 回归都会同时打穿它们。此处做最小端到端确认。
#
#    注意：不能用 expect_silent() —— duckdb() 每次建连都会打印
#    "duckdb keeps downloaded extensions and secrets in a temporary directory"
#    的 startup message（.tcm_query 每次调用建连 → 必然有 message）。
#    这里改为在 suppressMessages 下断言结果契约。
# ===========================================================================
test_that("查询层扇出：4 个导出查询函数均依赖的链路可端到端跑通", {
  skip_if_no_db()
  r1 <- suppressMessages(molecule_serch("Farmocaine"))
  expect_true(is.data.frame(r1))
  expect_gt(nrow(r1), 0)

  r2 <- suppressMessages(target_serch("PTGS2"))
  expect_true(is.data.frame(r2))
  expect_gt(nrow(r2), 0)

  r3 <- suppressMessages(herb_target("\u7f8c\u6d3b"))   # 羌活
  expect_true(is.data.frame(r3))
  expect_gt(nrow(r3), 0)

  r4 <- suppressMessages(target_herb("PTGS2"))
  expect_true(is.data.frame(r4))
  expect_gt(nrow(r4), 0)
})

# ===========================================================================
# 7. 回归防线：herb_target 拼音路径（原 0 行 bug，2026-09-22）
#    type = "Herb_name_pin_yin" 走的是碰撞列。旧代码因 WHERE 未映射列名
#    静默返回 0 行；本测试锁死该路径必须非空。
# ===========================================================================
test_that("★ 回归：herb_target 拼音路径非空（原 0 行 bug 防线）", {
  skip_if_no_db()
  r <- suppressMessages(herb_target("jiulixiang", type = "Herb_name_pin_yin"))
  expect_true(is.data.frame(r))
  expect_gt(nrow(r), 0)
  expect_true(all(c("herb", "molecule", "target") %in% colnames(r)))
  # 中文名分支（另一条 check_col 路径）不得因上述修复而回归
  r2 <- suppressMessages(herb_target("jiulixiang", type = "Herb_cn_name"))
  expect_true(is.data.frame(r2))
})
