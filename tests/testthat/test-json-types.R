# 包层：JSON 集成类型宽容（防 2026-10-19 缺陷复发）
#
# 缺陷实录：GUI（RDesk）把 JSON 顶层数组/字符串转成 R `list`，而 `tcm_enrich()`
# 内部用 `match.arg()` 校验 species / ont —— 它只接受 character vector，遇 list
# 抛 "'arg' must be NULL or a character vector"。用户在界面上看到的是
# "内部错误：…"，误以为程序卡死（日志实证：1,115 个基因在加载完依赖后
# **2.75 秒**即失败，压根没进计算）。
#
# 修法：包层先 `as.character(unlist(...))` 归一，再交给 match.arg 校验
# （非法**取值**仍报可读错误，非法**类型**不再崩）。

test_that("tcm_enrich: list 包装的 gene / species / ont 不应崩", {
  expect_no_error(
    r <- tcm_enrich(list("TP53", "BRCA1", "EGFR", "KRAS"),
                    keytype = "SYMBOL",
                    species = list("human"),      # 模拟 JSON 字符串 -> list
                    ont = list("GO"),             # 模拟 JSON 数组   -> list
                    pvalueCutoff = 1)
  )
  expect_s4_class(r$go, "enrichResult")
  expect_gt(nrow(r$go@result), 0L)
})

test_that("tcm_enrich: list 与 character 输入结果完全一致", {
  genes <- c("TP53", "BRCA1", "EGFR", "KRAS", "PTEN")
  a <- tcm_enrich(genes, species = "human", ont = c("GO"), pvalueCutoff = 1)
  b <- tcm_enrich(as.list(genes), species = list("human"),
                  ont = list("GO"), pvalueCutoff = 1)
  expect_equal(nrow(a$go@result), nrow(b$go@result))
  expect_equal(sum(a$go@result$pvalue), sum(b$go@result$pvalue), tolerance = 1e-12)
})

test_that("tcm_enrich: 非法**取值**仍要报可读错误（宽容类型 ≠ 放过错误值）", {
  expect_error(tcm_enrich(c("TP53", "BRCA1"), species = list("rat")),
               "should be one of")
  # 注意：2026-10-21 起 "KEGG" 已是**合法**取值（联网富集），不能再拿它当非法样例；
  # 这里换成一个确定不存在的来源，保持"宽容类型 ≠ 放过错误值"的语义。
  expect_error(tcm_enrich(c("TP53", "BRCA1"), ont = list("NOT_A_SOURCE")),
               "should be one of")
})

test_that("tcm_intersect_target: list 包装的字符串参数不应崩", {
  expect_no_error(
    hit <- tcm_intersect_target(list("\u9ebb\u9ec4"), list("Asthma"),
                                type = list("Herb_cn_name"),
                                match = list("exact"))
  )
  expect_gt(nrow(hit), 100L)
})
