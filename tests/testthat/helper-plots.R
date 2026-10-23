# 绘图测试的公共辅助（testthat 自动 source tests/testthat/helper-*.R）
#
# 历史（为什么 helper 里还留着 patchwork 分支）：
#   2026-10-23 上午 `go_barplot(style="grouped")` 一度返回 **patchwork**
#   （三段各带独立色阶），而 patchwork 没有单一 `$data` —— `p$data` 只拿到
#   **第一段**的条目，直接用它断言会得到「条目集合不一致」的**假失败**。
#   同日下午 boss 决定「柱状图不用色阶，按 BP/CC/MF 给 3 个分类色」，
#   grouped 改回 facet + scale_fill_manual，**又是普通 ggplot**，`$data` 完整。
#
#   那为什么分支还留着：本轮踩的坑是「对象形态变了，测试跟着静默失真」。
#   分支留着，任何形态下 items_of() 都给出**全量条目**，不会因为哪天再换
#   实现（patchwork / 嵌套 grob 之类）而退化成"只数到第一段"。这层防御
#   比删掉它便宜。
#
# 另注：断言「能出图」是**不够的**。要锁住「柱状图没有色阶」，必须看 fill
# 标度的类型（ScaleDiscrete vs ScaleContinuous），见 test-style-presets.R。

# 取一张图里承载的条目集合（Description 列）。
# 返回排序去重的字符向量；取不到时返回 character(0)。
items_of <- function(p) {
  if (is.null(p)) return(character(0))

  one <- function(sp) {
    d <- tryCatch(sp$data, error = function(e) NULL)
    if (is.null(d) || !is.data.frame(d) || !"Description" %in% names(d)) {
      return(character(0))
    }
    as.character(d$Description)
  }

  # patchwork：p[[i]] 依次取子图（实验确认 length(pw) = 3、pw[[1]]$data 可读）
  if (inherits(p, "patchwork")) {
    n <- tryCatch(length(p), error = function(e) 0L)
    if (!is.numeric(n) || n < 1L) return(character(0))
    parts <- lapply(seq_len(n), function(i) {
      one(tryCatch(p[[i]], error = function(e) NULL))
    })
    return(sort(unique(unlist(parts, use.names = FALSE))))
  }

  sort(unique(one(p)))
}

# 断言用：条目集合差异的可读描述（失败时给出具体差了几条）
items_diff <- function(a, b) {
  sprintf("差异 %d 条（多 %d / 少 %d）",
          length(setdiff(a, b)) + length(setdiff(b, a)),
          length(setdiff(a, b)), length(setdiff(b, a)))
}

# 取图里**所有**标题文本（整体标题 + 各段标题）。
# 为什么需要：patchwork 把整体标题存在 `p$patches$annotation$title`，段标题在
# 各子图 `$labels$title` —— 两者不在同一处，`p$labels$title` 取不到整体标题。
# 若某天实现把用户传的 title 丢了，只看一个位置会漏检。
ggplot_titles <- function(p) {
  if (is.null(p)) return(character(0))
  out <- character(0)

  ann <- tryCatch(p$patches$annotation$title, error = function(e) NULL)
  if (is.character(ann) && length(ann)) out <- c(out, ann)

  one <- function(sp) {
    t <- tryCatch(sp$labels$title, error = function(e) NULL)
    if (is.character(t) && length(t)) t else character(0)
  }

  if (inherits(p, "patchwork")) {
    n <- tryCatch(length(p), error = function(e) 0L)
    if (is.numeric(n) && n >= 1L) {
      out <- c(out, unlist(lapply(seq_len(n), function(i) {
        one(tryCatch(p[[i]], error = function(e) NULL))
      }), use.names = FALSE))
    }
  } else {
    out <- c(out, one(p))
  }

  out[!is.na(out)]
}

