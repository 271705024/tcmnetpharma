# 绘图测试的公共辅助（testthat 自动 source tests/testthat/helper-*.R）
#
# 为什么要有它：`go_barplot(style = "grouped")` 返回的是 **patchwork** 对象
# （三段各自独立色阶，2026-10-23 按 boss 决策改的），而 patchwork 没有
# 单一 `$data` —— 直接 `p$data` 只拿到**第一段**的条目。
# 若测试仍用 `p$data`，就会得出「条目集合与 default 不一致」的**假失败**；
# 反过来若某天 `$data` 变成 NULL，两边都空又会**假通过**。
# 故统一走 items_of()：patchwork 穿透到每个子图取并集（铁律 28）。

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

