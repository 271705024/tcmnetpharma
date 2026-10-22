library(testthat)
library(tcmnetpharma)

# ⚠️ 这里的包名必须是 tcmnetpharma（2026-10-22 修）：
# 更名后这里仍是 test_check("TCMNP")，而库里还装着旧包 TCMNP —— test_check
# 会**加载旧包并跑它的测试**，于是 R CMD check 一直在测旧代码：新写的用例
# 在旧函数上跑（如 bar_plot(style=) 因旧函数无该形参被 ... 吞掉、不报错），
# 出现「本地测试全绿、check 却 ERROR」的假象。旧包卸载前这行不能改回去。
test_check("tcmnetpharma")
