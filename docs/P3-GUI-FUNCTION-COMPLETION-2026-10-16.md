# TCMNP P3 GUI 功能补全处置清单（2026-10-16）

> 诊断依据：`docs/GAP-ANALYSIS-2026-10-16.md`（R 包层 + GUI 暴露层双矩阵 + 前端 `www/` 对账）。
> 本文档把"缺口"转成**可落地、可自检、待拍板**的行动项。
> 交付对象：面向**非技术用户**的 exe，故 GUI 暴露层优先级高于 R 包层。
> 纪律：铁律 65/69 —— 每阶段**限定路径提交**；共享文件(`NEWS.md`)加性共存，绝不整体 `git add -A`。

---

## 0. 对账结论（用户当前真实闭环）

**三层对齐**：前端可见 = 后端白名单 = 8 函数，无"前端暴露但后端不支持"的半对齐死链。

| 层 | 覆盖 | 用户感知缺口 |
|----|------|------|
| R 包导出 | ~39 函数 | G1 拓扑模块 / G2 对接计算 / G3 毒理 / G4 协同（R 层也缺） |
| GUI 后端白名单 | 8 函数 | 疾病 / 富集计算 / go 图 / PPI / 拓扑 / TF / 对接 / 处方 / 多数网络图 |
| GUI 前端可见 | 8 函数（与白名单 100% 一致） | 同上 |

**用户现在能做的**：4 种查询（看表→导 CSV）+ 4 种图（网络/富集柱/富集泡/Venn，其中富集图**只能画内置示例**）。
**用户做不到的**：疾病分析、端到端富集、PPI、拓扑、TF、对接、处方、绝大多数网络图。

---

## 1. 三阶段划分（go-decision 待拍板）

| 阶段 | 范围 | 为什么这个顺序 |
|------|------|------|
| **P3a** | G5 疾病进 GUI + G6 端到端富集 + G1 拓扑模块识别 | 直接补齐"查询→靶点→疾病→网络→富集→核心靶点"**标准闭环**；数据/R 函数基本就绪，主要是接线工作量 |
| **P3b** | G7 处方接入 + G8 多网络图 + G2 对接 + PPI/TF 接入 | 扩展分析维度；多数为"加白名单+最小 UI"，成本低 |
| **P3c** | G3 毒理/安全性 + G4 成分协同 | 受合规边界 + 许可约束，需先评估，不阻塞 P3a/b |

---

## 2. P3a 明细（首阶段，最划算）

### G5 · GUI 暴露疾病维度
- **目标**：GUI 新增"疾病→靶点"查询入口，复用 R 包已就绪的 `disease_serch` / `disease_target`。
- **改动文件**：
  - `gui-app/R/server.R`：`TCMNP_QUERY_FNS` 加 `disease_serch`（`query_vals` 加 disease 取值分支）；handler 已通用，无需改。
  - `gui-app/www/index.html`：query-mode 下拉加"疾病→靶点"项。
  - `gui-app/www/js/app.js`：`buildQueryArgs()` 加 disease 分支。
  - `gui-app/R/adapter.R`：如需别名/列契约，补 `.tcmnp_*` 适配。
- **依赖**：R 包 `disease_serch`/`disease_target` 已导出且 `load_all` 实证可用（Open Targets CC0 + 疾病本体表已接，**合规可发**）。
- **自检**：GUI 查疾病名→出表→导 CSV；与 R 包直跑结果一致；`R CMD check` 绿。
- **⚠️ go-decision**：疾病维度是否纳入首版分发？（建议是——数据合规且是网药标准一环。）

### G6 · 端到端富集
- **目标**：封装 `enrichGO`/`enricher`（GO + Reactome + **WikiPathways**）为 1 个 TCMNP 导出函数 + GUI handler，并把 `go_*`/`pathway_*` 绘图接进白名单。
- **⚠️ 许可决策（2026-10-17 已定）**：KEGG 离线富集在当前环境不可行（`KEGG.db` 在 Bioconductor 3.23 已下架；联网路径超时且最新 KEGG 许可禁分发）；MSigDB C2 CP:KEGG 同样带 Kanehisa "all rights reserved" 限制、且违背本项目 data.R 既定合规策略。故走**合规替代**：通路富集用 **Reactome（CC BY 4.0）+ WikiPathways（CC0 公共领域）**，彻底去掉 KEGG/MSigDB-KEGG。
- **R 层状态：✅ 已完成并实证 + R CMD check 闸门通过**（2026-10-17）：
  - `R/tcm_enrich.R`：新增 `tcm_enrich()`，`ont = c("GO","Reactome","WikiPathways")`，GO 走 `enrichGO`（org.Hs.eg.db 离线），Reactome/WikiPathways 走 `enricher` + 离线基因集；整段 `tryCatch` 降级。OrgDb 参数改用 `org.Hs.eg.db::org.Hs.eg.db` / `org.Mm.eg.db::org.Mm.eg.db`（`::` 引用，满足 R CMD check imported-from）。
  - `data/human_wikipathways.rda`（41891 行 / 991 通路）、`data/mu_wikipathways.rda`（11178 行 / 214 通路）：构建期 Entrez→SYMBOL，源 `data.wikipathways.org/20261010`（CC0）。
  - `data-raw/build_wikipathways.R`：可复现构建脚本。
  - **端到端实证**：`tcm_enrich(30 基因)` → go 2020 / reactome 734 / **wikipathways 522** 项，三绘图器串联 OK，全程离线。
  - `DESCRIPTION`：`clusterProfiler`/`DOSE`/`org.Hs.eg.db`/`org.Mm.eg.db` 已自 Suggests→Imports（进 exe 分发）。
  - **R CMD check（70f1zL）Status: OK（0 WARNING / 0 NOTE）**：初次 2 WARNING/2 NOTE 已清零——漏跑 `document()` 补生成 man/human_wikipathways.Rd、man/mu_wikipathways.Rd、tcm_enrich.Rd 默认更正；`globalVariables()` 增 4 个基因集名；OrgDb `::` 引用消 NOTE。
- **GUI 串联：✅ 已实现（2026-10-17，待 test-enrich.R 全绿 + boss 视觉验收）**：
  - `gui-app/R/server.R`：新增 `run_enrich` handler（payload=genes/species/ont/pvalueCutoff…，`tcm_enrich()` → `flatten_enrich_to_df()` → 表格）+ `run_enrich_plot` handler（按 `.TCMNP_ENRICH_PLOT` 注册表分发：首参名 `go.diff`/`data`、按 source 过滤、列契约校验、喂给 `go_barplot`/`go_dotplot`/`pathway_ccplot`/`pathway_cirplot`）。两条均 async + tryCatch 收敛为 ok=FALSE 返回值（绝不抛）。
  - `gui-app/R/adapter.R`：新增 `flatten_enrich_to_df()`（enrichResult 列表→统一表 + `source` 列）+ `.TCMNP_ENRICH_PLOT` 注册表（4 图的首参/来源/列契约）。
  - `gui-app/www/index.html`：新增「富集」导航 + `page-enrich`（基因输入/从查询取基因/物种/来源勾选/p 阈值/结果表/4 个绘图按钮/图预览/导出）。
  - `gui-app/www/js/app.js`：富集流程逻辑（`buildEnrichArgs`/`run_enrich_result`/`run_enrich_plot_result`/`genesFromLastQuery` 等），复用 `renderTable`/`beginReq`/`addResult`/`sendExportCsv/Png`。
  - `gui-app/tests/test-enrich.R`：后端自测（铁律28）——跑真实基因 → flatten → 4 图出 base64 png；并证伪「go 图误吃 pathway 数据」。
- **自检**：`tests/test-enrich.R` 全绿即证明"接入可用"；GUI 内交互（按钮/视觉）需 boss 在 RDesk 运行时肉眼验收。R CMD check（R 包层）已绿（70f1zL）。

### G1 · 拓扑模块识别
- **目标**：R 包补 betweenness/closeness 中心性 + MCODE/community 模块检测（核心靶点识别）；接 GUI。
- **改动文件**：
  - `R/` 新增拓扑模块（`man/*.Rd`）；`NAMESPACE` 导出。
  - `gui-app/` 白名单 + UI + handler + 列契约。
- **依赖**：igraph（或 base 图论），**无需新装重库**；需先确认 R 包层实现。
- **自检**：对 `tcm_net` 结果算核心靶点；GUI 可视化；`R CMD check` 绿。
- **⚠️ go-decision**：模块识别算法选型（MCODE vs community detection）。

---

## 3. P3b 明细（扩展维度）

| 编号 | 目标 | 改动量 | 备注 |
|------|------|------|------|
| **G7** | 处方接入 GUI | 中 | 先确认 `tcm_prescription` 意图（GAP §3.3：MVP 有意降级，注释过期）；补白名单+UI |
| **G8** | 多网络图暴露 | 低 | `tcm_sankey`/`tcm_alluvial`/`dot_*`/`ppi_plot`/`cir_plot`/`degree_plot`/`tf_*` 多数只需加白名单+最小 UI |
| **G2** | 分子对接 | 高 | `dock_plot` 仅画分。二选一：内置轻量对接（Autodock/Vina + PDB 抓取）或 GUI 定位"对接结果导入+可视化" |
| PPI | `ppi_plot` 接入 | 低 | STRING `ppi_data` 已打包，数据就绪 |
| TF | `tf_*` 接入 | 低 | R 包已就绪，仅接线 |

---

## 4. P3c 明细（合规受限，先评估）

| 编号 | 目标 | 约束 |
|------|------|------|
| **G3** | 毒理/安全性 (ADMET) | 全仓空白；受合规边界（DrugBank/TTD/药典不进分发产物）。需评估可用合规源 |
| **G4** | 成分协同/配伍增效 | 无 synergy 模块；论文级常需但较专，优先级低 |

---

## 5. 执行纪律 & 自检闸门

1. **每阶段开始先 `load_all` 实证 R 函数可用** —— 避免"探针跑错已安装副本"导致的假失败（铁律 52/76）。
2. **限定路径提交**：`git commit -- <paths>`，绝不用 `git add -A`；`NEWS.md` 若涉 Layer A 融合须加性共存（铁律 69）。
3. **每阶段结束跑 `R CMD check`**（tarball 检查，`Status: OK` 才算过；注意排除 `TCMNP-main/TCMNP.Rcheck/` 过期残留误判）。
4. **接 GUI 前先写取值级测试**（铁律 28：「无检查项≠通过」）—— 列契约、别名、空结果分支都要断言。

---

## 6. 待 boss 拍板的 go-decision 清单

- [ ] **启动 P3a？** 若启动，G5/G6/G1 三者顺序或并行？
- [ ] **疾病维度纳入首版分发？**（建议是，合规且为标准一环）
- [ ] **富集计算进分发产物？**（基因集 CC BY 4.0 可；需先 benchmark 耗时/内存）
- [ ] **`tcm_prescription` 取舍？**（回填注释 vs 补进 GUI）
- [ ] **分子对接定位？**（内置计算 vs 结果导入）
- [ ] P3a/b/c 是否分多个 PR 推进？（建议：P3a 一个 PR，P3b 一个 PR，P3c 评估后再定）

---

*生成时间：2026-10-16。对账数据来源于 `gui-app/www/index.html`、`gui-app/www/js/app.js`、`gui-app/R/server.R`、`gui-app/R/adapter.R` 实测。*
