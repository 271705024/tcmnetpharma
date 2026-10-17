# TCMNP 网药流程完整性差距分析（2026-10-16）

> 方法：核 `TCMNP-main/NAMESPACE` 导出 + `R/*.R` 实现 + `R/data.R` 打包数据集
> + `gui-app/R/server.R` / `adapter.R` 派发白名单。R 包层为逐导出核实；GUI 层为
> 白名单 + handler 核实（非逐前端按钮扫描，前端能力被后端 handler 死限定）。
> Boss 视角：本项目最终交付是面向**非技术用户**的 exe，故"GUI 暴露层"才是用户
> 真实感知的完整性，权重高于 R 包层。

---

## 1. 标准中药网络药理学流程（参照框架）

| # | 阶段 | 典型动作 |
|---|------|----------|
| 1 | 成分收集/筛选 | 活性成分 + ADME（OB/DL）过滤 |
| 2 | 靶点预测/收集 | 成分→靶点（数据库 + 预测） |
| 3 | 疾病-靶点关联 | 疾病→靶点/基因（OMIM/DisGeNET/Open Targets） |
| 4 | 网络构建 | C-T / C-T-D / PPI 网络 |
| 5 | 拓扑分析 | degree / betweenness / 模块识别（核心靶点） |
| 6 | 富集分析 | GO / KEGG / Reactome / DO |
| 7 | 通路可视化 | KEGG 通路图 |
| 8 | 分子对接 | 配体预处理 + 对接打分（Autodock/Vina） |
| 9 | 转录因子 | TF-靶点 |
| 10 | 处方分析 | 多味药/配伍超几何检验 |
| 11 | 毒理/安全性 | ADMET / 毒性预测 |
| 12 | 验证 | 实验/文献（本质外部，工具不内置） |

---

## 2. R 包层覆盖矩阵（~39 导出函数）

| 阶段 | 状态 | 证据 |
|------|------|------|
| 1 成分筛选 | ✓ | `molecule_serch` `tcm_compound` `tcm_comp` |
| 2 靶点 | ✓ | `herb_target` `target_herb` `target_serch` |
| 3 疾病-靶点 | ✓ | `disease_serch` `disease_target` + Open Targets + 疾病本体表 |
| 4 网络构建 | ✓ | `tcm_net` `ppi_plot`(STRING `ppi_data`) `sankey`/`alluvial`/`dot`/`venn` |
| 5 拓扑 | △ | 仅 `degree_plot` `cir_plot`（**缺 betweenness/closeness + 模块识别**） |
| 6 富集 | ✓(强) | `go_*` `pathway_*` + 打包 `human_reactome`/`mu_reactome`(CC BY 4.0) + clusterProfiler |
| 7 通路可视化 | ✓ | `pathway_ccplot` `pathway_cirplot` |
| 8 分子对接 | ✗(仅可视化) | `dock_plot` 文档："scoring heatmap"，输入为对接结果 df，**不跑对接** |
| 9 TF | ✓(额外) | `tf_filter` `tf_cirplot` |
| 10 处方 | ✓(额外) | `tcm_prescription` `tcm_pre_plot` + 超几何检验 |
| 11 毒理/安全 | ✗ | 全仓 grep 无 tox/ADMET/SIDER/CTD |
| 12 验证 | N/A | 外部 |

**R 包层结论**：数据/查询/网络/富集/通路/处方/TF 已齐，**不是"缺大模块"**。
真缺口：G1 拓扑模块识别、G2 对接计算、G3 毒理、G4 协同（详见 §4）。

---

## 3. GUI 暴露层映射（本次扫描核心发现）

后端 `init_handlers` 仅注册 7 类消息处理器；查询/绘图各有一份**白名单**，
前端只能调用白名单内函数。结果：

### 3.1 GUI 实际暴露的函数（共 8 个）

- **查询**（白名单 `TCMNP_QUERY_FNS`, server.R:31）：
  `herb_target` · `molecule_serch` · `target_herb` · `target_serch`
- **绘图**（白名单 `TCMNP_SAMPLE_MAP`, server.R:33）：
  `tcm_net` · `bar_plot` · `bubble_plot` · `venn_plot`

### 3.2 未暴露的导出函数（约 31 个，R 可用、GUI 不可达）

| 类别 | 未暴露函数 | 用户影响 |
|------|-----------|----------|
| 疾病维度 | `disease_serch` `disease_target` | **GUI 完全没有疾病-靶点分析** |
| 富集绘图 | `go_barplot` `go_cir` `go_dotplot` `go_lollipop` `pathway_ccplot` `pathway_cirplot` | GO/KEGG/Reactome 图在 GUI 不可画 |
| 富集计算 | （无 TCMNP 封装；依赖 clusterProfiler `enrichGO/enrichKEGG`） | **端到端富集在 GUI 跑不了**（仅能画图、且画图函数也未暴露） |
| 拓扑 | `degree_plot` `cir_plot` | 无拓扑图 |
| PPI | `ppi_plot` | 无 PPI 网络图 |
| TF | `tf_filter` `tf_cirplot` | 无 TF 分析 |
| 对接 | `dock_plot` | 无对接可视化 |
| 处方 | `tcm_prescription` `tcm_pre_plot` | 无处方分析（`tcm_prescription` 见 §3.3 不一致） |
| 其他网络图 | `tcm_sankey` `tcm_sankey_dot` `tcm_alluvial` `tcm_alluvial_dot` `alluvial_dot` `dot_plot` `dot_sankey` `dot_sankey2` `venn_net` | 仅 `tcm_net` 一种网络图可用 |
| 成分信息 | `tcm_comp` `tcm_compound` `etcm` | `etcm` 被显式拒绝（需上传文件，MVP 未开放）；另两者未接 |
| Venn | `venn_net` `venn_result` | 仅 `venn_plot` |

### 3.3 不一致点（已澄清：非用户可见不一致）

- **`tcm_prescription`**：MVP 设计注释（server.R:19-23）把它列为查询函数，
  但当前 `TCMNP_QUERY_FNS` 白名单**不含**它。本次扫前端 `www/index.html` 的
  query-mode 下拉**也已无该项** → 前端 / 后端**一致（都未暴露）**，属 MVP 有意降级，
  非临时不一致。仅 `server.R:19-23` 注释过期未回填。
  **处置**：要么把该注释改为"首版仅 4 查询"；要么若处方是卖点，补白名单+UI（见 P3 清单 G7）。

### 3.4 关键结构约束（adapter.R）

- 绘图函数有**列契约**：`tcm_net` 需 `herb/molecule/target`；
  `bar_plot`/`bubble_plot` 需富集结果框（`Description/Count/p.adjust` 等）。
- 注释（adapter.R:97-106）明确：查询结果**不满足**任何富集契约 →
  GUI 不能把"查询→富集→绘图"直接串起来。即使暴露了 `go_*`，也需先有
  富集结果框，而该结果框在 GUI 里无处产生（见 §3.2 富集计算缺失）。

**GUI 层结论**：对非技术用户，有效工作流 ≈ "4 种关键词查询 + 网络/柱/泡/Venn 图"。
疾病分析、富集（计算+图）、PPI、拓扑、TF、对接、处方、绝大多数网络图 **全部不可达**。

### 3.5 前端 UI 实测对账（2026-10-16 扫 `www/`；用户实际可见功能）

方法：Read `www/index.html`（UI 结构）+ `www/js/app.js`（调用逻辑），对照后端
`TCMNP_QUERY_FNS` / `TCMNP_SAMPLE_MAP` 白名单与 7 类 handler。

**三层对齐结论**：**前端可见 = 后端白名单 = 8 函数**，无"前端暴露但后端不支持"的半对齐死链。

| 层 | 覆盖 | 缺口 |
|----|------|------|
| R 包导出 | ~39 函数 | G1 拓扑模块 / G2 对接计算 / G3 毒理 / G4 协同（§4） |
| GUI 后端白名单 | 8 函数 | 疾病 / 富集计算 / go 图 / PPI / 拓扑 / TF / 对接 / 处方 / 多数网络图 |
| GUI 前端可见 | 8 函数（与白名单 100% 一致） | 同上行 |

**用户实际可见清单**（index.html 下拉 + app.js 逻辑）：
- 导航 4 tab：查询 / 绘图 / 结果 / 关于
- 查询入口（下拉 4 项）：药→成分靶点 · 成分→关联 · 靶点→关联 · 靶点→反查药
- 绘图入口（下拉 4 项）：网络图 · 富集柱状图 · 富集气泡图 · 韦恩图
- 导出：查询 CSV / 图 PNG / 结果页统一下载

**两处必须澄清（修正 §3.2/§3.3 初判）**：
1. **"富集柱状图/气泡图"实为 KEGG 富集可视化**：前端 label 写"富集"，但
   `TCMNP_SAMPLE_MAP[bar_plot]="kegg.filter"`、`[bubble_plot]="kegg.filter"`
   → 消费 KEGG 富集结果框；R 包 `go_barplot`/`go_bubbleplot`（GO 专图）未接 GUI。
   **且 GUI 无"运行富集"入口** → 两图只能画内置示例 `kegg.filter`，不能从查询结果跑富集。
2. **`tcm_prescription` 降级已前后端一致**（见 §3.3），非不一致。

**用户真实闭环**：4 种查询看表导 CSV + 4 种图（富集图仅能用内置示例）看 PNG 导图。
疾病分析 / 端到端富集 / PPI / 拓扑 / TF / 对接 / 处方 / 绝大多数网络图 **全部不可达**。

---

## 4. 优先级缺口（合并两层）

### P0（用户可见、且实现成本中低，最该先补）
- **G5 GUI 暴露疾病维度**：`disease_serch`/`disease_target` 已在 R 包就绪，
  只需补进白名单 + UI。这是"网药流程"的标准一环，且数据已接（Open Targets）。
- **G6 端到端富集**：封装 `enrichGO`/`enrichKEGG`/`enricher`（用已打包的
  Reactome/GO 基因集）为 1 个 TCMNP 导出函数 + GUI handler，并把 `go_*`/`pathway_*`
  绘图接进白名单。否则"富集"在 GUI 是空壳。
- **G1 拓扑模块识别**（R 包层也缺）：补 betweenness/closeness + MCODE/community，
  并接 GUI。论文"核心网络"标准动作。

### P1（标准论文常备、成本中）
- **G2 分子对接**：`dock_plot` 仅画分。要么内置轻量对接（Autodock/Vina + PDB 抓取），
  要么 GUI 明确定位为"对接结果导入 + 可视化"。
- **G7 处方分析接入 GUI**：确认 `tcm_prescription` 意图（§3.3），补白名单 + UI。
- **G8 其余网络图暴露**：`tcm_sankey`/`tcm_alluvial`/`dot_*`/`ppi_plot`/`cir_plot`
  /`degree_plot`/`tf_*` 多数只需加白名单 + 最小 UI 即可开放。

### P2（增值、受许可/成本约束）
- **G3 毒理/安全性**：全仓空白，受合规边界（DrugBank/TTD/药典不进分发产物）影响。
- **G4 成分协同/配伍增效**：无 synergy 模块。

---

## 5. 合规边界（非 bug，是设计上限）

`NEWS.md` 明示：DrugBank / TTD / 药典 **不进对外分发产物**。故疾病-靶点/适应症
覆盖是**故意收敛**，不是缺失。扩疾病维度须走合规源（Open Targets CC0 / SymMap 已接）。

---

## 6. 建议下一步（待 boss 拍板）

1. **最划算三连**：G5（疾病进 GUI）+ G6（端到端富集）+ G1（拓扑/模块）→ 直接补齐
   "查询→靶点→疾病→网络→富集→核心靶点"完整闭环，且数据/R 函数基本就绪。
2. **澄清 G7**：`tcm_prescription` 到底要不要进 GUI（白名单注释矛盾）。
3. **可选**：扫 `gui-app/www/` 前端按钮，与后端白名单对账，给出"用户实际可见功能清单"。
4. 本文档可作为 P3（GUI 功能补全）立项依据；与 R 包层 G2/G3/G4 分两个 PR 推进。
