# NEWS

## 未发布（2026-10-23）

### 修复：三合一分面图（`grouped`）的色阶改为每本体独立

- **问题**（boss 2026-10-23 截图反馈）：原 `grouped` 用 `facet_grid` + **单个**
  `scale_fill_gradientn`，色阶整图共用。实测三个本体的 `p.adjust` 量级差 20+ 个
  数量级（BP 1.3e-35 / CC 1.1e-9 / MF 6.6e-14）→ BP 全挤深蓝端、CC 挤红端、
  **各分面内部的条目差异完全看不出来**（MF 一组内部只差 1.4 个数量级，几乎同色）
- **改法**：改为 `patchwork` 上下拼三张独立图，每段有自己的 `scale` 与自己的
  色条，色条上显示**该本体真实的 `p.adjust`**。宽度不增（三行同宽，色条各在
  自己那行右侧垂直堆叠）
- **为什么不用分面独立标度**：ggplot2 原生不支持按分面切 `fill` 标度；
  `ggnewscale` 三层实测图例与分面错位、颜色不单调；`ggh4x::facetted_pos_scales`
  只支持 x/y 不支持 fill
- ⚠️ **返回类型变化**：`grouped` 现在返回 **patchwork** 对象（`inherits("ggplot")`
  仍为 TRUE，GUI 抓图链路不受影响），但它**没有单一 `$data`** —— `p$data` 只拿到
  第一段（BP）的条目。外部代码若用 `nrow(p$data)` 统计条数请改用各子图汇总
  （包内测试已抽到 `tests/testthat/helper-plots.R` 的 `items_of()`）
- 段标题改为 BP / CC / MF 本体名；用户传的 `title` 不再丢失，改走
  `patchwork::plot_annotation()` 作为整体标题（`title = NULL` 时不加）
- 新增 `patchwork` 依赖

### GUI：绘图页下线「富集柱状图」（`bar_plot`）

- 与富集页的 `go_barplot` 数据同源（富集结果会 `pushDataset` 进绘图页），而
  `go_barplot` 能力更强（本体三色 / BP-CC-MF 分段 / 文献横条·显著渐变 /
  GeneRatio / 配色体系），`bar_plot` 都不具备 → 留着只会让人选错
- **包 API 保留**（`R/bar_plot.R` 未删），只是绘图页不再暴露
- 绘图页保留 `cir_plot`（富集环形条形图，无对应专页图）

## 未发布（2026-10-22 批次，与上节同属一个未发布版本）

### 出图风格预设：GO 富集文献形制 + 通用期刊/演示风

- **`go_barplot(style = )`** 新增 6 种预设（**只改视觉，条目与统计量完全一致**，见 `tests/testthat/test-style-presets.R`）：
  - `classic`（新增）文献最常见形制：横条长度 = 基因数 `Count`，按本体（BP/CC/MF）三色，条端标注 Count
  - `grouped`（新增）"三合一图"：BP/CC/MF 分面，长度 = `Count`，颜色 = `p.adjust` 渐变
  - `significance`（新增）长度与颜色均为 `-log10(p.adjust)`，单图按显著性排序
  - `journal` / `presentation`（新增）通用期刊风 / 演示风
  - `default` 保持原有输出不变
- **`go_dotplot(style = )`** 新增 `genratio`（横轴 `GeneRatio`，clusterProfiler 经典口径，文献绝大多数气泡图用此口径）、`journal`、`presentation`；`default` 仍为 Rich Factor。GeneRatio 缺失时自动退回 Rich Factor 并给出 message（不静默丢点）
- **`bar_plot` / `degree_plot` / `tcm_sankey`** 新增 `style = c("default","journal","presentation")`，统一走 `R/style-presets.R` 的 `.tcm_style_apply()`（风格单一事实源）
- **GUI**：富集可视化新增「出图风格」下拉（文献横条 / 三合一分面 / 显著性渐变 / GeneRatio 气泡 / 期刊风 / 演示风）；绘图页的风格下拉改为**按图型动态重建**（此前只有 4 个网络风选项且提示"仅网络类图生效"）
- **契约**：`plot_contract` / `enrich_plot_contract` 新增 `styles` 字段（风格白名单单一事实源），前后端一致由 `test-style-presets.R` 双向断言
- **防御**：非法风格值在后端被 `.sanitize_style()` 丢弃并记 `style_dropped` 日志，不再因 `match.arg` 报错中断出图

### 配色体系：期刊色板 / 色盲友好 / ColorBrewer（同一版本）

- **新增 `R/palettes.R`** 作为配色单一事实源，登记 **13 套色板**，每套都带「出处」字段（`source`）与类型（`kind` = qual/seq/div）：
  - 期刊/机构色板（取自 **ggsci** 收录的同名色板）：`npg`（Nature 系列）、`aaas`（Science/AAAS）、`lancet`（Lancet Oncology）、`jama`、`nejm`、`jco`
  - 色盲友好：`okabe` = **Okabe & Ito 2008 Color Universal Design** 全 8 色（Wong 2011, *Nat Methods* 8:441, doi:10.1038/nmeth.1618）
  - 通用分类：`set2`、`tableau`
  - 连续/发散（取自 **ColorBrewer 2.0** / viridisLite）：`viridis`（色盲友好且可灰度还原）、`rdbu`、`spectral`、`ylgnbu`
- **新接口**：`tcm_palette(name, n, type)` 取色（超出原生颜色数自动插值并提示）；`tcm_palette_names(names_only = FALSE)` 返回带 label/short/source/kind 的目录
- **接入 `palette = ` 参数的 7 个绘图函数**：`go_barplot`、`go_dotplot`、`bar_plot`、`degree_plot`、`tcm_sankey`、`tcm_net`、`ppi_plot`。缺省 `NULL` = **行为与旧版完全一致**
- **修掉一处非确定性**：`tcm_sankey` 原先用 `sample(mycol, ...)` 取色 → **同一数据两次出图颜色不同**；现改为按节点数确定性取色（`test-palettes.R` 有回归锚点）
- **GUI**：绘图页与富集可视化各新增「配色体系」下拉，按分类/连续/发散分组显示，选项 label 用中文名、`title` 显示色值出处；空值 = 默认配色
- **契约**：`plot_contract` / `enrich_plot_contract` 新增 `palettes` 字段；`app_info` 新增 `palette_catalog`（name/label/short/source/kind），前端据此渲染分组
- **三方一致性由测试守**：前端常量 ↔ 契约 ↔ 包内色板表（`gui-app/tests/test-style-presets.R` F 段），并对「有 palette 形参却没登记进契约」做**导出函数反查**（否则前端下拉置灰、用户点不到 = 等于没接）
- **防御**：非法配色值由 `.sanitize_palette()` 丢弃并记 `palette_dropped` 日志，不把「配色不存在」升级成「绘图失败」
- `DESCRIPTION` 新增 `viridisLite` 依赖

### 文档澄清

- **`go_barplot(top = )` 的口径**：`top` 是「**每个本体**各取 top 条」，不是全局 top。因为入选前先按 `ONTOLOGY`（BP/CC/MF）分组再切片，`top = 8` 最多给出 `8 × 3 = 24` 条（某本体显著条目不足时更少）。原文档只写 "the number of categories to show"，易被读成全局条数 → 已补明，并加断言锁定（`test-style-presets.R`），防止未来被"顺手改成全局 top"。


## TCMNP 0.2.0（数据修订 2026-10-21，数据版本 TCMNP_1.3.0_2026-10-21）

### CID 桥接假阴性修复（2026-10-21）
详见 `docs/xuanfuhua-uc-precheck/CID桥接假阴性_缺陷归属分析.md` 及其补记、R3 决策文档。

- **R1｜InChIKey 规范化桥接**：`02d_layer_a_align.py` 的 `build_mol_bridge()` 增加 InChIKey 分支（新增 `match_by_inkey` / `layer_a_inkey` 列），不再仅靠裸 CID 等值匹配
- **R2｜补映射表扩充**：`cid_inkey.parquet` 34,570 → **41,100** 行（目标集由 35,031 扩为 **43,170** 多源并集）；BATMAN 全集 35,336 CID 缺口 766 → **555**
- **R2-b｜known 边补映射（报告未发现）**：`03_merge.py` 对 known 边按官方 CID→InChIKey 补齐 → known 边 InChIKey 13,137 → **16,801**；`UNMAPPED_batman_known` 21,269 → **1,068**（救回 20,201 行）；`raw_unmapped` 211,830 → **191,629**
- **R3｜分子实体别名通道**（新增 `mol_entity_alias.parquet`，**39,388** 行）：BATMAN 与 HERB 对同一分子使用不同立体形式 InChIKey（前 14 位连接性层相同）→ 此前按名查不到。新增 `molecule_serch(..., resolve_alias = TRUE)`（**默认开启**）：未逐字命中的名字经别名表解析为连接性层 `entity_key`，查询扩展至该实体的**全部**立体异构体
  - 效果：`molecule_serch("Britannilactone")` 0 → **67** 行；`molecule_serch("Inulicin")` 0 → **51** 行
  - **向后兼容**：仅在原有路径**完全查不到**时启用；原路径可查的名字不被改写（`quercetin` 仍为 106,020 行）
  - **数据侧零改动**：`standard` / `tcm_network_compat` 行数与列契约**完全不变**（16,269,134 / 16,202,124）
  - ⚠️ 曾评估「把 HERB 规范名回填进 `mol_name`」方案并**实测否决**：会覆盖 7,912,535 行既有名称、compat 塌缩 94,705 行、可检索实体反降 2,420
- **方法学教训**：跨库分子桥接一律走 **InChIKey**（或前 14 位连接性层 + 相似度），**禁止裸 CID 等值匹配**

### ETCM2 未消费数据接入（2026-10-21）
- **ETCM2 4 张已解析未消费表接入**（`02e_etcm2_align.py` 非破坏性对齐；粒度为二元组，**不进 standard 三元组**，行数守恒 16,269,155 不变）：
  - `etcm2_herb_property.parquet`（2,079）药材性味归经；herb_dict 补空缺 Properties 378 / Meridians 367 / **新增 Flavor 列 1,511**（旧非空值 0 覆盖）；命中 92.2%
  - `etcm2_mol_adme.parquet`（38,298）成分 QED/FDAMDD/MW；成分名精确匹配 mol_dict 40.0%（同名多 InChIKey 标 ambiguous 不硬配）
  - `etcm2_herb_target.parquet`（2,836）药材→靶点二元组（CamelCase 拼音归一）；靶点命中 97.5%
  - `etcm2_disease_gene.parquet`（24,106）疾病→基因二元组；基因命中 99.9%
- **TTD 处置**：不进对外分发（All rights reserved）；其 MOA/研发阶段增值改由 **ChEMBL `drug_mechanism`/`drug_indication`** 提供（开放许可）
- **文档纠错**：修正 `DATA-README.md` 中 TTD 版本/许可/用途三项失实、ETCM2 用途不全；`DESCRIPTION`/`TCMNP-package.Rd` 源列表移除 TTD、补 ChEMBL
- 数据版本统一 `_2026-10-12` → `_2026-10-21`（`verify_data_readme` 版本比对稳健化）

## TCMNP 0.2.0（数据修订 2026-10-14，数据版本 TCMNP_1.3.0_2026-10-21）

### Layer A 一次源融合（去依赖重建）
- 一次性独立源 **Open Targets 25.06（CC0, EBI FTP）** 与 **SymMap V2.0** 全量接入；关系层 herb-mol-target 由 HERB 2.0 / BATMAN 2.0（已冻结、开放许可）兜底，形成「半一次依赖」骨架（合规底线：DrugBank / TTD / 药典不进对外分发产物）
- `standard_herb_mol_target` 宽表新增 2 列一次源交叉引用：`layer_a_latin`（SymMap 草药拉丁名，53.3% 行命中）、`layer_a_hit_id`（SymMap/HIT 实验验证靶点标记，30.7% 行命中）
- 新增独立参考表 `layer_a_disease_ontology_map.parquet`（7,157 行，MeSH/UMLS 本体 ID），供下游疾病-靶点/适应症模块消费
- 全流程产物指纹接入 `run_pipeline` 状态持久化（`pipeline_state.json`）；06_export 导出纳入该表并经 `verify_data_readme()` 守护
- 详见 `docs/data-source-registry-2026-10-12.md`、`docs/data-source-independence-roadmap-2026-10-12.md`

### 疾病维度（2026-10-15）
- 新增导出函数 **`disease_serch()`**：按疾病名 / MeSH / UMLS / SymMap 疾病 ID 查疾病本体（读 `disease_main.parquet`，14,434 行；多值本体 ID 走**边界匹配**，默认**精确**）
- 新增导出函数 **`disease_target()`**：由疾病查到**关联靶点**（读 `disease_target.parquet`，3,134,397 行 = OpenTargets 25.06 `association_overall_direct` 经 MeSH/UMLS 桥接 SymMap 疾病）；保留 OpenTargets 原始 `score`（0–1）与 `evidence_count`，`min_score` 默认 0 **不设隐式阈值**
- 新增数据表 `disease_main.parquet` / `disease_target.parquet`；`layer_a_latin` 拉丁名**无损归一化**（内嵌换行→空格、全角→半角、罗马数字 `Ⅴ`→`V`；共修正 237,396 行）
- 详见 `docs/analysis-module-wiring-2026-10-14.md` §7–§9

### 通路富集：WikiPathways（CC0）替代 KEGG，KEGG 改为联网增强（2026-10-17）
- 新增离线基因集 **`human_wikipathways` / `mu_wikipathways`**（人 991 / 鼠 214 条通路，来源 data.wikipathways.org `20261010`，**CC0 公有领域**，可随软件分发）
- 新增 **`tcm_enrich()`**：GO / Reactome / WikiPathways 端到端富集，默认三者全跑，**全部离线可用**
- **KEGG 不再随包分发**（学术自用免费，但随软件再分发需另行授权，见 `docs/licensing-kegg-go-2026-10-19.md`）；
  需要时联网运行 `clusterProfiler::enrichKEGG()`，返回的 `enrichResult` 可直接交给本包的可视化函数
- 说明见 `inst/extdata/DATA-README.md` §可再分发基因集（含「关于通路富集」一节）

### STRING PPI 接入：`tcm_ppi()`（2026-10-20）
- 新增导出函数 **`tcm_ppi(targets, min_score = 700)`**：把**靶点列表**（交集 / 查询结果）
  转成 **PPI 边表**（`node1` / `node2` / `combined_score`），产物可直接喂 `ppi_plot()`。
  存在的理由：PPI 需要**配对**，而靶点列表只说"有哪些基因" —— 这是两种不同的数据形状。
- 新增数据表 **`inst/extdata/string_ppi_human.parquet`**：STRING v12.0 人类
  **physical subnetwork**（有实验依据的物理互作），`combined_score >= 700`（high confidence），
  蛋白 ID 已映射为 HGNC symbol。
  - 为什么用 physical 而非 full network：full 含共表达 / 文本挖掘等**预测关联**（79 MB），
    physical 只有 8.5 MB 且语义更准 —— 讲"蛋白互作"就该用物理证据。
- GUI：交集页新增「生成 PPI 子网」按钮；产物入数据集总线（shape = `ppi`），
  绘图页可直接选「PPI 蛋白互作网络」。
- **许可**：STRING 数据为 **CC BY 4.0**，分发需署名。本包分发的是**经筛选与符号映射的
  派生子集**（非原始文件）。完整声明与引用见 `docs/data-license-string-2026-10-20.md`，
  GUI「关于」页亦有说明。

### 药物 × 疾病 交集靶点 + 富集阶段进度（2026-10-19）
- 新增导出函数 **`tcm_intersect_target()`**：网络药理学主线第一步 —— 取「药物靶点」与「疾病靶点」的**交集**。
  两侧都已是 gene SYMBOL 口径（药侧 `target` / 病侧 `target_symbol`），**不需要额外映射表**。
  返回每个交集靶点的药侧支撑分子数（`n_molecule`）与病侧最强证据（`disease_score` / `evidence_count` / `n_ot_disease`）；
  聚合取 **max** 而非 sum（sum 会把「多个疾病各一条弱证据」抬成强证据），来源数另用 `n_ot_disease` 如实记录；
  **0 结果也返回同结构 0 行**，下游无需特判 `NULL`。
  - 实测：麻黄 × Asthma → **1,115 个交集靶点**（`ADORA1` 居首，score 0.728），约 3 秒
  - ⚠️ 疾病靶点来自 Open Targets（人类），故交集为人类 gene symbol；小鼠数据不适用
- **`tcm_enrich()` 新增可选 `progress` 回调**（`function(pct, message)`）：在 GO / Reactome / WikiPathways
  各阶段回报 25 → 50 → 60 → 78 → 82 → 95 → 100。包本身**不依赖 GUI**（默认 `NULL`），
  回调抛错不影响主流程。
  - 背景：富集耗时几乎全是**固定开销**（实测 8 / 30 / 100 / 200 个基因均为 20–24 秒，
    主要花在加载 `clusterProfiler`/`DOSE` + 基因集），没有阶段反馈时用户无法区分「在算」与「卡死」

## TCMNP 0.2.0（数据修订 2026-09-28）

### 数据质量修复（B1/B2/B3/B5）
- **B1 名称守卫**：`mol_name`/`iupac_name` 不再接受 `CID <数字>` 占位符与 CAS 号（原致 11,649 行成分名污染）；守卫收敛到 `cleaning/common.py`，构建器与回填表共用同一实现
- **B2 靶点符号补全**：BATMAN predicted 引用的 9,491 个 Entrez 中 1,090 个不在 HERB 靶点枢纽 → 经 `org.Hs.eg.db` 补全为 HGNC 符号（1,090/1,090，100%），消除 634,824 行空靶点
- **B3 compat 判重**：按对外暴露的 10 列判重（-67,010 行）
- **B5 物种过滤**：HERB 靶点枢纽剔除非人条目（187 个小鼠 + 4 个 741158），清除 398 行；非人基因符号命中 44 种
- **行数基准收敛**：三表行数收敛到单一声明点 `data-raw/cleaning/row_baseline.json`（原先在 `03_merge.py` 与 `tools/build_app.py` 各写死一份，改一处漏一处）
- 数据版本升至 `TCMNP_1.1.1_2026-09-28`；行数 16,269,134 / 16,202,124 / 4,988,060
- 详见 `docs/defect-fix-b1b2b3-2026-09-28.md`

### 新行为：查询口径「尽量全」（默认变更）
- `herb_target()` / `target_herb()` **默认保留源侧无 OB/DL 的行**，新增 `adme` 列标注（`ok` / `no_adme`）；原先这些行被阈值静默丢弃（compat 表约 80.33% 行属此类）
- 传 `include_missing = FALSE` 可回退旧口径
- `tcm_net()` 改为按**列名**取三元组（原先按列位置，遇查询结果的第 4 列 `molecule_id` 会错位报错）

## TCMNP 0.2.0（数据修订 2026-09-15）

### 数据质量修复（L3）
- `iupac_name`/`mol_name` 中的 CAS 号迁入独立 `cas` 列（`standard` 表新增第 23 列；`compat` 表扩为 10 列，新增 `CAS`）
- 分子名经 PubChem 回填（889 个 CID），`mol_name` 空值清零
- `mapping_status` 列语义修复（原先误写 InChIKey 值）
- `herb_cn_source` 溯源加守卫；数据版本升至 `TCMNP_1.1.0_2026-09-15`
- 详见 `docs/l3-data-defect-fix-report-2026-09-15.md`

## TCMNP 0.2.0 (2026-08-22)

### 数据（重大更新：单一 TCMSP → 多源整合）
- 新增 HERB 2.0（6,892 药/44,595 成分/15,515 靶点）与 BATMAN-TCM 2.0（成分-靶点 known/predicted）关系数据
- 新增 ETCM2（48,442 方/38,298 成分）、TTD（靶点-疾病关联）、ChEMBL 37（交叉验证）
- `herb_pm` 更新：药典 2020 版 → HERB 性味归经（409 → 6,892 味）
- 数据存储改为 Parquet 惰性加载（`inst/extdata/`），小表保留 rda
- 数据版本命名：`TCMNP_<semver>_<YYYY-MM-DD>`（详见 DATA-README）

### 工程化
- DESCRIPTION 修正（R>=4.1、License 规范、移除过时字段）
- 清理废弃 API（dplyr `sample_n/top_n/mutate_at` → `slice_*`/`across`）
- 新增契约测试 40 断言（testthat 3e）
- R CMD check：Status OK

## TCMNP 0.1.0 (2023)
- 初版：TCMSP 数据快照 + 38 个网络药理学可视化/查询函数
