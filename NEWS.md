# NEWS

## TCMNP 0.2.0（数据修订 2026-10-14，数据版本 TCMNP_1.3.0_2026-10-12）

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
