# NEWS

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
