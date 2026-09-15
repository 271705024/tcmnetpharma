# NEWS

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
