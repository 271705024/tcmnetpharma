# NEWS

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
