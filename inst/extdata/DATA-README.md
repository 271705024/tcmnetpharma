# TCMNP 数据版本说明（DATA-README）

## 数据版本命名规则（2026-08-22 定案）

```
TCMNP_<major>.<minor>.<patch>_<YYYY-MM-DD>
```

- 语义化版本（major.minor.patch）+ **获取日期**（必需，唯一标识版本）
- 例：`TCMNP_1.0.0_2026-08-22.parquet`
- 每次数据更新：patch+1（同日多次修订）或 minor+1（数据源版本变化）；major 用于结构破坏性变更
- 版本记录于每张表的 manifest（`data_version` 字段）

## 当前版本（待清洗产出后填充）

| 表 | 文件 | 数据版本 | 行数 | 说明 |
|---|---|---|---|---|
| standard_herb_mol_target | inst/extdata/standard_herb_mol_target.parquet | TCMNP_0.2.0_2026-08-22 | 待填 | 标准宽表（herb-mol-target + 来源 + 置信度 + ADME） |
| tcmsp_compat | inst/extdata/tcmsp_compat.parquet | TCMNP_0.2.0_2026-08-22 | 待填 | 9 列兼容表（喂现有函数） |
| raw_unmapped | inst/extdata/raw_unmapped.parquet | TCMNP_0.2.0_2026-08-22 | 待填 | 未映射/多余数据 |
| herb_pm | data/herb_pm.rda | TCMNP_0.2.0_2026-08-22 | 待填 | HERB 性味归经（6,892） |

## 来源与许可

| 数据源 | 版本/获取日期 | 许可 | 用途 |
|---|---|---|---|
| TCMSP | 2026-08-22 抓取 | ODbL | 基础关系 + ADME |
| HERB 2.0 | 2.0, 2024-11 更新（2026-08-22 下载） | 公开下载（引用 Fang et al. 2021） | 枢纽映射 + 性味归经 |
| BATMAN-TCM 2.0 | 2.0（2026-08-22 下载） | 公开下载（引用 Liu et al. 2024） | 关系主源（known/predicted） |
| ETCM2 | 2.0（2026-08-22 下载） | 注册使用（引用 Zhang et al. 2023） | 复方/成分补充 |
| TTD | 2026 v9.0（2026-08-22 下载） | 公开下载（引用 Zhang et al. 2026） | 靶点-疾病关联 |
| ChEMBL | 37（2026-05，2026-08-22 下载） | CC BY-SA 3.0 | 交叉验证 + ID 补充映射 |

## 清洗参数（S0-S6）

- 标准键：成分 InChIKey（主）+ PubChem CID（辅）；靶点 Entrez Gene ID；草药拼音+中文双键
- 关系主源：BATMAN；未映射数据入 raw_unmapped（保留原始 ID）
- 清洗脚本：data-raw/cleaning/{01_parse,02_map,03_merge,05_qc}.py（可复现）
- 完整规格：docs/data-cleaning-spec-2026-08-22.md / docs/data-cleaning-qc-plan-2026-08-22.md
