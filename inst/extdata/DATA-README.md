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
- 清洗脚本：data-raw/cleaning/{01_parse,02_map,03_merge,05_qc,05b_verify}.py（可复现）
- 完整规格：docs/data-cleaning-spec-2026-08-22.md / docs/data-cleaning-qc-plan-2026-08-22.md

## 已知数据特征（源数据约定，非错误）

| 特征 | 说明 |
|---|---|
| **herb_cn/herb_en 空缺 = BATMAN_ONLY** | 宽表 8.93% 行（1,365 味）为 BATMAN 独有草药（HERB/TCMSP/ETCM2 三库均未收录的生僻种/拉丁学名音译/菌藻类），中英文同步为空；常用药全部有中英文名。非合并缺陷，行数守恒已核验（wide ⊂ herb_browse，无凭空） |
| **`'NA'` 字符串占位缺失** | HERB 源数据用字符串 `'NA'`（非 NaN/空）表示缺失，常见于 TCMSP_id/PubChem_id/InChIKey 列；查询时需统一按缺失处理 |
| **BATMAN ChEBI 化学本体节点** | predicted 靶点列含约 0.26% 非基因条目（如 `acid 128`），为 ChEBI 本体注释；这些行 target_symbol_std 为空（靶点侧未映射），分析时应过滤 |
| **HERB 靶点符号大小写不统一** | 同一基因可能并存 `Hand2`/`HAND2` 两种写法；宽表按 Entrez 主键合并（正确），符号写法为显示层差异；HGNC 规范符号本身含混合大小写（如 `C12orf57`），不可简单 upper |
