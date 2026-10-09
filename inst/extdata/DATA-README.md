# TCMNP 数据版本说明（DATA-README）

## 数据版本命名规则（2026-08-22 定案）

```
TCMNP_<major>.<minor>.<patch>_<YYYY-MM-DD>
```

- 语义化版本（major.minor.patch）+ **获取日期**（必需，唯一标识版本）
- 例：`TCMNP_1.0.0_2026-08-22.parquet`
- 每次数据更新：patch+1（同日多次修订）或 minor+1（数据源版本变化）；major 用于结构破坏性变更
- 版本记录于每张表的 manifest（`data_version` 字段）

## 当前版本

| 表 | 文件 | 数据版本 | 行数 | 说明 |
|---|---|---|---|---|
| standard_herb_mol_target | inst/extdata/standard_herb_mol_target.parquet | TCMNP_1.2.0_2026-10-09 | 16,269,155 | 标准宽表（herb-mol-target + 来源 + 置信度 + ADME；含 `cas` 列） |
| tcm_network_compat | inst/extdata/tcm_network_compat.parquet | TCMNP_1.2.0_2026-10-09 | 16,202,145 | 10 列兼容表（喂现有函数；DuckDB 惰性查询；含 `CAS` 列；已按 10 列判重） |
| raw_unmapped | inst/extdata/raw_unmapped.parquet | TCMNP_1.2.0_2026-10-09 | 211,830 | 未映射/多余数据 |
| herb_pm | data/herb_pm.rda | TCMNP_0.2.0_2026-08-22 | 6,860 | HERB 性味归经（6,892 去空后） |

> **数据版本 1.2.0（2026-10-09）修复内容**：**成分结构标识换源** —— 原「PubChem CID→InChIKey」
> 补映射误用 ChEMBL 的 `PUBCHEM_BIOASSAY`（其 `src_compound_id` 是 PubChem **SID 而非 CID**），
> 导致 107 个 CID / 约 9,338 行的 InChIKey 与 CID 错配；现改用 PubChem 官方
> `CID-InChI-Key.gz`（7.37 GB，2026-09-27，md5 校验通过）。效果：宽表 **4,776,230 行由
> UNMAPPED→MAPPED**（`raw_unmapped` 4,988,060 → 211,830，未映射对降至 27,631）；
> 去重撞车修复致 +21 行。另修正 QC 真值校验的宽匹配假通过（改精确匹配优先）。
> 详见 `docs/p0-exec-report-2026-10-09.md`。

> **数据版本 1.1.1（2026-09-28）修复内容**：分子名守卫（`mol_name`/`iupac_name` 的
> `CID <数字>` 占位符与 CAS 号一律不再充当名称）；BATMAN predicted 引用的 1,090 个
> Entrez 经 `org.Hs.eg.db` 补全为 HGNC 符号（靶点空值清零）；compat 表按 10 列判重
> （-67,010 行）；HERB 靶点枢纽按物种过滤（剔除 187 个小鼠条目，-398 行）。
> 详见 `docs/defect-fix-b1b2b3-2026-09-28.md`。
>
> **查询口径（boss 2026-09-28 拍板「尽量全」）**：`herb_target()` / `target_herb()`
> 默认**保留**源侧无 OB/DL 的行，并以 `adme` 列标注（`ok` / `no_adme`）；
> 传 `include_missing = FALSE` 可回到旧的「仅保留过阈值行」口径。
>
> L3 数据缺陷修复（2026-09-15）：`iupac_name`/`mol_name` 中的 CAS 号已迁入独立 `cas` 列；
> 分子名经 PubChem 回填（889 CID）；`mapping_status` 列语义修复；`herb_cn_source` 溯源加守卫。
> 详见 `docs/l3-data-defect-fix-report-2026-09-15.md`。

## 可再分发基因集（data/，P3.3，2026-09-05 生成）

> 替代原 human/mu_kegg（KEGG）与 human/mu_gsea（MSigDB Hallmark）——两者下载均不可再分发，已从包内移除（kegg.filter 作为既往分析示例保留并注明来源）。

| 数据集 | 内容 | term 键 | 行数/通路数 | 来源与许可 |
|---|---|---|---|---|
| human_go | GO BP/CC/MF 合并，剔 IEA，≥2 基因/term | GO:xxxxxxx | 224,180 行 / 11,626 term | org.Hs.eg.db 3.23.1 直接注释 + GO.db；GO Consortium CC BY 4.0 |
| mu_go | 同上（小鼠） | GO:xxxxxxx | 271,809 行 / 13,625 term | org.Mm.eg.db 3.23.0 SQLite 注释（直读，未装 R 包）+ GO.db；CC BY 4.0 |
| human_reactome | Reactome 通路 all-level | R-HSA-xxxxx | 139,327 行 / 2,534 通路 | reactome.org NCBI2Reactome_All_Levels（2026-09-05）；Reactome CC BY 4.0 |
| mu_reactome | 同上（小鼠） | R-MMU-xxxxx | 88,600 行 / 1,802 通路 | 同上（Mus musculus） |

- 用途：离线 ORA/GSEA 的 TERM2GENE（clusterProfiler::enricher/GSEA）；term→名称/本体映射见 `data-raw/gene-sets/output/*_map.csv`
- 复现：`data-raw/gene-sets/scripts/{build_go_genesets.R, build_mu_go_sqlite.R, parse_reactome.py, build_reactome_symbol.R}`
- 决策记录：GO/Reactome 键用稳定 ID（GO 名跨本体重名 1.2 万+、Reactome 名同种内重名 ~85%）；Reactome 用官方 all-level（等价 ReactomePA 默认口径，用户拍板 2026-09-05）；KEGG REST API 在线可选接口为后续待评估项

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
