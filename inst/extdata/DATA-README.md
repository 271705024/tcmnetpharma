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
| standard_herb_mol_target | inst/extdata/standard_herb_mol_target.parquet | TCMNP_1.3.0_2026-10-12 | 16,269,155 | 标准宽表（herb-mol-target + 来源 + 置信度 + ADME；含 `cas`；**新增 `herb_entity_id`/`herb_entity_id_source` + 信任与溯源 6 列**；**Layer A 融合 +2 列 `layer_a_latin`/`layer_a_hit_id`（SymMap 拉丁名交叉引用 53.3% / HIT 实验标记 30.7%）→ 共 33 列**） |
| tcm_network_compat | inst/extdata/tcm_network_compat.parquet | TCMNP_1.3.0_2026-10-12 | 16,202,145 | 10 列兼容表（喂现有函数；DuckDB 惰性查询；含 `CAS` 列；已按 10 列判重） |
| raw_unmapped | inst/extdata/raw_unmapped.parquet | TCMNP_1.3.0_2026-10-12 | 211,830 | 未映射/多余数据 |
| herb_main | inst/extdata/herb_main.parquet | TCMNP_1.3.0_2026-10-12 | 7,351 | **herb 实体主表**（`herb_id` 唯一身份；HERB 6,892 + BATMAN 合成 459） |
| herb_key_map | inst/extdata/herb_key_map.parquet | TCMNP_1.3.0_2026-10-12 | 14,670 | **键→实体映射**（多对一；`key_type`/`id_source`/`is_canonical`） |
| herb_key_collapse | inst/extdata/herb_key_collapse.parquet | TCMNP_1.3.0_2026-10-12 | 7,506 | **塌缩留痕**（某键收了哪些原始源条目；321 键吞并 340 条） |
| provenance_tables | inst/extdata/provenance_tables.parquet | TCMNP_1.3.0_2026-10-12 | 22 | **表级溯源**（表 × 源目录：文件数/字节/sha256 状态/加工步骤） |
| provenance_rows | inst/extdata/provenance_rows.parquet | TCMNP_1.3.0_2026-10-12 | 814,924 | **行级溯源**（UNMAPPED 211,830 + 被修正 603,094） |
| layer_a_disease_ontology_map | inst/extdata/layer_a_disease_ontology_map.parquet | TCMNP_1.3.0_2026-10-12 | 7,157 | **疾病本体映射参考表**（SymMap SMDE × OpenTargets；MeSH/UMLS 本体 ID；2026-10-14 并入） |
| herb_pm | data/herb_pm.rda | TCMNP_0.2.0_2026-08-22 | 6,860 | HERB 性味归经（6,892 去空后） |

> **数据版本 1.3.0（2026-10-12）变更内容**：**引入 herb 实体层**（P0-B）。
> 背景：拼音键此前同时充当「检索入口」与「实体身份」，实测宽表 `Herb_id` 空 **1,459,435 行(8.97%)**，
> 且 BATMAN 源 **321 个键吞并 340 条**条目却**零留痕**。
> - `standard` **新增两列**：`herb_entity_id`（**实体身份**，始终有值）、`herb_entity_id_source`
>   （`HERB`/`HERB_LATIN`/`HERB_CN`/`BATMAN_SYNTH`/`WHITELIST_MERGE`）。
>   ⚠️ 命名说明：**不能叫 `herb_id`** —— 本表已有 `Herb_id`，而 DuckDB 列名匹配**不区分大小写**，
>   两者构成「仅大小写不同」的碰撞（`"herb_id"` 会静默解析到 `Herb_id`）。该碰撞由
>   `common.assert_no_col_case_collision` 在落盘前**硬拦下**（2026-10-12 实测：首次接线即被捕获）。
>   ⚠️ **`Herb_id`（旧列）语义不变** —— 它是「HERB 源自身的 id」，对 BATMAN 专有药材为空；
>   新列 `herb_entity_id` 才含跨源归一结果。**下游 join/去重请以 `herb_entity_id` 为准**。
> - 新增三表 + `herb_alias.csv`（人可读键别名表）。
> - 同名异键：83 组中 **56 组已自动归一**（拉丁名/中文名桥接），其余列入
>   `data-raw/cleaning/herb_alias_whitelist.csv` **逐条授权**（脚本从不自动跨实体合并）；
>   本轮已授权合并 3 组（拉丁名完全一致），另 24 组待人工判。
> - 行数**不变**（实体层只改身份归属，不增删行）。详见 `docs/legacy-items-solution-design-2026-10-12.md` §1。
>
> **数据版本 1.3.0（2026-10-12）· 信任与溯源 6 列（P1）**：`standard` 增至 **31 列**（**不进 `compat`**，守 10 列契约）。
> | 列 | 含义 |
> |---|---|
> | `qc_level` | 4 级信任：`High` 432,401 / `Medium` 15,808,628 / `Low` 210,077 / `Untrusted` 59 |
> | `qc_level_reason` | **逐行的判定理由**（免去使用者自己推断；也是"低 qc 不另做行级 provenance"的依据） |
> | `cas_verified` | **全量 `FALSE`** —— 我们**没有**校验过 CAS（NLM 明示 PubChem 不校验 CAS）。把它写成显式事实，而不是让使用者默认"有值即可信" |
> | `cas_source` | `source_field`（源侧字段给出）/ `none` |
> | `latin_name_std` | 拉丁名标准化（实体层权威名 → 二名法清洗；**只做属+种，禁部分匹配**） |
> | `latin_match_status` | `unverified`（有二名法但**未经 MPNS/Kew 交叉核验**）/ `missing` —— **不假装已验证**；MPNS 交叉核验待离线数据源 |
>
> **溯源两表**：`provenance_tables`（表级：源目录文件数/字节/sha256 状态/加工步骤；小源逐个 sha256，chembl/pubchem 等大源如实标 `not_hashed:size`，**不假装已校验**）；
> `provenance_rows`（行级：UNMAPPED 211,830 —— `source` 列**直接取自源侧**；被 PubChem 名称回填修正 603,094 —— CID 名单来自回填表本身，可复算）。
> **低 `qc_level` 不另做行级 provenance**：`qc_level_reason` 已逐行给出原因，再抄一份即同一事实两处声明（成本与收益不成比例）。
> 另：`iupac_name` 补齐 **B1 同类守卫**（`CID <数字>` 占位符/CAS 形态一律置空；实测 28 行 / 1 个 CID）。
>
> **数据版本 1.2.0（2026-10-09）修复内容**：**成分结构标识换源** —— 原「PubChem CID→InChIKey」
> 补映射误用 ChEMBL 的 `PUBCHEM_BIOASSAY`（其 `src_compound_id` 是 PubChem **SID 而非 CID**），
> 导致 107 个 CID / 约 9,338 行的 InChIKey 与 CID 错配；现改用 PubChem 官方
> `CID-InChI-Key.gz`（7.37 GB，2026-09-27，md5 校验通过）。效果：宽表 **4,776,230 行由
> UNMAPPED→MAPPED**（`raw_unmapped` 4,988,060 → 211,830，未映射对降至 27,631）；
> 去重撞车修复致 +21 行。另修正 QC 真值校验的宽匹配假通过（改精确匹配优先）。
> 详见 `docs/p0-exec-report-2026-10-09.md`。

> **数据版本 1.3.0（2026-10-12）· Layer A 融合补强（2026-10-14，版本号维持 1.3.0）**：`standard` **新增 2 列**（31 → **33 列**）——`layer_a_latin`（SymMap 草药拉丁名交叉引用，8,665,636 行非空 ≈53.3%）、`layer_a_hit_id`（SymMap SMTT 靶点 HIT 实验标记，4,990,660 行非空 ≈30.7%，值如 `T1237`）；两列**仅进 standard，不进 10 列 compat**（守契约）。新增独立参考表 `layer_a_disease_ontology_map`（7,157 行，SymMap SMDE × OpenTargets 疾病本体 MeSH/UMLS 映射），供下游疾病维度消费。Layer A 一次源（OpenTargets 25.06 + SymMap V2.0）经 `02d_layer_a_align` 跨库对齐后并入，与现有 HERB/BATMAN 枢纽 ID 语义一致（herb 92.7% / target 88.9%）——详见 `docs/data-source-registry-2026-10-12.md` §6 与 `docs/data-source-independence-roadmap-2026-10-12.md` §5。

> **⚠️ Layer A 两列的使用警戒（2026-10-15 纳入 QC F 层时补齐）**：
> - **`layer_a_latin` 是「拉丁药材名」（pharmaceutical Latin drug name），不是植物学学名（二名法）。**
>   取值形如 `Radix Et Rhizoma Notopterygii`（药用部位 + 属名），**不是** `Notopterygium incisum`。
>   它与 `latin_name_std`（实体层权威**学名**，如 `pheretima`）**语义不同、不可互换、不可互相校验**；
>   把两者当同一列用是错的。
> - 两列均为 **left join 交叉引用**：未命中即 NA（**合法**，不是缺数据），
>   不改变行数；键→值保证**函数性**（同一 herb 键只对应一个值）。
> - **已知缺陷（QC F 层如实登记为 FAIL，尚未修）**：① 字段**内嵌换行** 8 值 / 94,535 行（1.09%）
>   —— 导出 CSV 会**串行**（parquet 无此问题）；② 含**非 ASCII** 5 值 / 93,717 行（1.08%）
>   —— 源侧错字与全角混用，如 `Ⅴladimiriae Radix`（罗马数字 `Ⅴ` 应为 `V`）、
>   `Chuanxiong Rhizoma,…,；Rhizoma Chuanxiong`（全角分号）。
>   期间**不建议**直接用该列做字符串匹配或拼 SQL；修法见 `docs/analysis-module-wiring-2026-10-14.md` 后续条目。
> - **一字段多值**：75.66% 的取值为 2~4 个法定拉丁名**并列**（逗号/分号分隔）—— 属源数据语义结构，
>   不是错误；按单值处理会漏掉其余别名。

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

## 结构标识策略（2026-10-12 补 · P0-5）

本包以 **Standard InChIKey** 作为成分结构标识的**查找键（lookup key）**，
但**不把它当作化学身份的唯一判据**。使用时请理解其边界：

| 项 | 说明 |
|---|---|
| **来源** | PubChem 官方 `CID-InChI-Key.gz` 快照 —— **2026-09-27**，md5 `d39fd5843f2c70181c54f399bd2b4e86`（7.37 GB，逐字节校验）。**非本地重算** |
| **语义** | Standard InChI 会**归一化互变异构体与电荷形式**；**不支持相对立体化学**；不识别有机金属顺反异构（如 cisplatin/transplatin）。同一物质的不同盐或互变异构体**可能同键、也可能异键** |
| **四个已知不可判** | ① 互变异构归一（少数 1,5-酮-烯醇除外）；② 盐/游离碱取决于生成策略；③ 立体化学仅「绝对或无」；④ 跨库按 InChIKey join 会**继承各源标准化政策的分歧** |
| **本包口径** | **不做 canonical tautomer**（与 ChEMBL 一致）；只接受标准 27 字符形态 `^[A-Z]{14}-[A-Z]{8}[SN]A-[A-Z]$`；跨源补映射**只补不覆盖**（HERB 优先，PubChem 官方仅补缺口） |
| **映射溯源** | `mapping_status` 列记录来源；未映射者进 `raw_unmapped`，不静默丢弃 |
| **参考** | `docs/db-lessons-and-curation-playbook-2026-10-09.md` §2.1；`docs/p0-exec-report-2026-10-09.md` |

> ⚠️ **推论**：若需「严格化学身份」判定，请**回源比对结构（SMILES/InChI）**，
> 不要仅凭 InChIKey 相等下结论；跨源合并时尤须注意盐形式与立体化学差异。

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
