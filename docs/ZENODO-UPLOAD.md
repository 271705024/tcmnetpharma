# Zenodo 上传清单 — tcmnetpharma 外部化数据集

> 配套方案：发布交接手册 `docs/RELEASE-GITEE-GITHUB.md` 的「数据外置（1b）」收尾步。
> 数据集与代码分离托管：代码在 GitHub/Gitee（`tcmnetpharma`），**数据集发 Zenodo 拿 DOI**（论文可引用），由 `tools/fetch-data.R` 按版本拉取。

---

## 0. 概览

| 项 | 值 |
|---|---|
| 数据集名 | `tcmnetpharma` 运行时数据集（中医药网络药理学多源整合） |
| 数据版本 | `TCMNP_1.3.0_2026-10-21`（命名空间前缀 `TCMNP_` 为包内数据版本体系，刻意保留，不删） |
| 关联代码 | https://github.com/271705024/tcmnetpharma （Gitee 镜像：https://gitee.com/ZJX261/tcmnetpharma） |
| **发布状态** | ✅ **已发布 2026-10-27**，DOI `10.5281/zenodo.23681367`，记录页 https://zenodo.org/records/23681367 |
| 文件数 | 19 个 `.parquet` |
| 总体积 | 182.3 MB（单文件最大 `standard_herb_mol_target.parquet` = 101.62 MB） |
| Zenodo 限制 | 单文件 ≤ 50 GB、总计 ≤ 100 GB → 182 MB 远在限制内 |
| 本地源 | `D:/update new R/backups/tcmnp-inst-extdata-2026-10-22/` |
| 完整性 | 19 个文件 sha256 见 §3；与包内 `inst/extdata/` 逐字节一致（`wt_match=True`） |

---

## 1. 上传前准备（本地已完成 ✅）

- [x] 19 个 parquet 已置于 `backups/tcmnp-inst-extdata-2026-10-22/`
- [x] 每个文件 sha256 已算（见 `backups/.../ZENODO_MANIFEST_DATA.json` 与 §3）
- [x] 与包内工作树数据逐字节一致 → 发布的校验和能对上用户经 `fetch-data.R` 下载的文件
- [x] **已定**：数据集许可证 = `CC BY-SA 4.0`（保守路线，见 §7-A）
- [ ] **待填**：作者 ORCID（占位 `<ORCID>`，发布前在 UI 补）

> 不阻塞：可先「预保留 DOI」（Zenodo 上传页「Reserve DOI」），拿到 DOI 后回填 README 再正式发布。

---

## 2. Zenodo 元数据模板（上传页逐项填写）

| 字段 | 填写值 | 备注 |
|---|---|---|
| **Resource type** | Dataset | |
| **Title** | tcmnetpharma runtime dataset: multi-source Traditional Chinese Medicine network pharmacology data (TCMNP v1.3.0) | |
| **Authors** | `Zheng, JianXiao` | ORCID 待填（若有，格式 `0000-000X-XXXX-XXXX`）；可加共同作者 |
| **Affiliations** | Haihe Laboratory of Traditional Chinese Medicine, Tianjin University of Traditional Chinese Medicine | 天津中医药大学海河实验室 |
| **Description** | 见下方「描述正文」块 | 建议英文为主 |
| **License** | `CC BY-SA 4.0` *(已定，保守路线；ODbL 亦可，见 §7-A)* | |
| **Keywords** | Traditional Chinese Medicine; network pharmacology; TCMSP; HERB; BATMAN-TCM; ETCM2; ChEMBL; herb-molecule-target; data integration | |
| **Version** | `1.3.0` | 与数据版本 `TCMNP_1.3.0` 对应（前缀不进 Zenodo 版本号） |
| **Language** | English | |
| **Related identifiers** | 见 §4 | IsSupplementTo + references |
| **Funding** | 若有基金号在此填 | 可选 |
| **Publication date** | 上传当日 | |

### 描述正文（可整段复制）

```
tcmnetpharma runtime dataset (data version TCMNP_1.3.0, 2026-10-21) provides the 19 parquet
tables consumed by the tcmnetpharma R package (https://github.com/271705024/tcmnetpharma),
a toolkit for Traditional Chinese Medicine (TCM) network pharmacology analysis.

The dataset integrates six upstream sources — TCMSP, HERB 2.0, BATMAN-TCM 2.0, ETCM2,
ChEMBL 37, and SymMap/OpenTargets (disease dimension) — calibrated against the 2025 edition
of the Chinese Pharmacopoeia. It contains a standardized herb–molecule–target wide table
(16.3M rows, 33 columns, with provenance and QC-level columns), a 10-column compatibility
table, disease/entity mapping tables, ETCM2 reference tables, ChEMBL-derived mechanism-of-action
and indication tables, and row/table-level provenance tables.

All files are Apache Parquet. Each file's SHA-256 is published in the repository
(docs/ZENODO-UPLOAD.md) for integrity verification. The dataset is externalized from the code
repository and retrieved at build/runtime via tools/fetch-data.R.
```

---

## 3. 文件清单（含 SHA-256，上传后核对用）

| # | 文件 | 体积 (MB) | SHA-256 |
|---|---|---|---|
| 1 | disease_main.parquet | 0.34 | `eca42fbfeb9a5d625519ce8c6324371c9056eeec65af0b469f2ba7f5b8644b95` |
| 2 | disease_target.parquet | 29.16 | `b61183abff9fe8cdec9aff365f7efd8dae06b396e5df5fc76d2a927b897abd6f` |
| 3 | etcm2_disease_gene.parquet | 0.17 | `63c98ad7884ae81c92fbf06c5e7d68867e602e3774edf9ca69d273e62b592a6d` |
| 4 | etcm2_herb_property.parquet | 0.08 | `a420489e5da996dc00e59c171b43e383d30abc5eae72fdf8ce14677bcad0e550` |
| 5 | etcm2_herb_target.parquet | 0.05 | `e396c40038ad3fc8e9dc6061fc39ef5e1b8760010bab101ca0f9d37a22295ead` |
| 6 | etcm2_mol_adme.parquet | 1.29 | `98b2e3b4a05e2d3a9db6975f9e06414d4dd53f9e7f01f5e1747f2ab0a0309ed1` |
| 7 | herb_key_collapse.parquet | 0.30 | `d4bb897fbdb2df6f45ba014d092ab4306d3f382309686b59c98e0eb27ac98e23` |
| 8 | herb_key_map.parquet | 0.16 | `c0e634b965b9c989432319e1ed406f693ead3a2001496548f99fd2da03710fe5` |
| 9 | herb_main.parquet | 0.28 | `1ca26ec5dd936bb3129879d377e650d6bc08156b7c52699fb8d2047051e7543f` |
| 10 | layer_a_disease_ontology_map.parquet | 0.18 | `da44d620cb80979ea62772c8da7d10926ed40142935a783d04ff689d762f90a8` |
| 11 | mol_entity_alias.parquet | 1.02 | `38afc8087c293f1f61ff47516e2a6dc5f66eb771cefa871b566a47474b53bf13` |
| 12 | provenance_rows.parquet | 0.35 | `aa9dcea6499550df33b91bcb5b3f9706f18b3236419de0ffa03b2f73063a13dc` |
| 13 | provenance_tables.parquet | 0.01 | `ca3e9291da16eddc9139b8c0b0d8df3358219ecc6d52a92565d941282cbb0458` |
| 14 | raw_unmapped.parquet | 0.32 | `fc7ce7bfdb4fa990facc14b1de83b58f630927aa0441da1fda4355ffe7180997` |
| 15 | standard_herb_mol_target.parquet | 101.62 | `3dbdbbb67c50fab7e819afdf40fc2d0122964378dad0e503a0567f92cd6b1db4` |
| 16 | string_ppi_human.parquet | 0.30 | `8ea823705ed19054208cd7007ffc4f6908e1c33e959db9dc6671fe4747fbd2eb` |
| 17 | tcm_mol_indication.parquet | 1.20 | `7d4902516bdf6437ecb5e1afb191ca667cfb15b6d8e37926b7cb023e4cfe191e` |
| 18 | tcm_network_compat.parquet | 45.17 | `25a15145df0cd147943f9cdb6a204eff47b55e6030b6d7b366cac060a7362126` |
| 19 | tcm_target_moa.parquet | 0.30 | `9ec0d4b4db2ccde8662d90ba7a9cbfaaaedc240207159a8e337cb6a5ea964e48` |

**合计：19 文件，182.3 MB。** 机器可读版见 `backups/tcmnp-inst-extdata-2026-10-22/ZENODO_MANIFEST_DATA.json`。

---

## 4. Related identifiers（Zenodo 元数据「Related identifiers」区）

| Relation | Identifier | 说明 |
|---|---|---|
| **IsSupplementTo** | `https://github.com/271705024/tcmnetpharma` | 数据集 supplement 的代码仓库 |
| references | `https://doi.org/10.1093/nar/gkab746` | HERB 2.0（Fang et al. 2021） |
| references | `https://doi.org/10.1093/nar/gkae328` | BATMAN-TCM 2.0（Liu et al. 2024） |
| references | `https://doi.org/10.1093/nar/gkad968` | ETCM2（Zhang et al. 2023） |
| references | `https://doi.org/10.1093/nar/gkad984` | ChEMBL 37（Zdrazil et al. 2024） |
| references | `https://doi.org/10.1093/nar/gkv1318` | TCMSP（Ru et al. 2014） |

> 注：ETCM2 / TCMSP / BATMAN 的确切 DOI 请在上传前核对（部分以文章主页为准）；TTD 仅作内部校验、**不随数据集分发**，无需列入。

---

## 5. 上传步骤（手动，UI，逐步照做）

> 若 `https://zenodo.org/uploads` 列表里已有一个 **空白草稿**（之前脚本半截创建残留），优先点进去**直接填它**复用，省得删建；或删掉它再 New upload。

### 5.1 新建 / 打开草稿
1. 打开 https://zenodo.org/uploads/new （已登录账号）。
2. 页面分两层卡：**Files**（先传文件）与 **Basic information**（填元数据）。

### 5.2 传文件（Files 区）
3. 把 `D:/update new R/backups/tcmnp-inst-extdata-2026-10-22/` 下的 **19 个 `.parquet`** 直接**拖进**上传区（或点 browse 多选）。
   - 共 182.3 MB，Zenodo 限制单文件 50 GB，毫无压力，无需分卷。
   - 校园网若中途掉线，传完的文件会保留，刷新重传剩余即可。

### 5.3 填元数据（Basic information 区，逐项照抄）
4. 点开 **Basic information** 卡，按下表填（英文文本可直接复制）：

| 字段 | 填值 |
|---|---|
| **Resource type** | `Dataset` |
| **Title** | `tcmnetpharma runtime dataset: multi-source Traditional Chinese Medicine network pharmacology data (TCMNP v1.3.0)` |
| **Authors / Creator** | `Zheng, JianXiao`，Affiliation 填 `Haihe Laboratory of Traditional Chinese Medicine, Tianjin University of Traditional Chinese Medicine`；ORCID 有就补（格式 `0000-000X-XXXX-XXXX`） |
| **Description** | 复制粘贴下方「描述正文」整段（§2） |
| **License** | 搜索并选 **`Creative Commons Attribution Share Alike 4.0 International`**（即 CC BY-SA 4.0） |
| **Keywords** | `Traditional Chinese Medicine`, `network pharmacology`, `TCMSP`, `HERB`, `BATMAN-TCM`, `ETCM2`, `ChEMBL`, `herb-molecule-target`, `data integration`（逐条回车添加） |
| **Version** | `1.3.0` |
| **Language** | `English` |

5. **Related identifiers**（同一区往下找「Add」）：逐条添加 §4 的 6 行：
   - `IsSupplementTo` → `https://github.com/271705024/tcmnetpharma`
   - `references` → 5 个上游 DOI（见 §4）

### 5.4 发布
6. 文件传完 + 元数据填妥后，点右上 **Reserve DOI**（记下 `10.5281/zenodo.XXXXXXX` 备用），再点 **Publish**。
7. 发布后，Zenodo 会显示每个文件的 sha256/size → 与 §3 逐一对账（可在本机 `sha256sum` 复核）。

### 5.5 回填（闭环）
8. 把 DOI 基础 URL 回填 README 的 `TCMNETPHARMA_DATA_URL`（见 §8），`git commit` + `git push gitee main`。

---

## 6. 可选：REST API 批量上传【⚠️ 已不稳定，优先走 §5 手动 UI】

> 注：Zenodo 已迁移到 InvenioRDM，旧 `/api/deposit/depositions` legacy 接口与文件上传路径在新界面下行为不稳，实测已失败。**新用户请直接按 §5 手动 UI 填**，以下仅作高级参考、不再推荐。

```bash

```bash
# 1) 创建 deposition（拿到 DEP_ID）
DEP_ID=$(curl -s -X POST "https://zenodo.org/api/deposit/depositions?access_token=$ZENODO_TOKEN" \
  -H "Content-Type: application/json" -d '{}' | python -c "import sys,json;print(json.load(sys.stdin)['id'])")

# 2) 逐文件上传（182MB 走分块/单请求均可，Zenodo 接受大文件）
for f in /d/update\ new\ R/backups/tcmnp-inst-extdata-2026-10-22/*.parquet; do
  curl -X PUT "https://zenodo.org/api/deposit/depositions/$DEP_ID/files/$(basename "$f")?access_token=$ZENODO_TOKEN" \
    -H "Content-Type: application/octet-stream" \
    --data-binary @"$f"
done

# 3) 填元数据（title/authors/license/related_identifiers...，见 §2/§4 JSON 结构）
# 4) 发布
curl -X POST "https://zenodo.org/api/deposit/depositions/$DEP_ID/actions/publish?access_token=$ZENODO_TOKEN"
```

- `$ZENODO_TOKEN` 由你在 Zenodo「Applications → Personal access tokens」生成（勾 `deposit:write`），**自行保管，不要贴进仓库或聊天**。
- 本脚本仅作模板，执行由你手动完成（涉及凭证，按规矩不代跑）。

---

## 7. 待拍板项

### A. 数据集许可证（已定 ✅）
boss 拍板走**保守路线**，尊重上游「相同方式共享」义务：
- **采用 `CC BY-SA 4.0`**（默认值，已写入 §2 元数据模板）。
- **`ODbL` 亦可** —— 若希望与 TCMSP 上游许可完全同构，改用 ODbL 同样合规。
- 理由：上游 TCMSP 为 **ODbL**、ChEMBL 37 为 **CC BY-SA 3.0**，均含 share-alike 义务；用 `CC BY-SA 4.0`/ODbL 可避免「宽松许可遮蔽上游 copyleft」的合规风险。
- publish 前若导师/图书馆另有要求，仍可改；当前默认按上表执行。

### B. 作者与 ORCID（占位待填 ⏳）
- 第一作者 `Zheng, JianXiao`（已填占位）；**ORCID 留占位** `<ORCID>` 待你补入元数据与 §2。
- 是否列共同作者/通讯作者：**留占位**，请确认后补。
- 上传前务必补全这两项，Zenodo 发布后修改作者需发新版本。

### C. DOI 版本化
- 本次发 `1.3.0`；未来数据更新时**发新版本**（Zenodo 支持同一 deposition 下新版本，DOI 增加 suffix 如 `10.5281/zenodo.XXXXXXX.v2`），并同步 bump `fetch-data.R` 的默认版本。

---

## 8. 上传后回填（✅ 已完成 2026-10-27）

实际回填值：
- `README.md` → 「运行时数据（外部化）」+「引用」：DOI `10.5281/zenodo.23681367`、发布页、基础 URL `https://zenodo.org/records/23681367/files`
- `tools/fetch-data.R` → `default_data_url` 默认即上述 Zenodo 地址（不传参直接可用）

1. **README.md「运行时数据（外部化）」一节**：把 `<owner>/<repo>` 占位替换为 Zenodo 基础 URL，例如：
   ```
   https://zenodo.org/records/<ZENODO_ID>/files/
   ```
   或按 `tools/fetch-data.R` 期望的 `TCMNETPHARMA_DATA_URL` 基础路径填写（建议用 Zenodo 的 `records/<ID>/files/` 永久链接，而非易变的 `doi/` 重定向）。
2. **tools/fetch-data.R**：确认 `TCMNETPHARMA_DATA_URL` 默认值 = 上方基础 URL；如需按数据版本分子目录，对齐 §7-C。
3. **论文数据引用**：使用 Zenodo 提供的 Cite as / BibTeX（含 DOI）。
4. 回填后 `git commit` + `git push gitee main`（自动镜像 GitHub），数据外置链路全部闭环。

---

## 9. 完整性校验（publish 后）

- 下载任一文件，比对 sha256 与 §3 一致：
  ```bash
  sha256sum downloaded.parquet
  # 应与 §3 对应行一致
  ```
- 经 `tools/fetch-data.R` 拉取后，包内 `inst/extdata/*.parquet` 的 sha256 应与 §3 完全相同（已验证本地 working-tree 一致）。
