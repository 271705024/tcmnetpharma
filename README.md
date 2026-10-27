# tcmnetpharma

**Traditional Chinese Medicine Network Pharmacology** — 中药网络药理学可视化分析工具包。

## 简介

tcmnetpharma 提供中药复方/单药的网络药理学分析可视化功能（成分-靶点网络、富集分析、PPI、韦恩图、桑基图等，共 **56 个导出函数**），内置多数据库整合的知识库快照。

## 数据来源（按各源许可署名，详见 `inst/extdata/DATA-README.md`）

| 数据源 | 版本 | 内容 | 许可 | 分发方式 |
|---|---|---|---|---|
| TCMSP | 2026-08 抓取（502 药/13,729 成分/3,339 靶点） | 中药-成分-靶点 + ADME | ODbL | 派生数据随包 |
| HERB 2.0 | 2.0（2024-11 更新） | 6,892 药/44,595 成分/15,515 靶点 + 性味归经 | 公开下载（引用 Fang et al. 2021） | 派生数据随包 |
| BATMAN-TCM 2.0 | 2.0 | 成分-靶点（known/predicted） | 公开下载（引用 Liu et al. 2024） | 派生数据随包 |
| ETCM2 | 2.0 | 48,442 方/38,298 成分 + 性味归经、成分 ADME、药材靶点 | 注册使用（引用 Zhang et al. 2023） | 派生数据随包 |
| ChEMBL | 37（2026-05） | 成分-靶点交叉验证 + 作用机制（MOA）/ 适应症 | CC BY-SA 3.0 | 派生数据随包 |
| **STRING** | v12.0 | 人类 PPI 物理互作子集（combined score ≥ 700） | **CC BY 4.0**（引用 Szklarczyk et al. 2023） | 派生数据随包 |
| **TRRUST** | v2 | 人/鼠转录因子-靶点调控关系 | **CC BY-SA 4.0**（引用 Han et al. 2018） | 派生数据随包 |
| **Open Targets Platform** | 25.06 | 疾病→靶点关联（association_overall_direct） | **CC0 1.0**（引用 Ochoa et al. 2023） | 派生数据随包 |
| **SymMap** | V2.0 | 疾病本体映射、拉丁药名交叉引用 | **未明示**（原文 CC BY-NC 4.0）；仅限学术使用 | 派生数据随包 |
| WikiPathways / Reactome / GO | 20261010 / 2026-09-05 / 3.23.x | 通路与功能富集基因集 | CC0 / CC BY 4.0 / CC BY 4.0 | 随包离线 |
| TTD | 2024.01.10 冻结版 | 靶点-疾病关联 | **All rights reserved** | **不随包分发**（仅内部交叉校验） |
| KEGG | 联网实时（`rest.kegg.jp`） | 通路富集 | 专有（Kanehisa Labs）；不可再分发 | **接口保留、数据不存**：`tcm_enrich(ont = "KEGG")` 运行时抓取并缓存到用户目录，包内零 KEGG 数据 |

数据版本、逐表行数与清洗口径见 `inst/extdata/DATA-README.md`。

## 安装

```r
# 1) 拉取外部化运行时数据（19 个 parquet，~183 MB；首次或数据缺失时执行）
Rscript tools/fetch-data.R   # 默认从 Zenodo（DOI 10.5281/zenodo.23681367）拉取，可传参覆盖
# 2) 本地构建
R CMD build .
R CMD INSTALL tcmnetpharma_*.tar.gz
```

> 运行时数据（inst/extdata/*.parquet）已**外置、不入库**，详见文末「运行时数据（外部化）」。

## 快速开始

```r
library(tcmnetpharma)

# 1) 查两味药的成分-靶点（拼音键检索）
fufang <- herb_target(c("ma huang", "ku xing ren"), type = "Herb_name_pin_yin")
head(fufang)

# 2) 画成分-靶点网络（view 选数据轴，style 选外观）
data("xfbdf", package = "tcmnetpharma")
network.data <- xfbdf |>
  dplyr::select(herb, molecule, target) |>
  dplyr::slice_sample(n = 100) |>
  as.data.frame()
tcm_net(network.data, view = "cluster", style = "journal")
```

数据本身不必先联网：装好包即含知识库快照。想换成自建/镜像数据源，见下文 `TCMNETPHARMA_DATA_URL`。
完整四步流程（含常见坑与参数说明）见 [`docs/QUICKSTART.md`](docs/QUICKSTART.md)。

## 文档

| 文档 | 面向 | 内容 |
|---|---|---|
| [`docs/QUICKSTART.md`](docs/QUICKSTART.md) | R 用户 | 5 分钟跑通四步分析（取数 → 交集 → 富集 → 拓扑），含常见坑 |
| [`docs/USER-GUIDE.md`](docs/USER-GUIDE.md) | 非技术用户 | Windows 图形界面手册：怎么打开、七个功能页、四步流程、常见图怎么读、FAQ |
| [`inst/extdata/DATA-README.md`](inst/extdata/DATA-README.md) | 数据分析者 | 逐表行数、列语义、QC 分层、已知数据特征、来源与许可 |
| [`NEWS.md`](NEWS.md) | 所有用户 | 版本变更记录 |
| 函数参考 | R 用户 | `?tcm_net` / `help(package = "tcmnetpharma")` |

## 许可与免责声明

**代码**：GPL-3　**运行时数据**：CC BY-SA 4.0（DOI 见下文）

⚠️ **并非所有上游源都是开放许可**：ETCM2 需注册后使用；TTD 保留所有权利（All rights reserved），**不随包分发**；KEGG 专有，**不随包分发**；SymMap 未明示许可，仅限学术使用。本包分发的是经筛选、键化与合并的**派生数据**，不包含上述来源的原始文件。各源许可、署名要求与 CC BY-SA 4.0 的「相同方式共享」义务详见 `inst/extdata/DATA-README.md` §来源与许可。

**免责声明**：本软件与随附数据仅供科学研究与教学使用，**不构成任何医疗、诊断或用药建议**；数据按「现状」提供，不对完整性、准确性作明示或默示担保，不得作为临床决策依据。

## 引用

论文写作中，请按开源惯例引用本项目、**数据集 DOI** 及各数据源原始论文。

- **数据集**：Zheng, JianXiao. (2026). *tcmnetpharma runtime dataset: multi-source Traditional Chinese Medicine network pharmacology data (TCMNP v1.3.0)* [Data set]. Zenodo. https://doi.org/10.5281/zenodo.23681367

## 版本

- 0.2.0 (2026-08-22)：多源数据库整合版（数据更新至 2026）
- 0.1.0 (2023)：初版（TCMSP 单一数据源）

## 运行时数据（外部化）

`inst/extdata/*.parquet`（19 个文件，约 183 MB）是 tcmnetpharma 的运行时知识库快照，**不随 git 分发**，原因：

- 单文件 `standard_herb_mol_target.parquet` 约 102 MB，超过 GitHub 单文件 100 MB 硬上限；
- 多次版本迭代曾使仓库历史膨胀至 1.1 GB，外置后仓库降至约 14 MB，推送更稳更快。

数据已发布至 Zenodo（带 DOI，论文可直接引用）：

| 项 | 值 |
|---|---|
| **DOI（本版本 v1.3.0）** | [`10.5281/zenodo.23681367`](https://doi.org/10.5281/zenodo.23681367) |
| **发布页** | https://zenodo.org/records/23681367 |
| **下载基础 URL** | `https://zenodo.org/records/23681367/files` |
| 许可证 | CC BY-SA 4.0 |

```bash
# 默认即从上面的 Zenodo 地址拉取（curl 支持断点续传，适配不稳定网络）
Rscript tools/fetch-data.R

# 或显式指定其他数据源（任何提供同名文件的 HTTP 目录 / 对象存储镜像均可）
export TCMNETPHARMA_DATA_URL="https://<your-mirror>/tcmnetpharma-data"
Rscript tools/fetch-data.R
```

数据版本、来源与清洗口径见 `inst/extdata/DATA-README.md`。
