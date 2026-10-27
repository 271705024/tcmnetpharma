# tcmnetpharma

**Traditional Chinese Medicine Network Pharmacology** — 中药网络药理学可视化分析工具包。

## 简介

tcmnetpharma 提供中药复方/单药的网络药理学分析可视化功能（成分-靶点网络、富集分析、PPI、韦恩图、桑基图等 38 个绘图与查询函数），内置多数据库整合的知识库快照。

## 数据来源（均开源，按来源许可标注）

| 数据源 | 版本 | 内容 | 许可 |
|---|---|---|---|
| TCMSP | 2026-08 抓取（502 药/13,729 成分/3,339 靶点） | 中药-成分-靶点 + ADME | ODbL |
| HERB 2.0 | 2.0（2024-11 更新） | 6,892 药/44,595 成分/15,515 靶点 + 性味归经 | 公开下载（引用论文） |
| BATMAN-TCM 2.0 | 2.0 | 成分-靶点（known/predicted） | 公开下载 |
| ETCM2 | 2.0 | 48,442 方/38,298 成分 | 注册使用 |
| TTD | 2026 (v9.0) | 靶点-疾病关联 30 万+ | 公开下载 |
| ChEMBL | 37 (2026-05) | 成分-靶点交叉验证 | CC BY-SA 3.0 |

数据版本与清洗细节见 `inst/extdata/DATA-README.md`。

## 安装

```r
# 1) 拉取外部化运行时数据（19 个 parquet，~183 MB；首次或数据缺失时执行）
Rscript tools/fetch-data.R   # 默认从 Zenodo（DOI 10.5281/zenodo.23681367）拉取，可传参覆盖
# 2) 本地构建
R CMD build .
R CMD INSTALL tcmnetpharma_*.tar.gz
```

> 运行时数据（inst/extdata/*.parquet）已**外置、不入库**，详见文末「运行时数据（外部化）」。

## 开源声明

本项目数据来源与教程均为开源资源，成果将以开源协议发布。代码与数据遵守各来源的许可条款（详见 DATA-README）。

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

# 或显式指定其他源（如 GitHub Release 备用通道）
export TCMNETPHARMA_DATA_URL="https://github.com/271705024/tcmnetpharma/releases/download/data-v1"
Rscript tools/fetch-data.R
```

数据版本、来源与清洗口径见 `inst/extdata/DATA-README.md`。
