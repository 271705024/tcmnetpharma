# 快速开始（R 用户 · 5 分钟）

> 面向会一点 R 的使用者。不想写代码的读者 → 看 [`USER-GUIDE.md`](USER-GUIDE.md)（Windows 图形界面手册）。
> 本文所有调用均取自包内帮助页的示例，可逐段复制执行。

---

## 0. 装好就能用

```r
# 首次：拉运行时数据（19 个 parquet，约 183 MB，从 Zenodo 断点续传）
# 在包源码根目录执行一次即可
# Rscript tools/fetch-data.R

install.packages("tcmnetpharma_0.2.0.tar.gz", repos = NULL, type = "source")
library(tcmnetpharma)
```

数据随包离线可用，**分析时不联网**。只有 KEGG 维度需要联网（本包不分发 KEGG，见文末）。

---

## 1. 四步走完一次完整分析

### 第一步 · 取数据：某味药有哪些成分、打哪些靶点

```r
hit_herb <- herb_target("麻黄", type = "Herb_cn_name")
head(hit_herb)
#   herb molecule_id molecule          target    adme
# 1 麻黄     MOL000098  quercetin        PTGS2      ok
# ...
```

- `type` 可选 `Herb_cn_name`（中文名）、`Herb_name_pin_yin`（拼音）、`herb_id` 等；
- `adme` 列：`ok` = 过 OB/DL 阈值，`no_adme` = 源侧无 ADME 数据但**保留**（2026-09-28「尽量全」口径）。要旧的严格口径传 `include_missing = FALSE`。

### 第二步 · 交集：药和病的共同靶点

```r
hit <- tcm_intersect_target("麻黄", "Asthma")
head(hit)
#   target_symbol   herb n_molecule disease_score ...
```

返回按 `disease_score` 与 `n_molecule` 双降序；`target_symbol` 就是下一步要喂的基因。

### 第三步 · 富集：这些靶点在干什么

```r
res <- tcm_enrich(hit$target_symbol, keytype = "SYMBOL", species = "human")
names(res)          # "go" "reactome" "wikipathways"（只保留非空结果）
bar_plot(res$wikipathways, top = 10)
dot_plot(res$go, top = 10)
```

**离线三维**：GO（BP/CC/MF）+ Reactome + WikiPathways，全部随包，装上即跑、结果可复现（数据版本锁定）。

### 第四步 · 拓扑与网络：找核心靶点、画图

```r
net <- tcm_net(hit_herb, view = "cluster", style = "journal")   # 成分-靶点网络
tcm_centrality(hit_herb, top_n = 10)                            # 度/介数等中心性排名
tcm_modules(hit_herb, algorithm = "louvain")                    # 模块划分
tcm_ppi(hit$target_symbol, min_score = 700) |> ppi_plot(view = "hub")
```

- 网络的 `view` 是**数据轴**（`overview` / `cluster` / `hub`），`style` 是**外观轴**（`default` / `journal` / `cytoscape` / `presentation` / `minimal`）——两者正交，15 种组合都可用。
- `tcm_ppi()` 的 `min_score`：700 high / 400 medium / 150 low（STRING 置信度）。

---

## 2. 图形界面（不想写代码）

Windows 便携版（`tcmnetpharma.exe`）封装了同样的四步流程，双击即用。操作说明见
[`USER-GUIDE.md`](USER-GUIDE.md)；安装包发布渠道见仓库 Releases。

---

## 3. 常见坑

| 现象 | 原因与解法 |
|---|---|
| `herb_target("麻黄")` 查不到 | 药名写法不在库里。先 `molecule_serch()` / `herb_target(..., type = "Herb_name_pin_yin")` 试拼音键；`resolve_herb = TRUE` 走别名解析 |
| 富集报 "no significant" | 输入基因太少或阈值太严。放宽 `pvalueCutoff` / `qvalueCutoff`，或 `minGSSize` |
| 想要 KEGG 通路 | KEGG 不允许随包分发。联网自行 `clusterProfiler::enrichKEGG()`，返回值可直接喂给 `bar_plot()` / `dot_plot()` / `pathway_cirplot()` |
| 装包时报找不到 `inst/extdata/*.parquet` | 运行时数据未拉取，先跑 `Rscript tools/fetch-data.R` |
| 想换数据源（内网镜像） | `export TCMNETPHARMA_DATA_URL="https://<mirror>/tcmnetpharma-data"` 后再跑拉取脚本 |

---

## 4. 数据与引用

- 逐表行数、列语义、QC 分层、已知数据特征 → [`../inst/extdata/DATA-README.md`](../inst/extdata/DATA-README.md)
- 数据版本 `TCMNP_1.3.0_2026-10-21`，数据集 DOI **`10.5281/zenodo.23681367`**
- 论文引用格式见仓库根目录 `CITATION.cff`；**上游各源须同时引用**（清单见 DATA-README §来源与许可）
