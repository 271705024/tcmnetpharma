# tcmnetpharma 使用手册（小白版）

> 一个帮你做「中药网络药理学」分析的桌面小工具。不会写代码也能用，双击就能打开。
>
> **适用对象**：Windows 便携版（图形界面）用户。只用 R 的用户看仓库 `README.md` 即可。
> **安装包**：Windows 便携包由构建流水线产出，发布渠道见仓库 Releases（当前版本发布中）。
> **数据与许可**：本手册涉及的数据源来自多个上游库，署名与许可要求见
> [`../inst/extdata/DATA-README.md`](../inst/extdata/DATA-README.md) §来源与许可。

---

## 0. 这是什么？一句话看懂

**网络药理学** 是研究「一味中药 → 它的化学成分 → 这些成分作用在人体哪些靶点（基因/蛋白质）→ 影响哪些疾病」的一门方法。

这个软件帮你把上面这条链路的分析**自动完成并画成图**，你只需要：

1. 输入一味药（比如「甘草」）或几个基因
2. 点几个按钮
3. 得到现成的网络图、富集分析、韦恩图、桑基图等，可直接放进论文

> 想先了解「什么是网络药理学」？看这篇科普：
> - [网络药理学（Network pharmacology）— Wikipedia](https://en.wikipedia.org/wiki/Network_pharmacology)
> - [中药网络药理学研究思路（中文综述，可作入门）](https://www.baidu.com/s?wd=中药网络药理学+研究思路)

---

## 1. 怎么打开软件

### 如果你是「普通用户」（推荐）

1. 拿到 `tcmnetpharma-0.2.0-windows.zip`，**解压**（右键 → 解压到当前文件夹）
2. 进入解压后的文件夹，**双击 `tcmnetpharma.exe`**
3. 第一次打开可能要等几秒到几十秒（软件在加载数据），窗口出来后就能用了

> 不需要安装 R、Python 或任何其他软件，解压即用。**前提是 Windows 系统**（Win10/11，64 位）。

### 如果你是「R 用户」（进阶，可选）

也可以用 R 直接调用它的函数：

```r
install.packages("tcmnetpharma_0.2.0.tar.gz", repos = NULL, type = "source")
library(tcmnetpharma)

# 查「甘草」的成分-靶点
target_serch("PTGS2")
```

> 没装过 R？先看官方入门：[R 语言官方下载页](https://cran.r-project.org/) 与 [RStudio（写 R 的好用编辑器）](https://posit.co/download/rstudio-desktop/)

---

## 2. 界面长什么样？七个功能页

打开后，左边/顶部有一排功能页，按「做一篇网络药理学分析」的顺序排好了：

| 页签 | 干什么 | 对应专业术语 |
|---|---|---|
| **数据查询** | 输入一味药/一个基因，查它的成分、靶点 | 中药成分库、靶点库 |
| **药物 × 疾病 交集靶点** | 找出「药的作用靶点」和「疾病的靶点」重叠的部分 | 交集（intersection） |
| **功能富集分析** | 看这些靶点集中在哪些生物功能/通路上 | GO 富集、KEGG 通路 |
| **网络拓扑分析** | 找出网络里最重要的节点（核心靶点）和模块 | 中心性、社区发现 |
| **本次分析结果汇总** | 把前面所有结果整理到一页 | — |
| **关于 / 数据来源** | 软件说明、数据出处、性能参考 | — |

> 名词不懂没关系，跟着下面一步步点就行，每个术语后面都附了解释链接。

---

## 3. 第一步：数据查询（拿数据）

**你要做的**：在「数据查询」页，输入一个中药名（如 `甘草`、`人参`）或一个基因名（如 `PTGS2`），点查询。

**你会得到**：

- **中药 → 成分**：这味药里有哪些化学成分
- **成分 → 靶点**：每个成分作用在人体哪些靶点基因上
- 附带每味药的 **OB（口服生物利用度）**、**DL（类药性）** 等筛选指标

> 术语速查：
> - [OB 口服生物利用度（Oral bioavailability）— Wikipedia](https://en.wikipedia.org/wiki/Bioavailability)
> - [DL 类药性（Drug-likeness）— Wikipedia](https://en.wikipedia.org/wiki/Druglikeness)
> - [靶点（Drug target）是什么 — 英文科普](https://en.wikipedia.org/wiki/Biological_target)

**常用中药名示例**（可以直接试）：`甘草`、`麻黄`、`人参`、`黄连`、`黄芪`。

---

## 4. 第二步：交集靶点（药和病的共同点）

这是网络药理学最关键的一步：**证明「这个药」确实跟「这个病」有关系**。

**原理**：一味药有一堆靶点，一种病也有一堆相关靶点，两者**重叠**的那部分，就是这味药可能治疗这个病的关键靶点。

**你要做的**：

1. 输入药物相关基因（从上一步结果里复制，或软件自动带入）
2. 输入疾病相关基因（或从内置疾病库选）
3. 点「取交集」

**你会得到**：一张韦恩图（Venn 图），清楚显示重叠了几个靶点，以及重叠靶点的列表。

> 术语速查：
> - [韦恩图（Venn diagram）— Wikipedia](https://en.wikipedia.org/wiki/Venn_diagram)
> - [交集与并集（Set operations）— Wikipedia](https://en.wikipedia.org/wiki/Set_(mathematics))

---

## 5. 第三步：功能富集分析（这些靶点在干嘛）

拿到一堆重叠靶点后，你会想知道：**这些靶点凑在一起，主要参与哪些生物过程、哪条信号通路？**

富集分析就是回答这个问题的。软件支持四类：

| 类型 | 中文名 | 说明 | 数据从哪来 |
|---|---|---|---|
| **GO** | 基因本体富集 | 分成生物过程(BP)、细胞组分(CC)、分子功能(MF)三类 | 内置离线，无需联网 |
| **KEGG** | 通路富集 | 看落在哪条经典信号通路（如 PI3K-Akt、TNF 通路） | 联网抓取+本地缓存 |
| **Reactome** | 通路富集 | 类似 KEGG 的另一套通路数据库 | 内置离线 |
| **WikiPathways** | 通路富集 | 社区维护的通路库 | 内置离线 |

**你要做的**：贴入基因列表 → 选富集类型 → 点「富集」。

**你会得到**：一张气泡图/条形图/圈图，显示富集最显著的若干条目，可下载。

> 术语速查：
> - [GO 基因本体（Gene Ontology）— Wikipedia](https://en.wikipedia.org/wiki/Gene_ontology)
> - [KEGG 通路数据库官网](https://www.genome.jp/kegg/pathway.html)
> - [富集分析（Enrichment analysis）通俗解释](https://en.wikipedia.org/wiki/Gene_set_enrichment_analysis)

---

## 6. 第四步：网络拓扑分析（找核心靶点）

一张网络图里，有些节点特别「重要」（连着很多其他节点）。拓扑分析就是把这些关键节点找出来。

**你要做的**：把上一步的靶点关系导入 → 点拓扑分析。

**你会得到**：

- **核心节点（中心性）**：按「中介中心性（Betweenness）」等指标排名，找出枢纽靶点
- **功能模块（社区）**：网络里自发聚成的小团体，可能对应不同功能
- **拓扑可视化**：一张网络图，重要的节点画得更大/更居中

> 术语速查：
> - [中介中心性（Betweenness centrality）— Wikipedia](https://en.wikipedia.org/wiki/Betweenness_centrality)
> - [社区发现（Community detection）— Wikipedia](https://en.wikipedia.org/wiki/Community_structure)

---

## 7. 常见图怎么读

软件会产出这些图，附「怎么读」：

| 图 | 长什么样 | 怎么读 |
|---|---|---|
| **韦恩图** | 几个重叠的圆 | 重叠区域 = 交集，数字是交集元素个数 |
| **气泡图** | 横轴纵轴 + 大小不一的圆点 | 点越大越显著，颜色越红 p 值越小 |
| **柱状图（文献横条）** | 一排横条，条越长时间基因越多 | 条上数字 = 基因数；颜色 = 三大本体（BP/CC/MF），论文里最常用的形制 |
| **桑基图** | 左中右三列，中间流动的带子 | 展示「药→成分→靶点」的流向与数量 |
| **网络图** | 点 + 线 | 点=靶点/成分，线=有关系，越中心越重要 |
| **弦图/圈图** | 圆环状 | 展示通路或靶点间的对应关系 |

### 富集图的风格预设（2026-10-22 新增）

富集可视化页多了一个「出图风格」下拉框。**风格只改外观，不改数据与统计结果**（同一份数据换风格，条目、基因数、p 值完全一样）。

| 风格 | 适合场景 | 长什么样 |
|---|---|---|
| **文献横条**（默认） | 写论文首选 | 横条长度 = 基因数，按 BP/CC/MF 三色，条端标数字 |
| **三合一分面** | 图注想说清「三类各富集了什么」 | BP/CC/MF 分成三段，**每类一个颜色**（BP/CC/MF 三色，不按 p 值上色） |
| **显著性渐变** | 想突出「谁最显著」 | 横条长度和颜色都是 −log10(p 值) |
| **文献气泡** | 气泡图投稿版 | 横轴 = GeneRatio（文献标准口径），点大小 = 基因数 |
| **期刊风** | 直接放进 Word 投稿 | 白底、无网格线、细轴线 |
| **演示风** | 组会 / 答辩投影 | 整体字号放大、轴线加粗 |

> 绘图页的「出图风格」也会**随图型自动变化**：网络图有 Cytoscape / 模块 / 核心 30 等风格，柱状图、度分布、桑基图提供期刊风与演示风。选了不适用的风格会被自动忽略（不会报错）。

### 配色体系（2026-10-22 新增）

绘图页和富集可视化页各多了一个「配色体系」下拉框。**配色只改颜色，不改数据、条数、p 值**。空着不选 = 用软件原来的默认配色。

色值不是随手调的，都来自公开发表的色板 —— 每套都在下拉项的悬浮说明里写明出处：

| 分组 | 色板 | 出处 | 适合 |
|---|---|---|---|
| 期刊 | **NPG 自然风** | Nature 系列期刊色板（ggsci 收录） | 投稿 Nature 系刊物 |
| 期刊 | **Science 风** | Science/AAAS 色板（ggsci 收录） | 投稿 Science 系刊物 |
| 期刊 | **Lancet 风** | The Lancet Oncology 色板（ggsci 收录） | 医学类期刊 |
| 期刊 | **JAMA 风** | JAMA 色板（ggsci 收录） | 稳妥、低饱和 |
| 期刊 | **NEJM 风** | New England Journal of Medicine 色板（ggsci 收录） | 医学类期刊 |
| 期刊 | **JCO 风** | Journal of Clinical Oncology 色板（ggsci 收录） | 肿瘤类期刊 |
| 色盲友好 | **Okabe-Ito** | Okabe &amp; Ito 2008 色彩通用设计（Wong 2011, *Nat Methods*） | 有红绿色觉障碍读者时**首选** |
| 分类 | **柔和分类 Set2** | ColorBrewer 2.0 | 类别不多、想柔和一点 |
| 分类 | **Tableau 10** | Tableau 默认色板 | 通用、对比清楚 |
| 连续 | **viridis** | viridisLite | 数值渐变；**黑白打印也不糊** |
| 连续 | **蓝绿渐变 YlGnBu** | ColorBrewer 2.0 | 数值从浅到深 |
| 发散 | **红蓝发散 RdBu** | ColorBrewer 2.0 | 想突出"两端"，如高/低显著 |
| 发散 | **彩虹 Spectral** | ColorBrewer 2.0 | 同上，更鲜艳（慎用于正式投稿） |

用法提示：

- **柱状图类**（GO 柱状图、富集柱状图）里，颜色通常代表"类别"（BP/CC/MF 或通路），选**分类色**（期刊色板 / Okabe-Ito / Set2）最合适。
- **颜色深浅代表 p 值或度数**的图（气泡图、度分布、网络图节点色），选**连续色**（viridis / YlGnBu）。
- **柱状图一律不用色阶**（2026-10-23 起）：GO 柱状图的柱长已经表示基因数，
  再拿颜色深浅去编码 p 值属于重复表达；而且三个本体的 p 值量级可能差十几个数量级，
  共用一条色阶会让某些段整段变成一个颜色、内部差异根本看不出来。
  所以柱状图（文献横条 / 三合一分面 / 显著性排序）统一**按 BP/CC/MF 给三个颜色**，
  选哪套色板都只是换这三色，**不会**变成深浅渐变。
  红绿色觉障碍读者优先选 **Okabe-Ito**；投英文期刊优先用该刊同系色板。
- **有红绿色觉障碍读者**（评审、导师、听众）：优先选 **Okabe-Ito** 或 **viridis**，这两套是为色觉障碍专门设计的。
- **要投英文期刊**：优先用该刊同系色板（NPG / Science / Lancet / JAMA / NEJM / JCO），审稿人看着"眼熟"。
- 一套图里**尽量只用一套配色**，别一张红蓝、一张彩虹。

> 色板出处速查：
> - [ggsci（期刊色板 R 包）](https://nanx.me/ggsci/)
> - [ColorBrewer 2.0（分类/连续/发散色板）](https://colorbrewer2.org/)
> - [Okabe & Ito 色彩通用设计](https://jfly.uni-koeln.de/color/)
> - [viridis 色板说明](https://CRAN.R-project.org/package=viridisLite)

> 术语速查：
> - [GeneRatio 与 Rich Factor 的区别（clusterProfiler 文档）](https://yulab-smu.top/biomedical-knowledge-mining-book/enrichplot.html)
> - [如何读懂 GO 富集柱状图（YuLab 教程）](https://yulab-smu.top/ygc-blog/01_Development/documentation/clusterProfiler/GO/goall-barplot)

> 想更懂数据可视化术语：
> - [桑基图（Sankey diagram）— Wikipedia](https://en.wikipedia.org/wiki/Sankey_diagram)
> - [弦图（Chord diagram）— Wikipedia](https://en.wikipedia.org/wiki/Chord_diagram)

---

## 8. 数据从哪来？（可靠性与引用）

软件内置的知识库整合了多个公开数据库，做论文时**记得引用它们**：

| 数据库 | 内容 | 许可/说明 |
|---|---|---|
| TCMSP | 中药-成分-靶点 + ADME | ODbL 开放 |
| HERB 2.0 | 中药成分靶点 + 性味归经 | 公开下载（引用原论文） |
| BATMAN-TCM 2.0 | 成分-靶点（known/predicted） | 公开下载 |
| ETCM2 | 方剂、成分 | 注册使用 |
| TTD | 靶点-疾病关联（**仅内部交叉校验**） | **All rights reserved，不随本软件分发**；其作用机制信息改由 ChEMBL 提供 |
| ChEMBL | 成分-靶点交叉验证 | CC BY-SA 3.0 |

> 各数据库官网：
> - [TCMSP 官网](https://old.tcmsp-e.com/tcmsp.php)
> - [HERB 数据库](http://herb.ac.cn/)
> - [BATMAN-TCM](http://bionet.ncpsb.org.cn/batman-tcm/)
> - [ETCM（中医百科全书）](http://www.tcmip.cn/ETCM/)
> - [TTD 治疗靶点数据库](https://db.idrblab.net/ttd/)
> - [ChEMBL](https://www.ebi.ac.uk/chembl/)

---

## 9. 常见问题（FAQ）

**Q1：双击 exe 没反应 / 弹窗闪退？**
先等几秒——第一次加载数据较慢。若一直没反应，检查杀毒软件是否拦截（把文件夹加入信任区）。仍未解决，看解压目录下是否有 `logs/` 文件夹，把里面的日志发给维护者。

**Q2：富集分析点了很久没结果？**
KEGG 富集需要联网抓取数据，第一次约 30–90 秒，之后会缓存、变快。GO/Reactome/WikiPathways 是离线的，不联网也能跑。若 KEGG 一直失败，多半是网络问题，改用离线的那几类即可。

**Q3：输入药名查不到？**
试试拼音（如 `gancao`）、或换一种写法（如「甘草」vs「生甘草」）。内置库收录的中药名以 2025 版中国药典口径为主，个别别名可能需用规范名。

**Q4：结果能导出吗？**
能。查询表、图都能导出为 CSV / PNG / HTML，方便贴进 Word 或论文。

**Q5：这软件收费吗？**
不收费。项目以开源协议发布，数据遵循各来源许可（详见软件内「数据来源与许可」页）。

**Q6：我完全不懂网络药理学，能做出论文需要的图吗？**
能。按本手册第 3–6 步点一遍，就能拿到「交集韦恩图 + 富集图 + 网络图」这一套论文常用图。但**结果的生物学解读建议请教学科导师**——软件负责算和画，怎么讲好故事需要专业判断。

---

## 10. 我需要帮忙 / 反馈

- 软件有问题 → 记下操作步骤和截图，联系项目维护者（Zheng JianXiao，天津中医药大学海河实验室）
- 想了解项目技术细节 → 看 `docs/` 下的 `WORKFLOW.md`、`DATA-README.md`

---

*手册版本：随 0.2.0（2026-10）发布。*
