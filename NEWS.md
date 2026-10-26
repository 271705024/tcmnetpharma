# NEWS

## 未发布（2026-10-24）

### ⚠️ 破坏性重构：富集「来源」升格为一等公民（R1）

> **本节推翻 2026-10-23 的「最小侵入」修法**。当时的做法是把 `source` 回填进
> `ONTOLOGY` 字段，让下游 `group_by` / `fill` / `facet` 不用改。boss 2026-10-24
> 实测证伪：数据是 WikiPathways 时，标题仍写 `GO enrichment of cluster`、
> 图例写 `Ontology: wikipathways`。**省下的改动成本最终由用户支付。**
> 诊断与调研见 `docs/plot-module-audit-2026-10-24.md`。

#### 新增

- `tcm_enrich_table()`：把 `tcm_enrich()` 的返回（enrichResult 列表）/ 单个
  enrichResult / data.frame 统一成标准表。`source`（go/kegg/reactome/wikipathways）、
  `source_label`（GO/KEGG/Reactome/WikiPathways）、`ontology`（BP/CC/MF）
  **三列相互独立**；非 GO 来源的 `ontology` 一律 `NA`。
- `enrich_barplot()`：统一富集条形图。形制 / 条目数 / 去冗余 / 外观是**四个正交
  维度**，不是一个大枚举：
  - `layout`：`segmented`（默认，单轴按分组分段，段内各自排序）/
    `facet`（三栏分面）/ `flat`
  - `measure`：`neglogp` / `count` / `generatio`
  - `top`：数字或预设 `compact`=5 / `standard`=10 / `full`=20
  - `title = NULL` 时**从数据派生**（如 `KEGG pathway enrichment`、
    `GO enrichment (BP / CC / MF)`）
- 新文件：`R/enrich-source.R`（来源建模）、`R/enrich-order.R`（分组排序正交）、
  `R/enrich-barplot.R`（统一条形图）

#### 变更（破坏性）

- `go_barplot()` / `go_dotplot()` / `go_lollipop()` 的 `title` 默认值由字面量
  `"GO enrichment of cluster"` 改为 `NULL`（**从数据派生**）。
  需要旧行为时显式传 `legacy_title = TRUE`
- `go_barplot(style = "classic")` / `"significance"` 现在**按本体分段**，
  不再按数值全局排序（以前 BP/CC/MF 交错）
- `go_barplot()` 的 `plot.set` 只接受 `"vertical"`；传 `"horizontal"` 给警告
  并仍按水平条绘制
- 三个函数的 `...` 现在对未识别参数**发警告**（以前静默吞掉 → 假通过，
  铁律 28）
- `go_lollipop()` 的 `top` 语义由「原始行序前 N 条」改为「按 `p.adjust` 最显著的 N 条」

#### 修复

- `go_lollipop()` 换 KEGG / Reactome / WikiPathways 数据即崩（无 `ONTOLOGY` 列）
- `go_lollipop()` 的 `drop_na()` 会整批删掉非 GO 行
- `go_lollipop()` 的 `factor(levels = 含重复向量)` 在跨来源撞名时报 duplicated levels
- BP / CC / MF 混排（`classic` / `significance` 分支的全局 `order()`）
- 标题身份错标 + 图例 `Ontology: <source>` 语义污染

#### 移除

- `.tcm_inject_group_key()`（内部函数）：把 `source` 回填进 `ONTOLOGY`，是语义
  污染的源头。原位置保留原因说明注释，避免后来者重复踩坑。

### ⚠️ 破坏性重构：网络图拆成「数据/形状轴 × 外观轴」（R2）

> **动机**：`tcm_net` / `ppi_plot` 各自实现了「构图 → 裁剪 → 配色 → 标签 →
> 图例」，同一类缺陷在三个函数里各犯一次（双绘边、图例档位失效、size 通道被
> 错标度接管）。收敛到 `R/net-style-layer.R` 一层后，视觉契约只有一个事实源。

#### 新增

- `tcm_net(view = )` —— **数据/形状轴**，与 `style` 正交：
  - `"overview"`（**新默认**）：裁到 50 节点、颜色只区分节点类型、只标 top15
  - `"cluster"`：walktrap 社区着色、**只标簇名**（`M1`/`M2`…，画在质心）
  - `"hub"`：度数分档着色 + `keep = "ego"` 保留核心节点及一跳邻居、标 top10
- 裁剪报告挂在返回值上：`attr(p, "cap_report")` = `list(shown_nodes=,
  total_nodes=, shown_edges=, total_edges=, capped=)`，供 GUI 显示
  「已显示 49 / 125 个节点」
- 新文件 `R/net-style-layer.R`（`.tcm_net_plot()` / `.tcm_net_cap()` /
  `.tcm_net_view_presets()` / `.tcm_net_style_presets()` / `.tcm_resolve_cols()`）

#### 变更（破坏性）

- **`tcm_net` 不再双绘边**：旧 `style = "default"` 同时画 `geom_edge_link0`
  （直线，按 weight）和 `geom_edge_fan`（弧线，按 score），同一批边画两遍。
  现在**任何 view / style 都只画一层边**，边宽恒定（边不承载统计信息）
- **`tcm_net` 图例从 4 个收敛到 1 个**：旧图有 degree(点大小) / weight /
  class / degree(色条) 四个图例，吃掉约 1/4 画布
- `tcm_net(max_nodes = )` 默认值由 `150` 改为 `NULL`（= 用 view 自带上限：
  overview/cluster 50、hub 20）
- `tcm_net(graph.layout = )` 默认值由 `"kk"` 改为 `"auto"`（= 用 fr + 固定种子）
- `tcm_net(edge.width = )` **失效**：边宽现由 `style` 决定并恒定。保留形参只为
  向后兼容，显式传入时给出提示
- `venn_plot()` / `tf_cirplot()` **不再按「两列的基数谁大」猜列名**。改为
  「显式参数 > 列名精确匹配 > **报错并列出候选列名**」，新增
  `gene_col` / `source_col`（venn）与 `tf_col` / `target_col`（tf）参数。

#### 修复

- **`venn_plot()` 静默颠倒基因列与来源列**：旧判据在两列基数反转时（如 3 个
  基因 × 5 个来源）把两列对调，`ggvenn` 的集合名变成基因名，**不报错**。
  实测证据见 `logs/_probe_r2_defects.R`
- **`tf_cirplot()` 同上**（TF 与 Target 可被静默对调）
- **`ppi_plot()` 的 `node.size` 完全无效**：标度写成
  `scale_linewidth_continuous(range = node.size)`，管的是 linewidth 通道，
  而节点映射的是 size 通道 → `c(1, 10)` 与 `c(6, 30)` 出图的点大小**完全相同**
  （实测都是 1~6）。改为 `scale_size_continuous(range = node.size)`
- **`ppi_plot()` 有一个「幽灵图例」和一个「说谎图例」**：`linewidth` 标度
  无人使用却产出一个标题为 `degree` 的图例；`aes(colour = score)` 被外层
  `color = edge.color` 压成常量，但 colour 标度仍被注册 → 图上出现一个
  `score` 渐变色条，而没有任何一条边用这个颜色。现在图例从 3 个降到 1 个
- **`ppi_plot()` 的 `style` 图例档位全部失效**：`geom_node_point(show.legend =
  FALSE)` 写死，`legend = "degree"` / `"all"` 两档一个图例都出不来。改为按预设
  开关，`"all"` 会同时出 degree 色条与点大小图例
- **`venn_plot(type = 2)` 在来源数 > 5 时崩溃**：
  `colorRampPalette(brewer.pal(n, set.color)(...))` 把十六进制向量当调色板名用，
  必然报错。改为 `colorRampPalette(set.color)(n)`

### 新增：多行 y 轴标签不再重叠 + 画布尺寸建议（R2.4）

> 长富集条目（如 `GO:0006954 inflammatory response ...`）被 `str_wrap` 折成 2–4 行后，
> 以离散 y 位置**垂直居中**渲染，行数 × 行高 > 1 个 level 就压到邻居 —— 这是长期
> 存在的图面缺陷。诊断见 `docs/plot-module-audit-2026-10-24.md` §8.6。

#### 新增

- **`tcm_plot_size(p)`**：按**实际槽位数**（已含多行标签预留的空槽）与折行后
  单行最长宽度，算出建议的 `ggsave()` 尺寸，返回 `c(width=, height=)`。
  包层不知道用户最终画布多大，与其猜一个默认值，不如把「该用多大」算出来交还导出环节。
  ```r
  p  <- enrich_barplot(tcm_enrich_table(res), top = "full")
  sz <- tcm_plot_size(p)
  ggsave("out.png", p, width = sz[["width"]], height = sz[["height"]], dpi = 300)
  ```

#### 修复

- **多行 y 轴标签与相邻条目重叠**（`enrich_barplot` / `go_barplot` /
  `go_dotplot` / `go_lollipop` / `enrich_dotplot`）：给第 i 条分配 `k_i` 个 y 位置
  （`k_i` = 它的标签行数），在其上下插入空白占位行，使「占用位置数 = 行数」。
  柱宽仍是 0.8 个位置，坐标轴类型不变。同一份 24 条数据所需画布高度
  **19.20 in → 14.60 in**（省 24%），且间距按每条自身行数分配 ——
  单行条目不再被多行条目拖着占 3 行的高度
- **y 轴在占位位置出现无标签的裸刻度线**：改用 `breaks =` 只对真实条目出刻度
- `layout = "facet"` 下 y 轴占位必须**按组**补（`drop = FALSE` 是全局锁 limits，
  会把别组的占位灌进每个面板，实测 16 个位置被撑到 48）。这类补行**只喂图层**，
  `p$data` 仍是真实条目

#### 变更

- `module_netplot()` 接入网络视觉层，转调 `.tcm_net_plot(view = "cluster")`：
  **首次有簇名标签与图例**（此前无标签、无图例，自带一套绘制代码）；
  新增 `node.size` / `label.size` / `style` / `palette` 四个参数。
  默认 `max_nodes = 150`（`tcm_net(view = "cluster")` 的预设是 50，两者渲染路径相同）
- cluster 视图从「标全部簇」改为「只标最大的 8 个簇」：16 个模块全标时小模块的
  质心标签会互相压，小模块仍着色、只是不出名字

### ⚠️ 破坏性变更：`ppi_plot()` 的 7 个 style 拆成 `view` × `style` 两轴

> 旧版把「选哪些节点 / 颜色表什么」（数据轴）与「长什么样」（外观轴）塞进了
> 一个 7 值枚举：`hub30` 偷偷改 `max_nodes`、`module` 偷偷改 colour 通道，
> 其余 5 个只改外观。于是「module + 大字号」「hub + 无图例」这类组合无法表达。
> 现拆成 **`view`（3 档）× `style`（5 档）= 15 种组合**。

#### 修复（P0，此前完全不可用）

- **`ppi_plot()` 只要触发节点裁剪就渲染失败**：`.ppi_cap_nodes()` 调
  `.tcm_filter_igraph()` 从边表重建图 → 顶点属性 `degree` / `size` 全部丢失 →
  `aes(size = size)` 报 `object 'size' not found`。
  `ppi_data` 恰好 72 个节点，只有默认 `max_nodes = 150` 因为**不触发裁剪**而侥幸正常；
  一旦 `max_nodes < 节点数`（含 `style = "hub30"` 的 30）就 100% 崩溃。
  修法：`.ppi_cap_nodes()` 改走 `.tcm_net_cap()` 并在裁剪后**重算** degree / size
- **裁剪后边的 `combined_score` 丢失**：同一路径下 `tcm_filter_network()` 只回传
  from/to 列，`graph_from_data_frame()` 只能从边表恢复属性 → `E(net)$score` 消失，
  「边宽按 score」静默退化成固定线宽（图能出，信息没了）。
  修法：`.tcm_filter_igraph()` 按 (from, to) 无序对把边属性跨重建搬回
  —— 4 个网络函数的调用方都受益
- **style 的默认标签数超发**：旧实现用「度数 ≥ 第 N 名的度数」当阈值，
  PPI 里度数大量打平时同分节点全进来 —— 实测 `cytoscape` 声称 15 实标 18、
  `presentation` 声称 20 实标 29。改为按**名字集合**取精确 N 个

#### 变更（破坏性）

- 新增 `view`：`"degree"`（按度数连续着色，默认）/ `"module"`（按 walktrap 社区着色）/
  `"hub"`（只留 30 个核心节点且全标）
- `style` 收敛为外观轴：`"default"` / `"journal"` / `"cytoscape"` / `"presentation"` /
  `"minimal"`。旧名 `"hub30"` → `view="hub"` + `style="presentation"`、
  `"module"` → `view="module"` + `style="default"`，调用时给出等价写法提示
- `node.size` / `label.size` / `edge.width` / `label.degree` 默认改为 `NULL`
  （= 由 `view`/`style` 决定）；显式传入则**覆盖**预设。旧版这 4 个参数对
  非 default 风格是静默无效的
- `graph.layout` 默认改为 `"auto"`（= 用 style 的推荐布局）。旧版把 `"kk"`
  既当默认值又当「未指定」的哨兵，导致显式传 `"kk"` 反而被忽略
- 节点渲染统一为**白描边圆点**（`shape = 21`），`default` 也如此 —— 旧 `default`
  是实心圆点。这是 2026-10-21 boss 反馈「想接近 Cytoscape」时确认的方向
- 三份基本同构的绘制代码（`default` 内联 / `rem.dis.inter=TRUE` 内联 /
  `.ppi_style_plot`）收敛为一份 `.ppi_plot_core()`（铁律 54）
- `style = "module"` 的标签数 10 → 8、节点大小 2–10 → 1–10；
  `style = "hub30"` 会多出图例（旧版无）

### ⚠️ 破坏性变更 + 三个静默错误修复：`venn_net()` 接入网络视觉层

> `venn_net` 此前自带一套绘制代码（三种边几何 = 三份 30 行的重复代码），
> 且**换个输入类就静默变空白/报错**。本轮接入 `.tcm_net_plot()` 并修掉三个缺陷。

#### 修复（三个静默错误）

- **猜列导致 `as.data.frame()` 后整张图渲染失败**（严重）。旧实现用
  `length(unique(data[, 1])) > length(unique(data[, 2]))` 猜哪列是 from/to，
  而这个判据**在 tibble 与 data.frame 上取值不同**：1 列 tibble 的 `length()`
  是列数 1，data.frame 单列取出来是向量、`length()` 是行数。实测 `venn_data`
  （gene 149 / source 4）：
  | 输入 | 判据 | 走的分支 | 结果 |
  |---|---|---|---|
  | `tibble` | `1 > 1` = FALSE | `c("from","to")` | ✅ 正确（**纯属侥幸**） |
  | `as.data.frame()` | `149 > 4` = TRUE | `c("to","from")` | ❌ 边按**基因名**着色 → `Insufficient values in manual scale. 149 needed but only 5 provided.` |

  2026-09-15 的 p42 报告把这归因为「适配层传了 `as.data.frame`」，**归因是错的**：
  缺陷就在包内这一行。现改为 **显式 `gene_col` / `source_col` > 列名别名 > 位置**
  三级解析，不再按基数猜
- **`edge.type` 非法值静默返回 `NULL`**：函数体是三个 `if / else if` 且没有
  收尾分支，传 `"xxx"` 既不报错也不出图。改为 `match.arg()`，非法值直接报错
- **来源数 > 5 必定渲染失败**：`scale_edge_color_manual(values = edge.color)`
  写死 5 色，第 6 个来源开始就 NA，最终报 `Insufficient values`。改为按来源数
  取足（`edge.color` 给少了自动插值补足，`NULL` 时用定性色板）

#### 变更（破坏性）

- `venn_net()` 的默认外观全部改变：节点颜色由 `degree` 渐变改为**角色**
  （gene / source）定性两色；边颜色改为**按来源**并**补上图例**（旧实现
  `show.legend = FALSE` → 上了色却不给图例，读者无法解码）
- `node.color` **已废弃**（原本是 `RColorBrewer` 色板名，用于 degree 渐变）。
  传入会警告并被忽略，改用 `palette =`
- `label.degree` 默认值由 `1`（= **全部**节点都标）改为 `NULL`（= 标 top 15）。
  153 个节点全标会糊成一团；需要旧行为请显式传 `label.degree = 1`
- 新增 `style`（外观轴，与 `edge.type` 几何正交）、`palette`、`gene_col` /
  `source_col`、`max_nodes` / `min_degree` / `keep`、`edge.type = "link"` 选项

#### 共享层同步修复（影响 `tcm_net` / `module_netplot` / `ppi_plot`）

- **`create_layout()` 写死 `niter = 3000`**：igraph 里只有 `fr` / `graphopt`
  接受该参数，其余 8 种布局（kk / nicely / circle / lgl / mds / tree / star /
  grid）全部报 `unused argument`。R2 集中布局时引入，`tcm_net(graph.layout="kk")`
  之前会直接报错。改为按报错文本**精确回退**（不吞真错误）
- **`label_top` 用度数阈值实现，度数打平时严重超发**：二分图里 149 个基因多是
  degree 1–2，第 15 名的阈值会把同分节点全部放进 → 实测 `label_top = 15`
  标了约 **40 个**。改为按**名字集合**精确取 N 个（同分按名字排序，可复现）
- `geom_node_label(label.size = 0.1)` 在 ggplot2 4.0 起被**静默忽略**
  （边框参数已改名 `linewidth`），改为 `linewidth`


### 修复：桑基图爆炸 + 弦图标签重叠 + 富集结果按来源拆分（boss 四反馈）

- **桑基图节点爆炸**（boss 反馈）：旧实现 `make_long(colnames(data_sankey))`
  把**所有列**当层级，内置 `sankey_data` 有 4 列（herb / molecule_id /
  molecule / target），`molecule_id` 是 MOL 编号、频率=1，使 `max_per_level`
  的 top-N 过滤失效、节点数爆炸。修法：
  - 包层 `tcm_sankey()` 新增 `max_nodes = 60` 总量硬闸：分层过滤后端点总数
    仍超限则**逐层收紧** `max_per_level`（每次减半，下限 1）直到达标
  - GUI 侧新增「桑基列」语义三列投影（herb→molecule→target），把 MOL 编号
    等中间列在喂给包函数前剔除；「节点总数」滑条（20–200，默认 60）+ 四档
    预设按钮（40/60/100/150）
- **弦图标签重叠**（boss 反馈）：`pathway_ccplot()` 把最外层（leaf）所有基因
  标签全画。修法：新增 `max_gene_labels = 40`，按基因度数（出现在多少条通路）
  降序只标 top N 个 leaf 标签，其余只画点；字号随标注数自适应。GUI 侧加
  「通路条数」top 预设（5/10/15/20）+「基因标签数」滑条与预设（20/40/80/120）
- **富集结果按来源拆分**（boss 反馈「合一块去了」）：GO/KEGG/Reactome/WikiPathways
  各自独立多重校正、背景注释集不同，本就不该混进同一张图。GUI 富集结果现按
  `source` 列拆成**多条独立数据集**，富集可视化区加「来源」下拉，跳转时按所选
  来源单独出图
- **模块着色网络图引导文案**（boss 反馈「找不到数据」）：拓扑页加说明——该图
  用上方「数据来源」选的 herb/molecule/target 三元组着色，需先运行拓扑分析

## 未发布（2026-10-23）

### 变更：柱状图不再用色阶，改为按本体三色（气泡图保留色阶）

- **决定**（boss 2026-10-23）：「气泡图类用色阶，柱状图不用色阶了，按 BP/CC/MF
  分类给 3 个颜色」。`go_barplot()` 的 `grouped` 与 `significance` 两个形制
  原本用 `scale_fill_gradientn` 渐变上色，现全部改为
  `scale_fill_manual()` —— 颜色只表类别，**柱状图里不再出现任何连续色阶**
- **为什么**：
  1. 柱长已经承载主量，再用颜色编码同一/相邻量属**重复编码**。`significance`
     更极端 —— 长度与颜色是**同一个变量** `-log10(p.adjust)`，纯冗余
  2. 渐变只能用连续色板，但 GUI 配色下拉里 13 套色板是按 `kind` 分组、**不限制
     混选**的。选到 `okabe` / `npg` 这类**分类色板**时，插值出的渐变色相不单调
     （实测成图：橙→黄绿→紫→黑来回跳），"颜色深浅 = p 值"这句话就不成立
- **形制变化**：
  - `grouped`：从 `patchwork` 三段独立色阶 **改回 `facet_grid` + 三色离散标度**
    （三段共用一个离散标度，不需要独立标度）。**返回类型回到普通 ggplot**，
    `p$data` 重新是完整数据；只有一条图例（三个色块），与文献里最常见的
    "GO Results of Three Ontologies" 一致
  - `significance`：横轴仍是 `-log10(p.adjust)` 并按显著性排序，只是颜色改成
    本体三色。它的独特价值落在"x 轴口径是显著性而非基因数"上
- **气泡 / 点图（`go_dotplot()` 等）保留色阶**：这类图里颜色编码的是另一个维度
  （`p.adjust`），与大小（`Count`）不重复，用色阶是对的
- `go_barplot()` 的 `palette` 形参语义收窄：只取色板前 3 色给 BP/CC/MF，
  **不再驱动渐变**；对柱状图而言 seq / div 色板与 qual 色板效果等价
- 移除 `patchwork` 依赖（上一版为分段独立色阶引入，现已不需要）
- ⚠️ `bar_plot()`（富集柱状图，已从 GUI 下线）**未改动**：它的数据里没有
  `ONTOLOGY` 列，无法按本体三色；且不在界面上暴露

### GUI：绘图页下线「富集柱状图」（`bar_plot`）

- 与富集页的 `go_barplot` 数据同源（富集结果会 `pushDataset` 进绘图页），而
  `go_barplot` 能力更强（本体三色 / BP-CC-MF 分段 / 文献横条·显著渐变 /
  GeneRatio / 配色体系），`bar_plot` 都不具备 → 留着只会让人选错
- **包 API 保留**（`R/bar_plot.R` 未删），只是绘图页不再暴露
- 绘图页保留 `cir_plot`（富集环形条形图，无对应专页图）

## 未发布（2026-10-22 批次，与上节同属一个未发布版本）

### 出图风格预设：GO 富集文献形制 + 通用期刊/演示风

- **`go_barplot(style = )`** 新增 6 种预设（**只改视觉，条目与统计量完全一致**，见 `tests/testthat/test-style-presets.R`）：
  - `classic`（新增）文献最常见形制：横条长度 = 基因数 `Count`，按本体（BP/CC/MF）三色，条端标注 Count
  - `grouped`（新增）"三合一图"：BP/CC/MF 分面，长度 = `Count`，颜色 = 本体三色
  - `significance`（新增）长度 = `-log10(p.adjust)`，单图按显著性排序，颜色 = 本体三色
  - `journal` / `presentation`（新增）通用期刊风 / 演示风
  - `default` 保持原有输出不变
- **`go_dotplot(style = )`** 新增 `genratio`（横轴 `GeneRatio`，clusterProfiler 经典口径，文献绝大多数气泡图用此口径）、`journal`、`presentation`；`default` 仍为 Rich Factor。GeneRatio 缺失时自动退回 Rich Factor 并给出 message（不静默丢点）
- **`bar_plot` / `degree_plot` / `tcm_sankey`** 新增 `style = c("default","journal","presentation")`，统一走 `R/style-presets.R` 的 `.tcm_style_apply()`（风格单一事实源）
- **GUI**：富集可视化新增「出图风格」下拉（文献横条 / 三合一分面 / 显著性渐变 / GeneRatio 气泡 / 期刊风 / 演示风）；绘图页的风格下拉改为**按图型动态重建**（此前只有 4 个网络风选项且提示"仅网络类图生效"）
- **契约**：`plot_contract` / `enrich_plot_contract` 新增 `styles` 字段（风格白名单单一事实源），前后端一致由 `test-style-presets.R` 双向断言
- **防御**：非法风格值在后端被 `.sanitize_style()` 丢弃并记 `style_dropped` 日志，不再因 `match.arg` 报错中断出图

### 配色体系：期刊色板 / 色盲友好 / ColorBrewer（同一版本）

- **新增 `R/palettes.R`** 作为配色单一事实源，登记 **13 套色板**，每套都带「出处」字段（`source`）与类型（`kind` = qual/seq/div）：
  - 期刊/机构色板（取自 **ggsci** 收录的同名色板）：`npg`（Nature 系列）、`aaas`（Science/AAAS）、`lancet`（Lancet Oncology）、`jama`、`nejm`、`jco`
  - 色盲友好：`okabe` = **Okabe & Ito 2008 Color Universal Design** 全 8 色（Wong 2011, *Nat Methods* 8:441, doi:10.1038/nmeth.1618）
  - 通用分类：`set2`、`tableau`
  - 连续/发散（取自 **ColorBrewer 2.0** / viridisLite）：`viridis`（色盲友好且可灰度还原）、`rdbu`、`spectral`、`ylgnbu`
- **新接口**：`tcm_palette(name, n, type)` 取色（超出原生颜色数自动插值并提示）；`tcm_palette_names(names_only = FALSE)` 返回带 label/short/source/kind 的目录
- **接入 `palette = ` 参数的 7 个绘图函数**：`go_barplot`、`go_dotplot`、`bar_plot`、`degree_plot`、`tcm_sankey`、`tcm_net`、`ppi_plot`。缺省 `NULL` = **行为与旧版完全一致**
- **修掉一处非确定性**：`tcm_sankey` 原先用 `sample(mycol, ...)` 取色 → **同一数据两次出图颜色不同**；现改为按节点数确定性取色（`test-palettes.R` 有回归锚点）
- **GUI**：绘图页与富集可视化各新增「配色体系」下拉，按分类/连续/发散分组显示，选项 label 用中文名、`title` 显示色值出处；空值 = 默认配色
- **契约**：`plot_contract` / `enrich_plot_contract` 新增 `palettes` 字段；`app_info` 新增 `palette_catalog`（name/label/short/source/kind），前端据此渲染分组
- **三方一致性由测试守**：前端常量 ↔ 契约 ↔ 包内色板表（`gui-app/tests/test-style-presets.R` F 段），并对「有 palette 形参却没登记进契约」做**导出函数反查**（否则前端下拉置灰、用户点不到 = 等于没接）
- **防御**：非法配色值由 `.sanitize_palette()` 丢弃并记 `palette_dropped` 日志，不把「配色不存在」升级成「绘图失败」
- `DESCRIPTION` 新增 `viridisLite` 依赖

### 文档澄清

- **`go_barplot(top = )` 的口径**：`top` 是「**每个本体**各取 top 条」，不是全局 top。因为入选前先按 `ONTOLOGY`（BP/CC/MF）分组再切片，`top = 8` 最多给出 `8 × 3 = 24` 条（某本体显著条目不足时更少）。原文档只写 "the number of categories to show"，易被读成全局条数 → 已补明，并加断言锁定（`test-style-presets.R`），防止未来被"顺手改成全局 top"。


## TCMNP 0.2.0（数据修订 2026-10-21，数据版本 TCMNP_1.3.0_2026-10-21）

### CID 桥接假阴性修复（2026-10-21）
详见 `docs/xuanfuhua-uc-precheck/CID桥接假阴性_缺陷归属分析.md` 及其补记、R3 决策文档。

- **R1｜InChIKey 规范化桥接**：`02d_layer_a_align.py` 的 `build_mol_bridge()` 增加 InChIKey 分支（新增 `match_by_inkey` / `layer_a_inkey` 列），不再仅靠裸 CID 等值匹配
- **R2｜补映射表扩充**：`cid_inkey.parquet` 34,570 → **41,100** 行（目标集由 35,031 扩为 **43,170** 多源并集）；BATMAN 全集 35,336 CID 缺口 766 → **555**
- **R2-b｜known 边补映射（报告未发现）**：`03_merge.py` 对 known 边按官方 CID→InChIKey 补齐 → known 边 InChIKey 13,137 → **16,801**；`UNMAPPED_batman_known` 21,269 → **1,068**（救回 20,201 行）；`raw_unmapped` 211,830 → **191,629**
- **R3｜分子实体别名通道**（新增 `mol_entity_alias.parquet`，**39,388** 行）：BATMAN 与 HERB 对同一分子使用不同立体形式 InChIKey（前 14 位连接性层相同）→ 此前按名查不到。新增 `molecule_serch(..., resolve_alias = TRUE)`（**默认开启**）：未逐字命中的名字经别名表解析为连接性层 `entity_key`，查询扩展至该实体的**全部**立体异构体
  - 效果：`molecule_serch("Britannilactone")` 0 → **67** 行；`molecule_serch("Inulicin")` 0 → **51** 行
  - **向后兼容**：仅在原有路径**完全查不到**时启用；原路径可查的名字不被改写（`quercetin` 仍为 106,020 行）
  - **数据侧零改动**：`standard` / `tcm_network_compat` 行数与列契约**完全不变**（16,269,134 / 16,202,124）
  - ⚠️ 曾评估「把 HERB 规范名回填进 `mol_name`」方案并**实测否决**：会覆盖 7,912,535 行既有名称、compat 塌缩 94,705 行、可检索实体反降 2,420
- **方法学教训**：跨库分子桥接一律走 **InChIKey**（或前 14 位连接性层 + 相似度），**禁止裸 CID 等值匹配**

### ETCM2 未消费数据接入（2026-10-21）
- **ETCM2 4 张已解析未消费表接入**（`02e_etcm2_align.py` 非破坏性对齐；粒度为二元组，**不进 standard 三元组**，行数守恒 16,269,155 不变）：
  - `etcm2_herb_property.parquet`（2,079）药材性味归经；herb_dict 补空缺 Properties 378 / Meridians 367 / **新增 Flavor 列 1,511**（旧非空值 0 覆盖）；命中 92.2%
  - `etcm2_mol_adme.parquet`（38,298）成分 QED/FDAMDD/MW；成分名精确匹配 mol_dict 40.0%（同名多 InChIKey 标 ambiguous 不硬配）
  - `etcm2_herb_target.parquet`（2,836）药材→靶点二元组（CamelCase 拼音归一）；靶点命中 97.5%
  - `etcm2_disease_gene.parquet`（24,106）疾病→基因二元组；基因命中 99.9%
- **TTD 处置**：不进对外分发（All rights reserved）；其 MOA/研发阶段增值改由 **ChEMBL `drug_mechanism`/`drug_indication`** 提供（开放许可）
- **文档纠错**：修正 `DATA-README.md` 中 TTD 版本/许可/用途三项失实、ETCM2 用途不全；`DESCRIPTION`/`TCMNP-package.Rd` 源列表移除 TTD、补 ChEMBL
- 数据版本统一 `_2026-10-12` → `_2026-10-21`（`verify_data_readme` 版本比对稳健化）

## TCMNP 0.2.0（数据修订 2026-10-14，数据版本 TCMNP_1.3.0_2026-10-21）

### Layer A 一次源融合（去依赖重建）
- 一次性独立源 **Open Targets 25.06（CC0, EBI FTP）** 与 **SymMap V2.0** 全量接入；关系层 herb-mol-target 由 HERB 2.0 / BATMAN 2.0（已冻结、开放许可）兜底，形成「半一次依赖」骨架（合规底线：DrugBank / TTD / 药典不进对外分发产物）
- `standard_herb_mol_target` 宽表新增 2 列一次源交叉引用：`layer_a_latin`（SymMap 草药拉丁名，53.3% 行命中）、`layer_a_hit_id`（SymMap/HIT 实验验证靶点标记，30.7% 行命中）
- 新增独立参考表 `layer_a_disease_ontology_map.parquet`（7,157 行，MeSH/UMLS 本体 ID），供下游疾病-靶点/适应症模块消费
- 全流程产物指纹接入 `run_pipeline` 状态持久化（`pipeline_state.json`）；06_export 导出纳入该表并经 `verify_data_readme()` 守护
- 详见 `docs/data-source-registry-2026-10-12.md`、`docs/data-source-independence-roadmap-2026-10-12.md`

### 疾病维度（2026-10-15）
- 新增导出函数 **`disease_serch()`**：按疾病名 / MeSH / UMLS / SymMap 疾病 ID 查疾病本体（读 `disease_main.parquet`，14,434 行；多值本体 ID 走**边界匹配**，默认**精确**）
- 新增导出函数 **`disease_target()`**：由疾病查到**关联靶点**（读 `disease_target.parquet`，3,134,397 行 = OpenTargets 25.06 `association_overall_direct` 经 MeSH/UMLS 桥接 SymMap 疾病）；保留 OpenTargets 原始 `score`（0–1）与 `evidence_count`，`min_score` 默认 0 **不设隐式阈值**
- 新增数据表 `disease_main.parquet` / `disease_target.parquet`；`layer_a_latin` 拉丁名**无损归一化**（内嵌换行→空格、全角→半角、罗马数字 `Ⅴ`→`V`；共修正 237,396 行）
- 详见 `docs/analysis-module-wiring-2026-10-14.md` §7–§9

### 通路富集：WikiPathways（CC0）替代 KEGG，KEGG 改为联网增强（2026-10-17）
- 新增离线基因集 **`human_wikipathways` / `mu_wikipathways`**（人 991 / 鼠 214 条通路，来源 data.wikipathways.org `20261010`，**CC0 公有领域**，可随软件分发）
- 新增 **`tcm_enrich()`**：GO / Reactome / WikiPathways 端到端富集，默认三者全跑，**全部离线可用**
- **KEGG 不再随包分发**（学术自用免费，但随软件再分发需另行授权，见 `docs/licensing-kegg-go-2026-10-19.md`）；
  需要时联网运行 `clusterProfiler::enrichKEGG()`，返回的 `enrichResult` 可直接交给本包的可视化函数
- 说明见 `inst/extdata/DATA-README.md` §可再分发基因集（含「关于通路富集」一节）

### STRING PPI 接入：`tcm_ppi()`（2026-10-20）
- 新增导出函数 **`tcm_ppi(targets, min_score = 700)`**：把**靶点列表**（交集 / 查询结果）
  转成 **PPI 边表**（`node1` / `node2` / `combined_score`），产物可直接喂 `ppi_plot()`。
  存在的理由：PPI 需要**配对**，而靶点列表只说"有哪些基因" —— 这是两种不同的数据形状。
- 新增数据表 **`inst/extdata/string_ppi_human.parquet`**：STRING v12.0 人类
  **physical subnetwork**（有实验依据的物理互作），`combined_score >= 700`（high confidence），
  蛋白 ID 已映射为 HGNC symbol。
  - 为什么用 physical 而非 full network：full 含共表达 / 文本挖掘等**预测关联**（79 MB），
    physical 只有 8.5 MB 且语义更准 —— 讲"蛋白互作"就该用物理证据。
- GUI：交集页新增「生成 PPI 子网」按钮；产物入数据集总线（shape = `ppi`），
  绘图页可直接选「PPI 蛋白互作网络」。
- **许可**：STRING 数据为 **CC BY 4.0**，分发需署名。本包分发的是**经筛选与符号映射的
  派生子集**（非原始文件）。完整声明与引用见 `docs/data-license-string-2026-10-20.md`，
  GUI「关于」页亦有说明。

### 药物 × 疾病 交集靶点 + 富集阶段进度（2026-10-19）
- 新增导出函数 **`tcm_intersect_target()`**：网络药理学主线第一步 —— 取「药物靶点」与「疾病靶点」的**交集**。
  两侧都已是 gene SYMBOL 口径（药侧 `target` / 病侧 `target_symbol`），**不需要额外映射表**。
  返回每个交集靶点的药侧支撑分子数（`n_molecule`）与病侧最强证据（`disease_score` / `evidence_count` / `n_ot_disease`）；
  聚合取 **max** 而非 sum（sum 会把「多个疾病各一条弱证据」抬成强证据），来源数另用 `n_ot_disease` 如实记录；
  **0 结果也返回同结构 0 行**，下游无需特判 `NULL`。
  - 实测：麻黄 × Asthma → **1,115 个交集靶点**（`ADORA1` 居首，score 0.728），约 3 秒
  - ⚠️ 疾病靶点来自 Open Targets（人类），故交集为人类 gene symbol；小鼠数据不适用
- **`tcm_enrich()` 新增可选 `progress` 回调**（`function(pct, message)`）：在 GO / Reactome / WikiPathways
  各阶段回报 25 → 50 → 60 → 78 → 82 → 95 → 100。包本身**不依赖 GUI**（默认 `NULL`），
  回调抛错不影响主流程。
  - 背景：富集耗时几乎全是**固定开销**（实测 8 / 30 / 100 / 200 个基因均为 20–24 秒，
    主要花在加载 `clusterProfiler`/`DOSE` + 基因集），没有阶段反馈时用户无法区分「在算」与「卡死」

## TCMNP 0.2.0（数据修订 2026-09-28）

### 数据质量修复（B1/B2/B3/B5）
- **B1 名称守卫**：`mol_name`/`iupac_name` 不再接受 `CID <数字>` 占位符与 CAS 号（原致 11,649 行成分名污染）；守卫收敛到 `cleaning/common.py`，构建器与回填表共用同一实现
- **B2 靶点符号补全**：BATMAN predicted 引用的 9,491 个 Entrez 中 1,090 个不在 HERB 靶点枢纽 → 经 `org.Hs.eg.db` 补全为 HGNC 符号（1,090/1,090，100%），消除 634,824 行空靶点
- **B3 compat 判重**：按对外暴露的 10 列判重（-67,010 行）
- **B5 物种过滤**：HERB 靶点枢纽剔除非人条目（187 个小鼠 + 4 个 741158），清除 398 行；非人基因符号命中 44 种
- **行数基准收敛**：三表行数收敛到单一声明点 `data-raw/cleaning/row_baseline.json`（原先在 `03_merge.py` 与 `tools/build_app.py` 各写死一份，改一处漏一处）
- 数据版本升至 `TCMNP_1.1.1_2026-09-28`；行数 16,269,134 / 16,202,124 / 4,988,060
- 详见 `docs/defect-fix-b1b2b3-2026-09-28.md`

### 新行为：查询口径「尽量全」（默认变更）
- `herb_target()` / `target_herb()` **默认保留源侧无 OB/DL 的行**，新增 `adme` 列标注（`ok` / `no_adme`）；原先这些行被阈值静默丢弃（compat 表约 80.33% 行属此类）
- 传 `include_missing = FALSE` 可回退旧口径
- `tcm_net()` 改为按**列名**取三元组（原先按列位置，遇查询结果的第 4 列 `molecule_id` 会错位报错）

## TCMNP 0.2.0（数据修订 2026-09-15）

### 数据质量修复（L3）
- `iupac_name`/`mol_name` 中的 CAS 号迁入独立 `cas` 列（`standard` 表新增第 23 列；`compat` 表扩为 10 列，新增 `CAS`）
- 分子名经 PubChem 回填（889 个 CID），`mol_name` 空值清零
- `mapping_status` 列语义修复（原先误写 InChIKey 值）
- `herb_cn_source` 溯源加守卫；数据版本升至 `TCMNP_1.1.0_2026-09-15`
- 详见 `docs/l3-data-defect-fix-report-2026-09-15.md`

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
