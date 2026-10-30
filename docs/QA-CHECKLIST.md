# TCMNP / tcmnetpharma — 测试与 CI 流程（QA 清单）

> 范围决策（2026-10-30 boss 拍板）：**只做「科学分发 + 开源 + 论文复现」，不投 CRAN**；
> CI **默认建在 Gitee**（实际可运行门禁经 Gitee→GitHub 镜像在 GitHub Actions 上跑）。
> 本文件为可勾选的收尾测试清单与流程设计，随进度更新状态。
> CI 文件位于本包仓库根：`.github/workflows/ci.yml` 与 `.gitee/workflows/ci.yml`。

---

## 1. 目标与范围

| 项 | 内容 |
|---|---|
| 交付形态 | 一站式 exe + 数据自动下载（非技术用户）；开源 R 包；配套论文 |
| 测试目标 | 复现性证据（CI 绿 = 包可在干净 Linux 跑通）、回归门禁、发布前验收 |
| **不做** | CRAN 准入（`--as-cran` 严格整改、全平台 devel 矩阵暂不做） |
| 当前状态 | 本机 `R CMD check` 修复后无 ERROR（WARNING/NOTE 为直接目录跑 check 的假阳性，已清）；covr **84.2%**；R 包 33 测试文件/320+ test_that 块全绿；GUI 19 用例；Python 9 单测；数据契约 E2E 通过 |

---

## 2. 测试分层映射（标准流程 → 本项目）

| 标准层级 | 本项目对应物 | 状态 |
|---|---|---|
| 静态分析 | R CMD check（已含）；lintr / goodpractice 门禁 | ⚠️ 缺 lintr（P2） |
| 单元测试 | R 包 33 测试文件/320+ test_that 块 + Python 9 单测 + GUI 19 用例 | ✅ 已具备（covr 实测全绿） |
| 集成测试 | GUI IPC 契约 (`gui-app/tests/test-ipc-contract.py`)、数据契约 (`tests/testthat/test-data-contract.R`) | ✅ 已具备 |
| 系统/端到端 | R 侧 `tests/testthat/test-smoke-all-exports.R`；**exe 分发链路** | ⚠️ exe 干净环境 E2E 缺（P0-2） |
| 验收测试 | 非技术用户干净环境一键跑通 | ⚠️ 缺（P0-2，手动门禁） |
| 兼容/多平台 | 仅本机 Windows；`R CMD check` 多平台 | ❌ 仅本机；CI 加了 Linux（P1 多版本待定） |
| 性能/负载 | 代表数据量级内存/耗时 | ❌ 未做（P2） |
| 回归 | 全量套件 + CI 每次跑 | ⚠️ 有套件、CI 已接（P0-1） |
| 发布/可复现 | SHA-256 清单 + 签名 tag + Zenodo 双 DOI | ⚠️ 工具在，缺自动校验测试（P1） |

---

## 3. CI 门禁设计（双文件，位于包仓库根）

仓库 push Gitee（`git@gitee.com:ZJX261/tcmnetpharma.git`）→ 自动镜像 GitHub。CI 采用两份一致文件：

- **`.github/workflows/ci.yml`（真正执行）**：`r-lib/actions` 在 `ubuntu-latest` 装 R（`release`）、apt 装系统库、`BiocManager`+`remotes` 装 CRAN+Bioconductor+Remotes 依赖、`rcmdcheck` 跑 `R CMD check`（`--no-manual`，`error_on=error`）+ 非阻断 covr 覆盖率。包在仓库根（`.`）。
- **`.gitee/workflows/ci.yml`（Gitee 侧主声明）**：纯 shell `run:`，在 Linux runner 上自行 apt 装 R 与依赖后跑同一套 check。**Gitee Go 不解析 `uses: actions/*`，故全部自包含**。需 boss 在 Gitee 网页开启 Gitee Go + 绑定 Linux runner。

**CI 顺带消除的本地地雷**：`rcmdcheck` 在临时库构建并检查包，`test_check("tcmnetpharma")` 跑的是刚构建的新包，而非本机可能残留的旧 `TCMNP`（见 `tests/testthat.R` 注释的脆弱依赖）。

---

## 4. 收尾测试清单（P0 / P1 / P2）

### P0 — 发布前必须过

- [x] **P0-1 CI 门禁**：`.github/` + `.gitee/` 双 YAML 已建；GitHub 侧自动跑 `R CMD check` + testthat。
  - 待 boss：① push 到 Gitee 验证镜像触发；② 在 Gitee 网页启用 Gitee Go（如需 Gitee 侧也跑）。
- [ ] **P0-2 干净环境 exe 端到端验收**（核心交付目标，尚未验证）：
  1. 取一台**全新/洁净 Windows 虚拟机**（无 R、无本包、无缓存）；
  2. 解压 `dist/tcmnetpharma-0.2.1-windows-final.zip`；
  3. 双击 exe → 确认 GUI 启动、`library(tcmnetpharma)` 自包含可用；
  4. 触发"首次运行自动拉取数据" → 确认 `tools/fetch-data.R` 从 Zenodo 拉取成功、落盘、sha256 校验通过；
  5. 跑一个最小分析（如单味药 → 网络图）出图，留日志/录屏。
  - 此步**需 boss 在干净机手动执行**（AI 无第二台机/无 exe 运行环境）。可提供 `tools/verify-exe-clean-env.ps1` 半自动脚本，待 boss 确认后补。

### P1 — 复现性增强（建议发版前）

- [x] **P1 覆盖率 covr**：`Suggests: covr` + `tools/coverage.sh` 已就位；**本机实测 84.2%**（最大盲区 `tcm_kegg.R` 8.5%，绘图类 50–70%，其余 85–100%）。交互报告见 `coverage-report/index.html`（或 `tools/coverage.sh` 复现）。KEGG 函数因不随包分发（运行时联网抓取）覆盖率低属预期。
- [ ] **P1 多版本 R**：CI 当前 `release`；论文复现建议锁 `4.6.1` 并在 matrix 加 `oldrel`。
- [x] **P1 数据完整性 E2E（本机部分）**：`tools/fetch-data.R` 已从 Zenodo 拉 19 个 parquet（184M）落 `inst/extdata` 并被 `.gitignore` 正确忽略；`test-data-contract.R` 隔离跑**全绿**（schema/维度/类型/关键链契约对真实数据成立）。SHA-256 比对 + 发布版本断言建议作为手动/定时任务（183MB 重，不进每次 push）。
- [ ] **P1 可复现/归档校验**：`tools/make-manifest.py` 产 SHA-256 清单 + 签名 tag；补一个测试断言"归档可还原"。

### P2 — 卫生与加固（可延后）

- [ ] **P2 lintr / goodpractice** 静态门禁。
- [x] **P2 性能基准**：代表链路实测（R 4.6.1，真实 184M 数据）：`disease_target` 0.29s/2505行、`tcm_intersect_target` 2.05s/1115交集、`tcm_ppi` 0.10s/1446边、`ppi_plot+ggsave` 1.97s、**`tcm_enrich` 三源离线 53.9s**（首次加载 clusterProfiler/DOSE 固定开销）、**峰值内存 ~776 MB**。16GB 非技术机可承受。
- [ ] **P2 secret 扫描 + 下载签名校验**：IPC/下载链路安全，发布前必过。

---

## 5. 铁律相关审计结论（发版前复查项）

- **铁律 91「随机夹具定时炸弹」**：✅ 已规避。所有 `runif/sample` 前均跟 `set.seed(...)`（helper-enrich-fixture、tests/testthat/test-smoke-all-exports.R、test-sankey-*.R、test-pathway-ccplot-render 等），确定性夹具，不会 flake。
- **铁律 90「遗留脚本失效」**：⚠️ `tools/fetch-data.R` 已实测可跑（拉 19 个 parquet 成功）；`data-raw/`、`tools/` 其余旧脚本仍须发版前扫一遍是否还能跑（CI 不覆盖）。
- **`tests/testthat.R` 旧包依赖**：⚠️ 本地依赖"旧包 TCMNP 已卸载"才能测对新包；**CI 已绕开**（临时库构建）；但本地跑测试仍须遵守其新鲜度守卫。

---

## 6. go-decision 记录

| 决策点 | 结论 | 时间 |
|---|---|---|
| 是否投 CRAN | 否（只科学分发+开源+论文复现） | 2026-10-30 |
| CI 建在哪 | 默认 Gitee；实际可跑门禁经镜像在 GitHub Actions | 2026-10-30 |
| 多平台 check | 暂不做全平台 devel 矩阵；CI 先上 Linux release | 2026-10-30 |
| 覆盖率门槛 | 不强制；先出报告看盲区 | 2026-10-30 |

**下一步（待 boss）**：① 刷新 GitHub Actions 看 commit `b485299` 是否转绿（已清 WARNING/NOTE）；② 在干净 Windows 机执行 P0-2 exe E2E；③ 确认是否锁 R 版本 4.6.1 / 补 `verify-exe-clean-env.ps1` / 接 P2 lintr 门禁。

---

## 7. 本机实测结果（2026-10-30 收尾，不依赖第二台机）

### 7.1 R CMD check 修复链路（CI 两次失败根因 + 修复）
| 提交 | 现象 | 根因 | 修复 |
|---|---|---|---|
| `7f81f7c` | Step5 依赖安装失败、run 仅 1m12s | GitHub Actions 共享 IP 拉 `ggsankey` 的 `Remotes` 触发 60次/时 API 限流 | 注入 `GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}`（额度 60→5000/时） |
| `c154dea` | `Required fields missing: Author/Maintainer` 致命 ERROR | R 4.6.1 check 元数据校验无法从 `Authors@R` 单独派生 author/maintainer（磁盘值有效、`read.dcf`+`eval` 可解析出含 `cre` 的 person） | 补显式 `Author:`/`Maintainer:` 字段 |
| `8fd154d` | `.github`/`.gitee` 误打进 tarball → hidden-files NOTE | `.Rbuildignore` 漏这两项 | 加 `^\.github$`/`^\.gitee$` |
| `b485299` | NEWS 未发布小节缺版本号 → WARNING；`.gitattributes` hidden NOTE | NEWS 解析需版本 token；`.gitattributes` 未排除 | 未发布小节补 `tcmnetpharma 0.2.1` 前缀；`.Rbuildignore` 加 `^\.gitattributes$` |

> 注：`rcmdcheck` 在 CI 先 `R CMD build`（应用 `.Rbuildignore`）再 check，故「LICENSE 未提及」「meta-information: should be prepared by build」「`..Rcheck`」等项均不会在 CI 出现——本机此前是直接对源码目录跑 check 产生的假阳性。

**现状**：本机修复后 `R CMD check` EXIT=0、无 ERROR；上述 WARNING/NOTE 在 CI 下将不再命中。`installed size 191.7 Mb`（含 183Mb 数据）为 INFO，非阻塞。

### 7.2 覆盖率（covr 3.6.5，本机 R 4.6.1）
- **总行覆盖率 84.2%**；320+ test_that 块全绿（covr 构建 + 跑测无失败）。
- 最大盲区 `R/tcm_kegg.R` **8.5%**（KEGG 不随包分发、运行时联网抓取，低覆盖属预期）。
- 绘图类 50–70%（dock_plot / etcm / venn_plot / enrich-order / enrich-source / alluvial_dot / tcm_sankey_dot / degree_plot / dot_sankey* / pathway_cirplot）；其余多在 85–100%。
- 交互报告：`coverage-report/index.html`（由 `tools/coverage.sh` 或 `covr::report()` 复现；可直接点开看每行未覆盖代码）。

### 7.3 数据完整性 E2E（本机部分）
- `tools/fetch-data.R` 从 Zenodo（TCMNP_1.3.0）拉 **19 个 parquet（184M）** 落 `inst/extdata`，被 `.gitignore` 正确忽略（不入库、随发布另分发）。
- `tests/testthat/test-data-contract.R` 隔离跑**全绿**：核心数据集 schema、compat/standard 列集、Layer A 两列语义、疾病本体映射表均对真实数据成立（关键链 `PTGS2` 可查、`Herb_cn_name` 去重 > 1000 成立）。

### 7.4 性能基准（R 4.6.1，真实 184M 数据，已安装包）
| 步骤 | 耗时 | 规模 |
|---|---|---|
| `disease_target('Asthma')` | 0.29s | 2,505 行 |
| `tcm_intersect_target('麻黄','Asthma')` | 2.05s | 1,115 交集靶点 |
| `tcm_ppi(min_score = 900)` | 0.10s | 1,446 边 |
| `ppi_plot` + `ggsave` | 1.97s | 50 节点裁剪出图 |
| `tcm_enrich(GO+Reactome+WikiPathways)` | **53.9s** | 三源离线富集（首次加载 clusterProfiler/DOSE 固定开销，预期） |
| 峰值内存 | ~776 MB | 16GB 非技术机可承受 |
