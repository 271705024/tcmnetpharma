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
| 当前状态 | `R CMD check` Status: OK（本机 2026-10-28）；34 个 testthat 用例；GUI 19 用例；Python 9 单测 |

---

## 2. 测试分层映射（标准流程 → 本项目）

| 标准层级 | 本项目对应物 | 状态 |
|---|---|---|
| 静态分析 | R CMD check（已含）；lintr / goodpractice 门禁 | ⚠️ 缺 lintr（P2） |
| 单元测试 | R 包 34 用例 + Python 9 单测 + GUI 19 用例 | ✅ 已具备 |
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

- [ ] **P1 覆盖率 covr**：已加 `Suggests: covr` + `tools/coverage.sh`；CI 已出非阻断覆盖率。待本地跑一次确认盲区。
- [ ] **P1 多版本 R**：CI 当前 `release`；论文复现建议锁 `4.6.1` 并在 matrix 加 `oldrel`。
- [ ] **P1 数据完整性 E2E**：对**实际发布数据版本**（Zenodo DOI）做 `fetch → SHA-256 比对 → schema 断言`，而非仅跑打包样例。建议作为手动/定时任务，不进每次 push（183MB 太重）。
- [ ] **P1 可复现/归档校验**：`tools/make-manifest.py` 产 SHA-256 清单 + 签名 tag；补一个测试断言"归档可还原"。

### P2 — 卫生与加固（可延后）

- [ ] **P2 lintr / goodpractice** 静态门禁。
- [ ] **P2 性能基准**：代表数据量级下内存/耗时基线。
- [ ] **P2 secret 扫描 + 下载签名校验**：IPC/下载链路安全，发布前必过。

---

## 5. 铁律相关审计结论（发版前复查项）

- **铁律 91「随机夹具定时炸弹」**：✅ 已规避。所有 `runif/sample` 前均跟 `set.seed(...)`（helper-enrich-fixture、tests/testthat/test-smoke-all-exports.R、test-sankey-*.R、test-pathway-ccplot-render 等），确定性夹具，不会 flake。
- **铁律 90「遗留脚本失效」**：⚠️ 仍须发版前扫一遍 `data-raw/`、`tools/` 旧脚本是否还能跑；CI 不覆盖这些。
- **`tests/testthat.R` 旧包依赖**：⚠️ 本地依赖"旧包 TCMNP 已卸载"才能测对新包；**CI 已绕开**（临时库构建）；但本地跑测试仍须遵守其新鲜度守卫。

---

## 6. go-decision 记录

| 决策点 | 结论 | 时间 |
|---|---|---|
| 是否投 CRAN | 否（只科学分发+开源+论文复现） | 2026-10-30 |
| CI 建在哪 | 默认 Gitee；实际可跑门禁经镜像在 GitHub Actions | 2026-10-30 |
| 多平台 check | 暂不做全平台 devel 矩阵；CI 先上 Linux release | 2026-10-30 |
| 覆盖率门槛 | 不强制；先出报告看盲区 | 2026-10-30 |

**下一步（待 boss）**：① push 验证 CI 触发；② 在干净 Windows 机执行 P0-2 exe E2E；③ 确认是否锁 R 版本 / 补 `verify-exe-clean-env.ps1`。
