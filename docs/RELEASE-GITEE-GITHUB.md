# tcmnetpharma 发布交接手册（GitHub 上传 · 不稳定网络方案）

> 决策：**1b 数据外置 + A Gitee 桥接**。本机只 push 到 Gitee（国内快稳），
> 由 Gitee 的「仓库镜像管理」自动跨境同步到 GitHub，彻底绕开校园网不稳。

## 0. 现状快照（已就绪，无需再改）

| 项 | 值 |
|---|---|
| 分支 | `main`（已自 `master` 改名） |
| 提交数 | 75（SHA 已因历史重写而全部更新） |
| tracked 文件体积 | 9.3 MB（约 210 个文件） |
| `.git` 体积 | 15 MB（重写前 1.1 GB） |
| 许可证 | GPL-3（`DESCRIPTION` + `LICENSE`） |
| 数据外置 | `inst/extdata/*.parquet` 已移出 git，改由 `tools/fetch-data.R` 拉取 |
| 回滚点 | `D:/update new R/backups/TCMNP-main-pre-rewrite-2026-10-22.bundle`（全量历史） |
| 外置数据文件 | `D:/update new R/backups/tcmnp-inst-extdata-2026-10-22/*.parquet`（19 个，共 183 MB） |

## 1. 在 Gitee 建空仓库 ✅ 已完成（2026-10-26）

- 仓库地址：`https://gitee.com/ZJX261/tcmnetpharma`（账号 `ZJX261`）
- 已建为**空仓库**（未初始化 README/.gitignore/许可证，避免推本地 75 提交时 non-fast-forward）。

## 2. 本机 push 到 Gitee ✅ 已完成（2026-10-26，SSH 免密）

- 已生成本机 SSH 密钥（`~/.ssh/id_ed25519`），公钥已加入 Gitee。
- remote 已设为 SSH：`git@gitee.com:ZJX261/tcmnetpharma.git`
- 已推送 `main`（`git push -u gitee main`），75 个提交全部上链，0 parquet 入库，`.git` 15 MB。

```bash
cd "D:/update new R/TCMNP-main"

# 已配置好的 remote（SSH）
git remote -v
# gitee   git@gitee.com:ZJX261/tcmnetpharma.git (fetch/push)

# 后续增量推送只需：
git push gitee main
```

- 改用 SSH 的原因：HTTPS 推报 `Incorrect username or password`，本机无凭证助手；SSH 一次性配置、免密、对不稳网络更稳。
- 仓库仅 9.3 MB，国内网络下几秒完成；即便校园网抖动也无碍。

## 3. Gitee 自动镜像到 GitHub（跨境同步交给 Gitee 服务器）

1. 先在 https://github.com → New repository 建空仓库 `tcmnetpharma`（默认分支也设 `main`，空仓库）。
2. 回到 Gitee 仓库：**管理 → 仓库镜像管理 → 镜像推送到 GitHub**。
3. 填写：
   - 目标仓库地址：`https://github.com/<你的GitHub账号>/tcmnetpharma.git`
   - 私人令牌：GitHub 生成的 **PAT**（勾 `repo` 权限）
4. 开启「自动同步」并**立即同步一次**。
5. 此后本机只要 `git push gitee main`，Gitee 会自动把更新推到 GitHub。

> 若只想用 GitHub、不想用 Gitee：可直接 `git remote add origin <github url>` + `git push -u origin main`，
> 但跨境直推在不稳网络上较易中断；此时建议改用 `git bundle`（见第 5 节兜底）。

## 4. 上传外置数据（19 个 parquet，183 MB）

数据不进 git，需另存到可下载位置，供 `tools/fetch-data.R` 拉取。

**方案 A（推荐，便于论文引用）：Zenodo**
- 在 https://zenodo.org 新建 deposit，上传 `backups/tcmnp-inst-extdata-2026-10-22/*.parquet`，发布获得 DOI。
- 基础 URL 形如 `https://zenodo.org/records/<id>/files`。

**方案 B（便捷）：GitHub Release**
- 在 GitHub 仓库建 Release，tag `data-v1`，上传那 19 个 parquet。
- 基础 URL 形如 `https://github.com/<账号>/tcmnetpharma/releases/download/data-v1`。
- 可用 CLI 续传上传：`gh release upload data-v1 backups/tcmnp-inst-extdata-2026-10-22/*.parquet`

**本地构建/测试前拉取数据：**
```bash
export TCMNETPHARMA_DATA_URL="<上面的基础 URL>"
Rscript tools/fetch-data.R      # 或 Rscript tools/fetch-data.R <URL>
```
`fetch-data.R` 优先用 `curl -C -` 断点续传，适配不稳定网络。

最后把选中的基础 URL 回填到 `README.md` 的「运行时数据（外部化）」一节（替换 `<owner>/<repo>` 占位）。

## 5. 兜底：git bundle（完全不依赖本机跨境网络）

若 Gitee 也不可用，把整个仓库打成单文件，用云盘/USB 转到稳网机器再推：

```bash
# 本机生成单文件
git bundle create tcmnetpharma.bundle --all
# 稳网机器上：
git clone tcmnetpharma.bundle tcmnetpharma && cd tcmnetpharma
git remote add origin <github-or-gitee url>
git push -u origin main
```

## 6. 回滚

若历史重写后发现问题，可从 bundle 整体恢复：
```bash
cd "D:/update new R"
git clone backups/TCMNP-main-pre-rewrite-2026-10-22.bundle tcmnetpharma-rollback
```
（bundle 含重写前全部 55 个提交，可逐文件取回。）

## 7. 备注

- **单文件上限**：原 `standard_herb_mol_target.parquet` 约 102 MB，超 GitHub 100 MB 硬限——这正是数据外置的根本原因，通道方案解决不了，必须先外置。
- **分支**：已统一为 `main`，Gitee/GitHub 建仓时默认分支也选 `main`，避免镜像错位。
- **凭证**：Gitee/GitHub 的账号、密码、PAT、SSH key 均由你在本机手动配置，我不代填。
- **License**：采用 GPL-3（与 `DESCRIPTION` 一致），`LICENSE` 为规范全文。
