#!/usr/bin/env bash
# tcmnetpharma **软件** → Zenodo 归档脚本（resource_type=software；元数据单一来源 = .zenodo.json）
#
# 与 tools/zenodo-upload.sh（数据集版）的分工：
#   - zenodo-upload.sh          → 数据集 19 个 parquet（upload_type=dataset，CC-BY-NC-SA-4.0）
#   - zenodo-upload-software.sh → 源码快照（upload_type=software，GPL-3.0-only）← 本脚本
#
# 用法（首次，新建草稿）：
#   ZENODO_TOKEN=xxxx bash tools/zenodo-upload-software.sh
#
# 用法（复用已建草稿，避免残留空草稿）：
#   ZENODO_TOKEN=xxxx ZENODO_DEP_ID=1234567 bash tools/zenodo-upload-software.sh
#
# 用法（指定快照文件，跳过硬打包）：
#   SNAPSHOT=/path/to/tcmnetpharma-0.2.0-src.tar.gz ZENODO_TOKEN=xxxx bash tools/zenodo-upload-software.sh
#
# 说明：
#   - 默认用 `git archive` 从 **tag** 打薄快照：只含 git 跟踪的文件，因此 183 MB 的
#     parquet 不会重复进软件记录（数据已单独有 DOI，见 .zenodo.json 的 related_identifiers）。
#   - 本脚本只负责「填充元数据 + 上传文件」，不代你点 Publish（发布动作由你最后确认）。
#   - ZENODO_TOKEN 在 Zenodo → 右上角头像 → Applications → Personal access tokens 生成，勾 deposit:write。
#   - 令牌只存在于你本机环境变量，不写入任何文件。
#   - 无 python / jq 依赖（Git Bash 自带 grep/sed/curl 即可）。

set -euo pipefail

TOKEN="${ZENODO_TOKEN:?请先设置环境变量 ZENODO_TOKEN（Zenodo→Applications→Personal access tokens，勾 deposit:write）}"
API="https://zenodo.org/api/deposit/depositions"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
META_FILE="$ROOT/.zenodo.json"
TMP="$(mktemp)"
trap 'rm -f "$TMP"' EXIT

cd "$ROOT"

[ -f "$META_FILE" ] || { echo "!! 找不到 $META_FILE"; exit 1; }

# ---------- 0a. 版本号与 tag 一致性（防止快照与元数据脱节）----------
VERSION=$(grep -o '"version": *"[^"]*"' "$META_FILE" | head -1 | sed 's/.*"\([^"]*\)"$/\1/')
TAG="${TAG:-v$VERSION}"
if ! git rev-parse -q --verify "refs/tags/$TAG" >/dev/null; then
  echo "!! 本地没有 tag $TAG —— 先打签名 tag 再归档（见 docs/ownership-evidence-playbook-*.md）"
  exit 1
fi
if [ -n "$(git status --porcelain)" ]; then
  echo "!! 工作树不干净，git archive 出来的快照不可复现 —— 先提交或 stash"
  git status --short
  exit 1
fi
echo "==> tag $TAG 存在，工作树干净（版本 $VERSION）"

# ---------- 0b. 准备源码快照 ----------
if [ -n "${SNAPSHOT:-}" ]; then
  [ -f "$SNAPSHOT" ] || { echo "!! SNAPSHOT 不存在：$SNAPSHOT"; exit 1; }
  echo "==> 使用指定快照：$SNAPSHOT"
else
  mkdir -p dist
  SNAPSHOT="dist/tcmnetpharma-$VERSION-src.tar.gz"
  echo "==> 生成薄快照：git archive $TAG → $SNAPSHOT"
  git archive --format=tar.gz --prefix="tcmnetpharma-$VERSION/" -o "$SNAPSHOT" "$TAG"
fi
SNAP_SHA=$(sha256sum "$SNAPSHOT" | cut -d' ' -f1)
SNAP_SIZE=$(wc -c < "$SNAPSHOT")
echo "    sha256 = $SNAP_SHA"
echo "    size   = $SNAP_SIZE bytes"
echo "    （把上面两行抄进发布说明 / 权属存证清单）"

# ---------- 0c. 得到 deposition id（新建 或 复用）----------
if [ -n "${ZENODO_DEP_ID:-}" ]; then
  DEP_ID="$ZENODO_DEP_ID"
  echo "==> 复用已有草稿 DEP_ID=$DEP_ID（不新建，避免残留空草稿）"
else
  echo "==> 创建 deposition（空草稿）"
  code=$(curl -sS -o "$TMP" -w '%{http_code}' \
    -X POST "$API?access_token=$TOKEN" \
    -H "Content-Type: application/json" -d '{}' || true)
  if [ "$code" != "201" ]; then
    echo "    !! 创建失败 HTTP $code —— 返回体："
    cat "$TMP"; echo
    case "$code" in
      401|403) echo "    → token 无效或缺少 deposit:write 权限，请重新生成"; ;;
      000)     echo "    → 网络不可达，可稍后重试或直接走网页手动填"; ;;
    esac
    exit 1
  fi
  DEP_ID=$(grep -o '"id":[[:space:]]*[0-9]\+' "$TMP" | head -1 | grep -o '[0-9]\+')
  [ -n "$DEP_ID" ] || { echo "    !! 未能解析 DEP_ID，返回体："; cat "$TMP"; echo; exit 1; }
  echo "    DEP_ID=$DEP_ID"
fi

# ---------- 1. 元数据（唯一来源：.zenodo.json）----------
echo "==> 提交元数据（读 .zenodo.json，upload_type=software）"
META="{\"metadata\":$(cat "$META_FILE")}"
code=$(curl -sS -o "$TMP" -w '%{http_code}' \
  -X PUT "$API/$DEP_ID?access_token=$TOKEN" \
  -H "Content-Type: application/json" \
  --data-binary "$META" || true)
if [ "$code" != "200" ]; then
  echo "    !! 元数据提交失败 HTTP $code —— 返回体："
  cat "$TMP"; echo
  echo "    → 常见原因：license 取值、related_identifiers 的 resource_type 不被接受。"
  echo "      当前 license=$(grep -o '\"license\": *\"[^\"]*\"' "$META_FILE" | head -1)"
  echo "      Zenodo 许可证词表（SPDX）：curl -s 'https://zenodo.org/api/vocabularies/licenses?q=GPL&size=25'"
  exit 1
fi
echo "    元数据已提交"

# ---------- 2. 上传快照 ----------
# ⚠️ 2026-10-27 实测：legacy 的 `POST /depositions/{id}/files`（multipart）在新式
#    deposit 上会**长时间无响应**（8.4 MB 卡满 2 分钟无输出）。改用 InvenioRDM 的
#    bucket 接口 `PUT {bucket}/{filename}`，同样网络下一次成功。
#    上行慢时用 ZENODO_SKIP_UPLOAD=1 只填元数据，再去网页把文件拖进去。
if [ -n "${ZENODO_SKIP_UPLOAD:-}" ]; then
  echo "==> 跳过上传（ZENODO_SKIP_UPLOAD 已设置）"
  echo "    请打开 https://zenodo.org/uploads/$DEP_ID 手动拖入：$SNAPSHOT"
  exit 0
fi

BUCKET=$(curl -sS --max-time 60 "$API/$DEP_ID?access_token=$TOKEN" \
  | grep -o '"bucket": *"[^"]*"' | head -1 | sed 's/.*"\(http[^"]*\)"$/\1/')
[ -n "$BUCKET" ] || { echo "!! 未能取得 bucket 链接，请手工上传：$SNAPSHOT"; exit 1; }

FNAME=$(basename "$SNAPSHOT")
echo "==> 上传 $FNAME（$((SNAP_SIZE / 1048576)) MiB，bucket 接口）"
code=$(curl -sS -o "$TMP" -w '%{http_code}' --max-time 1800 \
  -X PUT "$BUCKET/$FNAME?access_token=$TOKEN" \
  -H "Content-Type: application/octet-stream" \
  --data-binary @"$SNAPSHOT" || true)
if [ "$code" != "201" ] && [ "$code" != "200" ]; then
  echo "    !! 上传失败 HTTP $code —— 返回体："
  cat "$TMP"; echo
  echo "    → 上行不稳时：ZENODO_SKIP_UPLOAD=1 重跑本脚本，再打开"
  echo "      https://zenodo.org/uploads/$DEP_ID 手动拖入 $SNAPSHOT"
  exit 1
fi
echo "    上传完成"

# 上传后核对：Zenodo 记录的 checksum 必须等于本地 sha256
REMOTE_SUM=$(curl -sS --max-time 60 "$API/$DEP_ID?access_token=$TOKEN" \
  | grep -o '"checksum": *"sha256:[0-9a-f]*"' | head -1 | sed 's/.*sha256:\([0-9a-f]*\)"/\1/')
if [ -n "$REMOTE_SUM" ]; then
  if [ "$REMOTE_SUM" = "$SNAP_SHA" ]; then
    echo "    checksum 核对一致（sha256:$SNAP_SHA）"
  else
    echo "    !! checksum 不一致！本地 $SNAP_SHA / 远端 $REMOTE_SUM —— 不要 Publish"
    exit 1
  fi
fi

echo
echo "==> 完成。请打开以下链接核对元数据与文件，然后点 Publish："
echo "    https://zenodo.org/uploads/$DEP_ID"
echo
echo "发布后把软件 DOI 回填："
echo "    1) CITATION.cff 顶层加 doi:（或新增 references 条目）"
echo "    2) README 的「引用」段落：Software DOI + Dataset DOI 双引用"
echo "    3) docs/ownership-evidence-playbook-*.md 的权属链表格"
