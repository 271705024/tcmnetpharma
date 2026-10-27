#!/usr/bin/env bash
# tcmnetpharma 数据集 → Zenodo 上传脚本（元数据自动填充；文件可脚本上传或 UI 拖拽）
#
# 用法（首次，新建草稿）：
#   ZENODO_TOKEN=xxxx  bash tools/zenodo-upload.sh
#
# 用法（复用已建草稿，避免残留空草稿）：
#   ZENODO_TOKEN=xxxx ZENODO_DEP_ID=1234567  bash tools/zenodo-upload.sh
#
# 说明：
#   - 本脚本只负责「填充元数据 + 上传文件」，不代你点 Publish（发布动作由你最后确认）。
#   - ZENODO_TOKEN 在 Zenodo → 右上角头像 → Applications → Personal access tokens 生成，勾 deposit:write。
#   - 令牌只存在于你本机环境变量，不写入任何文件。
#   - 无 python 依赖（Git Bash 自带 grep/sed/curl 即可）。
#   - 若文件上传步骤报网络错（校园网不稳），可忽略该步，直接去网页把 19 个 parquet 拖进去再 Publish。

set -euo pipefail

TOKEN="${ZENODO_TOKEN:?请先设置环境变量 ZENODO_TOKEN（Zenodo→Applications→Personal access tokens，勾 deposit:write）}"
API="https://zenodo.org/api/deposit/depositions"
DATA_DIR="${DATA_DIR:-/d/update new R/backups/tcmnp-inst-extdata-2026-10-22}"
TMP="$(mktemp)"
trap 'rm -f "$TMP"' EXIT

# ---------- 0. 得到 deposition id（新建 或 复用）----------
if [ -n "${ZENODO_DEP_ID:-}" ]; then
  DEP_ID="$ZENODO_DEP_ID"
  echo "==> 0/3 复用已有草稿 DEP_ID=$DEP_ID（不新建，避免残留空草稿）"
else
  echo "==> 0/3 创建 deposition（空草稿）"
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
  if [ -z "$DEP_ID" ]; then
    echo "    !! 未能解析 DEP_ID，返回体："; cat "$TMP"; echo; exit 1
  fi
  echo "    DEP_ID=$DEP_ID"
fi

# ---------- 1. 元数据（对应 docs/ZENODO-UPLOAD.md §2/§4）----------
# 许可证：cc-by-sa-4.0（保守路线，已拍板）
# ORCID：留空，发布前在 Zenodo UI 补（或在此 creators 里加 "orcid":"0000-..."）
META=$(cat <<'JSON'
{
  "metadata": {
    "upload_type": "dataset",
    "title": "tcmnetpharma runtime dataset: multi-source Traditional Chinese Medicine network pharmacology data (TCMNP v1.3.0)",
    "creators": [
      {
        "name": "Zheng, JianXiao",
        "affiliation": "Haihe Laboratory of Traditional Chinese Medicine, Tianjin University of Traditional Chinese Medicine"
      }
    ],
    "description": "<p>tcmnetpharma runtime dataset (data version TCMNP_1.3.0, 2026-10-21) provides the 19 parquet tables consumed by the tcmnetpharma R package (https://github.com/271705024/tcmnetpharma), a toolkit for Traditional Chinese Medicine (TCM) network pharmacology analysis.</p><p>The dataset integrates six upstream sources — TCMSP, HERB 2.0, BATMAN-TCM 2.0, ETCM2, ChEMBL 37, and SymMap/OpenTargets (disease dimension) — calibrated against the 2025 edition of the Chinese Pharmacopoeia. It contains a standardized herb–molecule–target wide table (16.3M rows, 33 columns, with provenance and QC-level columns), a 10-column compatibility table, disease/entity mapping tables, ETCM2 reference tables, ChEMBL-derived mechanism-of-action and indication tables, and row/table-level provenance tables.</p><p>All files are Apache Parquet. Each file's SHA-256 is published in the repository (docs/ZENODO-UPLOAD.md) for integrity verification. The dataset is externalized from the code repository and retrieved at build/runtime via tools/fetch-data.R.</p>",
    "license": "cc-by-sa-4.0",
    "keywords": ["Traditional Chinese Medicine", "network pharmacology", "TCMSP", "HERB", "BATMAN-TCM", "ETCM2", "ChEMBL", "herb-molecule-target", "data integration"],
    "version": "1.3.0",
    "language": "eng",
    "related_identifiers": [
      {"relation": "isSupplementTo", "identifier": "https://github.com/271705024/tcmnetpharma"},
      {"relation": "references", "identifier": "https://doi.org/10.1093/nar/gkab746"},
      {"relation": "references", "identifier": "https://doi.org/10.1093/nar/gkae328"},
      {"relation": "references", "identifier": "https://doi.org/10.1093/nar/gkad968"},
      {"relation": "references", "identifier": "https://doi.org/10.1093/nar/gkad984"},
      {"relation": "references", "identifier": "https://doi.org/10.1093/nar/gkv1318"}
    ]
  }
}
JSON
)

echo "==> 1/3 提交元数据（填充表单：标题/作者/描述/许可证/关键词/版本/关联）"
code=$(curl -sS -o "$TMP" -w '%{http_code}' \
  -X PUT "$API/$DEP_ID?access_token=$TOKEN" \
  -H "Content-Type: application/json" \
  --data-binary "$META" || true)
if [ "$code" != "200" ]; then
  echo "    !! 元数据提交失败 HTTP $code —— 返回体："
  cat "$TMP"; echo
  echo "    → 常见原因：license/related_identifiers 取值不被接受。把上面返回体贴回来即可定位。"
  exit 1
fi
echo "    元数据已提交"

# ---------- 2. 上传 19 个 parquet ----------
echo "==> 2/3 上传 parquet（legacy deposit files 接口）"
if [ ! -d "$DATA_DIR" ]; then
  echo "    !! 数据目录不存在：$DATA_DIR（跳过上传，请在网页拖入 19 个 parquet）"
else
  ok=0; fail=0
  ( cd "$DATA_DIR" && for f in *.parquet; do
      sz=$(wc -c < "$f")
      printf '    uploading %-46s %8.1f MB ... ' "$f" "$(awk "BEGIN{print $sz/1048576}")"
      code=$(curl -sS -o "$TMP" -w '%{http_code}' \
        -X POST "$API/$DEP_ID/files?access_token=$TOKEN" \
        -F "file=@$f" || true)
      if [ "$code" = "201" ] || [ "$code" = "200" ]; then echo "ok"; ok=$((ok+1));
      else echo "FAIL(HTTP $code)"; fail=$((fail+1)); fi
    done
    echo "    上传结果：成功 $ok 个，失败 $fail 个"
  )
fi

echo
echo "==> 3/3 完成。请打开以下链接核对元数据与文件，然后点 Publish："
echo "    https://zenodo.org/uploads/$DEP_ID"
echo
echo "（可选）脚本发布："
echo "    curl -X POST \"$API/$DEP_ID/actions/publish?access_token=\$ZENODO_TOKEN\""
echo
echo "发布后把 DOI 基础 URL 回填 README 的 TCMNETPHARMA_DATA_URL + tools/fetch-data.R，再 git push gitee main。"
