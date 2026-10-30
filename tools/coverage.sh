#!/usr/bin/env bash
# =============================================================================
# tcmnetpharma 本地覆盖率（covr）脚本
# -----------------------------------------------------------------------------
# 用法（在包仓库 TCMNP-main 下执行）：
#   bash tools/coverage.sh
# 前置：
#   1. 已把包重装到库（见本地 run-tests 流程的新鲜度守卫）；
#   2. DESCRIPTION 的 Suggests 含 covr（已加）。
# 注：covr 会重跑全部 testthat 用例，首次较慢；仅本地用、不入 CI 必过项。
# =============================================================================
set -u
RSCRIPT="${RSCRIPT:-Rscript}"
# 脚本位于 TCMNP-main/tools/ → 上一层即包仓库根（包在根目录）。
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
if [ ! -f "$ROOT_DIR/DESCRIPTION" ]; then
  echo "[coverage] 错误：找不到包 DESCRIPTION（期望 $ROOT_DIR/DESCRIPTION）" >&2
  exit 2
fi
cd "$ROOT_DIR" || exit 2
echo "[coverage] 在 $ROOT_DIR 运行 covr::package_coverage ..."
"$RSCRIPT" -e 'covr::package_coverage(quiet=FALSE)'
