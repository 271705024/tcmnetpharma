#!/usr/bin/env Rscript
# tcmnetpharma 外部化数据拉取脚本
# ---------------------------------------------------------------------------
# 背景：inst/extdata/*.parquet（19 个运行时数据文件，共 ~183 MB）按 1b 决策
#       已从 git 历史中移除、不入库。数据通过 GitHub Releases 或 Zenodo(DOI)
#       分发，本脚本将其下载到 inst/extdata/ 供本地构建 / 测试 / exe 打包使用。
#
# 用法：
#   Rscript tools/fetch-data.R                    # 默认从 Zenodo 拉取（推荐）
#   Rscript tools/fetch-data.R <BASE_URL>         # 覆盖数据源
#   或 export TCMNETPHARMA_DATA_URL=<BASE_URL> 再 Rscript tools/fetch-data.R
# 默认 <BASE_URL> = https://zenodo.org/records/23681367/files
#        （DOI 10.5281/zenodo.23681367，数据版本 TCMNP_1.3.0，许可 CC BY-NC-SA 4.0）
# 其他源：任何提供同名 19 个 parquet 的 HTTP 目录 / 对象存储镜像（传参或设 env 覆盖）
#
# 注意：优先使用 curl（支持 -C - 断点续传，适配不稳定网络）；
#       无 curl 时回退 download.file。
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

# 默认数据源：Zenodo（DOI 10.5281/zenodo.23681367，数据版本 TCMNP_1.3.0）
default_data_url <- "https://zenodo.org/records/23681367/files"

data_url <- Sys.getenv("TCMNETPHARMA_DATA_URL", unset = "")
if (length(args) >= 1) data_url <- args[1]
if (data_url == "") data_url <- default_data_url

message("数据源: ", data_url)

files <- c(
  "disease_main.parquet", "disease_target.parquet",
  "etcm2_disease_gene.parquet", "etcm2_herb_property.parquet",
  "etcm2_herb_target.parquet", "etcm2_mol_adme.parquet",
  "herb_key_collapse.parquet", "herb_key_map.parquet", "herb_main.parquet",
  "layer_a_disease_ontology_map.parquet", "mol_entity_alias.parquet",
  "provenance_rows.parquet", "provenance_tables.parquet", "raw_unmapped.parquet",
  "standard_herb_mol_target.parquet", "string_ppi_human.parquet",
  "tcm_mol_indication.parquet", "tcm_network_compat.parquet", "tcm_target_moa.parquet"
)

dest_dir <- file.path("inst", "extdata")
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

has_curl <- nchar(Sys.which("curl")) > 0
base <- sub("/$", "", data_url)

for (f in files) {
  dest <- file.path(dest_dir, f)
  url <- paste0(base, "/", f)
  cat(sprintf("Fetching %s ...\n", f))
  if (has_curl) {
    # -C - 断点续传；-f 失败不输出；-L 跟随重定向
    rc <- system2("curl", c("-fL", "-C", "-", "-o", dest, url),
                  stdout = FALSE, stderr = FALSE)
    if (rc != 0) {
      stop(sprintf("curl 拉取失败：%s （rc=%d）。可重跑本脚本断点续传。", f, rc))
    }
  } else {
    method <- if (capabilities("libcurl")) "libcurl" else "auto"
    download.file(url, dest, mode = "wb", method = method)
  }
  cat(sprintf("  -> %s (%d bytes)\n", dest, file.info(dest)$size))
}

cat(sprintf("完成：%d 个 parquet 已写入 %s/\n", length(files), dest_dir))
