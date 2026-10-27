#' Xuanfei Baidu granule (XFBD)
#'
#' @description
#' ephedra 6 g, bitter almond 15 g, raw gypsum 30 g,
#' raw almond 30 g, raw plaster 30 g,
#' Atractylodes macrocephala 10 g, patchouli 15 g,
#' artemisia grass 12 g, Polygonum cuspidatum 20 g,
#' verbena 30 g, dried Reed root 30 g,
#' Fructus thunbergii 15 g, tangerine red 15 g,
#' raw licorice 10 g.
#'
#' A data frame with 5024 rows and 4 variables
#' @source \url{https://www.nhc.gov.cn/cms-search/downFiles/a449a3e2e2c94d9a856d5faea2ff0f94.pdf}
#' @examples
#' data("xfbdf", package = "tcmnetpharma")
#' head(xfbdf)
"xfbdf"


#' ppi_data
#'
#' @description Protein-protein interaction analysis was performed using STRING database.
#' A data frame with 100 rows and 3 variables
#' @source \url{https://string-db.org/}
#' @examples
#' ppi_data<- data(ppi_data)
#' head(ppi_data)
#'
"ppi_data"

#' ma huang
#'
#' The datasets was from ETCM database.
#' A data frame with 28 rows and 2 variables
#' @source \url{http://www.tcmip.cn/ETCM/index.php/Home/Index/yc_details.html?id=212}
#' @examples
#' head(mahuang)
#'
"mahuang"

# ── 已移除的数据对象（2026-10-27）────────────────────────────────────────────
# KK / kegg.filter 均为 KEGG 富集结果（clusterProfiler 输出），属内部测试用数据。
# KEGG 数据版权归 Kanehisa Laboratories，条款不允许随软件再分发，故删除，
# 与本包「KEGG 不随包分发」的声明保持一致（见 inst/extdata/DATA-README.md）。
# 需要 KEGG 富集：联网自行调用 clusterProfiler::enrichKEGG()，返回值可直接
# 喂给本包可视化函数；离线替代见 tcm_enrich() 的 WikiPathways/Reactome/GO。
# 原测试夹具 test-cirplot-canvas.R 已改用合成富集表（只量几何，不依赖通路内容）。

#' Venn diagram example data
#'
#' The datasets was from xfbdf and https://doi.org/10.1038/s41556-021-00796-6.
#' A data frame with 259 rows and 2 variables
#' @source \url{https://www.nature.com/articles/s41556-021-00796-6#Sec47}
#' @examples
#' head(venn_data)
"venn_data"

#' tf_data
#' @description The datasets was from TRRUST database
#' A data frame with 100 rows and 2 variables
#' @source \url{https://www.grnpedia.org/trrust/}
#' @examples
#' head(tf_data)
"tf_data"

#' Herbal Properties and Meridians
#' @description 中药性味归经表：取自 HERB 2.0 的 herb 表，按 herb 实体键对齐；
#' 共 6,860 行（HERB 6,892 味去空后）。
#' 2025 版《中国药典》仅作口径参照，**不打包、不重分发**。
#' A data frame with 6860 rows and 7 variables
#' @source \url{http://herb.ac.cn/}
#' @examples
#' head(herb_pm)
"herb_pm"

#' network_data
#'
#'
#' A data frame with 100rows and 3 variables
#' @source  network_data
#' @examples
#' head(network_data)
#'
"network_data"


#' disease_data
#'
#' A data frame with 790 rows and 1 variable (示例基因符号列表，测试用)
#' @source 作者早期 KEGG/GSEA 分析所得的基因符号列表；仅含符号，不含通路数据
#' @examples
#' head(disease_data)
#'
"disease_data"

#' sankey_data
#'
#' A data frame with 30 rows and 4 variables
#' @source  xfbdf
#' @examples
#' head(sankey_data)
#'
"sankey_data"

#' compoundfinal
#'
#' A data frame with 441860 rows and 7 variables
#' @examples
#' head(compoundfinal)
#'
"compoundfinal"

#' trrust
#'
#' The datasets was from TRRUST v2 database (CC BY-SA 4.0; Han et al. 2018)
#' A data frame with 16453 rows and 6 variables
#' @examples
#' head(trrust)
#'
"trrust"

#' human_reactome
#'
#' Human Reactome pathway gene sets (all levels), in long \code{term}/\code{gene} format
#' for offline enrichment with clusterProfiler \code{enricher}/\code{GSEA}
#' (pass as \code{TERM2GENE}).
#'
#' @format A data frame with 139327 rows and 2 variables:
#' \describe{
#'   \item{term}{Reactome stable identifier, e.g. \code{R-HSA-70171} (Glycolysis)}
#'   \item{gene}{official human gene symbol}
#' }
#' @details Replaces the KEGG gene sets (human_kegg, removed; KEGG downloads are
#'   not redistributable). Built 2026-09-05 from
#'   \code{reactome.org/download/current/NCBI2Reactome_All_Levels.txt}
#'   (Homo sapiens, all levels), Entrez IDs mapped to symbols via org.Hs.eg.db.
#'   Reactome content is licensed CC BY 4.0 (redistributable). The pathway name
#'   for each term is available in the companion CSV
#'   \code{data-raw/gene-sets/output/human_reactome_pathway_map.csv}.
#' @examples
#' head(human_reactome)
#'
"human_reactome"

#' human_go
#'
#' Human GO gene sets (BP/CC/MF pooled) in long \code{term}/\code{gene} format
#' for offline enrichment with clusterProfiler \code{enricher}/\code{GSEA}.
#'
#' @format A data frame with 224180 rows and 2 variables:
#' \describe{
#'   \item{term}{Gene Ontology identifier, e.g. \code{GO:0006915} (apoptotic process)}
#'   \item{gene}{official human gene symbol}
#' }
#' @details Replaces the MSigDB Hallmark gene sets (human_gsea, removed; MSigDB
#'   downloads are not redistributable). Built 2026-09-05 from direct GO
#'   annotations of org.Hs.eg.db (Bioconductor 3.23), evidence codes excluding
#'   IEA, minimum 2 genes per term. GO content is CC BY 4.0 (Gene Ontology
#'   Consortium). GO ID -> term name/ontology mapping in companion CSV
#'   \code{data-raw/gene-sets/output/human_go_go_term_map.csv}.
#' @examples
#' head(human_go)
#'
"human_go"


#' mu_reactome
#'
#' Mouse Reactome pathway gene sets (all levels), in long \code{term}/\code{gene}
#' format for offline enrichment with clusterProfiler \code{enricher}/\code{GSEA}.
#'
#' @format A data frame with 88600 rows and 2 variables:
#' \describe{
#'   \item{term}{Reactome stable identifier, e.g. \code{R-MMU-70171} (Glycolysis)}
#'   \item{gene}{official mouse gene symbol}
#' }
#' @details Replaces the KEGG gene sets (mu_kegg, removed). Built 2026-09-05 from
#'   \code{NCBI2Reactome_All_Levels.txt} (Mus musculus, all levels), Entrez IDs
#'   mapped to symbols from the org.Mm.eg.db 3.23.0 SQLite annotation.
#'   Reactome content is licensed CC BY 4.0 (redistributable). Pathway names in
#'   companion CSV \code{data-raw/gene-sets/output/mu_reactome_pathway_map.csv}.
#' @examples
#' head(mu_reactome)
#'
"mu_reactome"

#' mu_go
#'
#' Mouse GO gene sets (BP/CC/MF pooled) in long \code{term}/\code{gene} format
#' for offline enrichment with clusterProfiler \code{enricher}/\code{GSEA}.
#'
#' @format A data frame with 271809 rows and 2 variables:
#' \describe{
#'   \item{term}{Gene Ontology identifier, e.g. \code{GO:0006915}}
#'   \item{gene}{official mouse gene symbol}
#' }
#' @details Replaces the MSigDB Hallmark gene sets (mu_gsea, removed). Built
#'   2026-09-05 from direct GO annotations of org.Mm.eg.db 3.23.0 SQLite,
#'   evidence codes excluding IEA, minimum 2 genes per term. GO content is
#'   CC BY 4.0 (Gene Ontology Consortium). GO ID -> term name/ontology mapping
#'   in companion CSV \code{data-raw/gene-sets/output/mu_go_go_term_map.csv}.
#' @examples
#' head(mu_go)
#'
"mu_go"

#' human_wikipathways
#'
#' Human WikiPathways pathway gene sets (2026-10-10 monthly release) in long
#' \code{term}/\code{gene} format for offline enrichment with clusterProfiler
#' \code{enricher}/\code{GSEA} (pass the \code{genes} element as
#' \code{TERM2GENE}). The companion \code{meta} element maps each \code{term}
#' (WikiPathways WP id) to its pathway \code{name} for result annotation.
#'
#' @format A list of two data frames:
#' \describe{
#'   \item{genes}{long \code{term} (WP id) / \code{gene} (official human gene
#'     symbol) pairs, 41891 rows.}
#'   \item{meta}{unique \code{term} / \code{name} mapping, 991 rows.}
#' }
#' @details Built 2026-10-17 from the WikiPathways GMT release
#'   \url{https://data.wikipathways.org/20261010/gmt/wikipathways-20261010-gmt-Homo_sapiens.gmt};
#'   Entrez IDs mapped to symbols via org.Hs.eg.db. WikiPathways content is
#'   released under CC0 (public domain) since 2018, so it is freely
#'   redistributable and safe for the offline exe distribution. This gene set
#'   replaces KEGG pathway enrichment, whose derived data are not redistributable.
#'   Build script: \code{data-raw/build_wikipathways.R}.
#' @examples
#' head(human_wikipathways$genes)
#' head(human_wikipathways$meta)
#'
"human_wikipathways"

#' mu_wikipathways
#'
#' Mouse WikiPathways pathway gene sets (2026-10-10 monthly release) in long
#' \code{term}/\code{gene} format for offline enrichment with clusterProfiler
#' \code{enricher}/\code{GSEA} (pass the \code{genes} element as
#' \code{TERM2GENE}). The companion \code{meta} element maps each \code{term}
#' (WikiPathways WP id) to its pathway \code{name} for result annotation.
#'
#' @format A list of two data frames:
#' \describe{
#'   \item{genes}{long \code{term} (WP id) / \code{gene} (official mouse gene
#'     symbol) pairs, 11178 rows.}
#'   \item{meta}{unique \code{term} / \code{name} mapping, 214 rows.}
#' }
#' @details Built 2026-10-17 from the WikiPathways GMT release
#'   \url{https://data.wikipathways.org/20261010/gmt/wikipathways-20261010-gmt-Mus_musculus.gmt};
#'   Entrez IDs mapped to symbols via org.Mm.eg.db. WikiPathways content is
#'   released under CC0 (public domain) since 2018, so it is freely
#'   redistributable and safe for the offline exe distribution. This gene set
#'   replaces KEGG pathway enrichment, whose derived data are not redistributable.
#'   Build script: \code{data-raw/build_wikipathways.R}.
#' @examples
#' head(mu_wikipathways$genes)
#' head(mu_wikipathways$meta)
#'
"mu_wikipathways"


#' compound_data
#'
#' A data frame with 13 rows and 5 variables
#' @examples
#' head(compound_data)
#'
"compound_data"



#' human_go_annot
#'
#' Offline GO annotation for \strong{Homo sapiens}, built to replace the
#' \pkg{org.Hs.eg.db} package (~400 MB) in the offline bundle while keeping
#' \code{enrichGO()}'s exact statistics.
#'
#' Built from \pkg{org.Hs.eg.db} 3.23.1 direct GO annotations
#' (\emph{including} IEA evidence codes, as \code{enrichGO} does), then
#' \strong{propagated to all ancestor terms} within each sub-ontology
#' (BP/CC/MF) using \pkg{GO.db}. Enrichment is therefore run with
#' \code{enricher()} per sub-ontology using that sub-ontology's own gene
#' universe, which reproduces \code{enrichGO(ont = "ALL")} exactly
#' (verified 2026-10-19: identical term set, GeneRatio, BgRatio and p-values).
#'
#' @format A list with four elements:
#' \describe{
#'   \item{genes}{data.frame(\code{term}, \code{gene}); \code{gene} is a SYMBOL;
#'     terms are already ancestor-propagated}
#'   \item{meta}{data.frame(\code{term}, \code{name}, \code{ontology});
#'     \code{ontology} is BP/CC/MF}
#'   \item{universe}{character vector of all annotated SYMBOLs}
#'   \item{ez2sym}{data.frame(\code{entrez}, \code{symbol}); full ID map used
#'     when \code{keytype = "ENTREZID"}}
#' }
#' @source \pkg{org.Hs.eg.db} 3.23.1 + \pkg{GO.db}; builder script
#'   \code{data-raw/gene-sets/scripts/build_go_offline_annot.R}.
#' @examples
#' str(human_go_annot, max.level = 1)
"human_go_annot"

#' mu_go_annot
#'
#' Offline GO annotation for \strong{Mus musculus}; same structure, build
#' procedure and equivalence guarantee as \code{\link{human_go_annot}}.
#'
#' @format See \code{\link{human_go_annot}}.
#' @source \pkg{org.Mm.eg.db} 3.23.0 + \pkg{GO.db}; built by
#'   \code{data-raw/gene-sets/scripts/build_go_offline_annot.R}.
#' @examples
#' str(mu_go_annot, max.level = 1)
"mu_go_annot"
