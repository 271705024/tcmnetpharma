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
#' data("xfbdf", package = "TCMNP")
#' head(xfbdf)
"xfbdf"


#' ppi_data
#'
#' Protein-protein interaction analysis was performed using STRING database.
#' A data frame with 2234 rows and 3 variables
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

#' KK
#'
#' The results of R clusterprofiler package for KEGG results
#' S4 object of class enrich results
#' @source R clusterprofiler package for KEGG results
#' @examples
#' head(KK@result)
#'
"KK"

#' kegg.filter
#'
#' The results after KEGG/GO screening of pathways and genes
#' A data frame with 8 rows and 9 variables
#' @source example data from the KEGG results
#' @examples
#' head(kegg.filter)
#'
"kegg.filter"

#' Venn diagram example data
#'
#' The datasets was from xfbdf and https://doi.org/10.1038/s41556-021-00796-6.
#' A data frame with 6439 rows and 2 variables
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
#' @description Chinese Pharmacopoeia commonly used herbs property and flavor, tropism
#' The datasets was from Chinese Pharmacopoeia
#' A data frame with 409 rows and 5 variables
#' @source \url{https://db.ouryao.com/yd2020/index.php?cid=1}
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
#' A character with 790 genes
#' @source  KEGG & GSEA
#' @examples
#' head(disease_data)
#'
"disease_data"

#' sankey_data
#'
#' A data frame with 100 rows and 4 variables
#' @source  xfbdf
#' @examples
#' head(sankey_data)
#'
"sankey_data"

#' compoundfinal
#'
#' A data frame with 3397rows and 5 variables
#' @examples
#' head(compoundfinal)
#'
"compoundfinal"

#' trrust
#'
#' A data frame with 7189 rows and 2 variables
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


#' compound_data
#'
#' A data frame with 13 rows and 5 variables
#' @examples
#' head(compound_data)
#'
"compound_data"


