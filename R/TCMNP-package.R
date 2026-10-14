#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom grDevices recordPlot
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom stats aggregate
#' @importFrom utils head
#' @importFrom VennDetail make.subset
#' @importFrom VennDetail result
#' @importFrom VennDetail venndetail
## usethis namespace: end
#'
#' @docType _PACKAGE
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## 注意：globalVariables() 只能在「已注册且未锁定」的包命名空间中调用。
## 若在 R CMD INSTALL 的源码求值阶段（此时命名空间已锁定）直接调用，
## 会抛 "namespace ... is locked" 并使 INSTALL 报出误导性的
## "ERROR: lazy loading failed for package"（真实原因被 R 吞掉）。
## 因此这里用 try() 包住：安装期静默跳过，仅在开发期（devtools::load_all 等）生效。
if (getRversion() >= "2.15.1") {
  try(utils::globalVariables(c(
    "Herb_cn_name",
    "herb_pinyin_raw",
    "Herb_name_pin_yin",
    "MOL_ID",
    "Molecule_name",
    "OB",
    "DL",
    "Target_full_name",
    "Target",
    "Disease_name",
    "X.node1",
    "node2",
    "combined_score",
    "herb",
    "molecule_id",
    "molecule",
    "target",
    # P0-B/P1（2026-10-14）：实体层与信任列在 dplyr NSE 中引用
    "herb_id",
    "herb_key",
    "qc_level",
    "freq",
    "D",
    "Description",
    "GeneRatio",
    "BgRatio",
    "pvalue",
    "p.adjust",
    "qvalue",
    "geneID",
    "Count",
    "ONTOLOGY",
    "name",
    ".,",
    "from_c",
    "from",
    "to",
    "to_c",
    "score",
    "weight",
    ".",
    "ID",
    "id",
    "logP",
    "next_node",
    "next_x",
    "node",
    "richFactor",
    "RichFactor",
    "rowname",
    "size",
    "tcm_network",
    "type",
    "x",
    "Candidate.Target.genes",
    "Chemical.Component",
    "Candidate Target genes",
    "Chemical Component",
    "QED",
    "source",
    "gene",
    ".groups",
    "n",
    "names_from",
    "values_from",
    "query",
    "params",
    "intersects",
    "leaf",
    "node.branch",
    "node.short_name",
    "node.size",
    "node1.node.branch",
    "y",
    "ratio",
    "tail",
    "node.name",
    "gather_graph_edge",
    "KK",
    "degree",
    "TF",
    "Target",
    "newdata",
    "trrust",
    "herb_pm",
    "flavor_color",
    "flavor.number",
    "property",
    "property.color",
    "meridian.number",
    "meridian.color",
    "dose.color",
    "color.num",
    "list1",
    "list2",
    "list3",
    "meridian.label1",
    "meridian.label2",
    "meridian.label3",
    "flavor.label1",
    "flavor.label2",
    "freq",
    "compoundfinal",
    "compound",
    "row_number",
    "phyper",
    "Compound",
    "CompoundId",
    "Pvalue",
    "compound_pinyin",
    "compound_cn",
    "M",
    "k",
    "exp_count",
    "Oddratio"
  )), silent = TRUE)
}
