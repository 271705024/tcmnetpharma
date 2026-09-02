#' Screen herbs and compound based on disease gene
#'
#' @param data disease gene
#' @param OB.value oral bioavailability
#' @param DL.value drug-likeness
#' @param compound.source Writing format of herb name:
#' Chinese medicine prescription(tcmp) and Chinese patent medicine(cpm)
#' "tcmp" or "pcm"
#' @param freq.value herbs freqence
#' @param digits.value Keep the number of decimal places
#' @param ... additional parameters
#'
#' @return data.frame
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @examples
#' data(disease_data, package = "TCMNP")
#' herbs <- tcm_prescription(disease_data)[[1]]
#' compound <- tcm_prescription(disease_data)[[2]]

tcm_prescription <- function(data,
                             compound.source = "tcmp",
                             OB.value = 30,
                             DL.value = 0.18,
                             freq.value = 50,
                             digits.value = 2, ...) {
  # Search for the right herbal medicine（惰性查询 P3.2：DuckDB 直读 compat parquet）
  data <- as.data.frame(data)
  colnames(data)<- 'disease_gene'
  genes <- unique(as.character(data$disease_gene))

  tcm_network2 <- .tcm_query(sprintf(
    "SELECT * FROM read_parquet('%s') WHERE Target IN (%s) AND %s",
    .tcm_parquet_path(), .sql_in(genes), .tcm_adme_where(OB.value, DL.value))) %>%
    tidyr::drop_na() %>%
    dplyr::distinct(., .keep_all = TRUE)

  # Count the frequency of occurrence of herbs
  herb_frq <- tcm_network2 %>%
    dplyr::select(Herb_cn_name) %>%
    dplyr::group_by(Herb_cn_name) %>%
    dplyr::mutate(freq = dplyr::n()) %>%
    arrange(desc(freq)) %>%
    dplyr::distinct() %>%
    dplyr::filter(freq > freq.value)

  # Construct a data set of traditional Chinese medicine prescriptions
  tmp <- compoundfinal %>%
    dplyr::filter(type == compound.source) %>%
    dplyr::select(herb, compound) %>%
    as.data.frame()

  # Hypergeometric distribution calculation (vectorized, P3.1 2026-09-02)
  # 原实现为嵌套 R 循环（O(方剂×草药)，441k 行 compoundfinal 下 >30min）；
  # 改为 dplyr group_by 计数 + 向量化 phyper（全量 <10s）。
  # 背景中药数目 N=universeherb；复方草药数 M；命中背景集的高频草药数 k。
  herb_name <- herb_frq$Herb_cn_name
  universeherb <- .tcm_distinct("Herb_cn_name")   # 惰性 DISTINCT（去空，P3.2）
  N <- length(universeherb)
  n <- length(herb_name)
  herb_name_has_compound <- intersect(herb_name, unique(tmp$herb))

  stat <- tmp %>%
    dplyr::filter(herb %in% universeherb) %>%            # 仅背景集内草药计入 M
    dplyr::group_by(compound) %>%
    dplyr::summarise(M = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(M >= 5)                                # 原逻辑：M < 5 跳过

  hf <- tmp %>%
    dplyr::filter(herb %in% herb_name_has_compound) %>%  # 高频草药计数 k
    dplyr::group_by(compound) %>%
    dplyr::summarise(k = dplyr::n(), .groups = "drop")

  stat <- stat %>%
    dplyr::left_join(hf, by = "compound") %>%
    dplyr::mutate(k = ifelse(is.na(k), 0L, k)) %>%
    dplyr::filter(k > 0)                                 # 原逻辑：k == 0 跳过

  stat <- stat %>%
    dplyr::mutate(
      exp_count = n * M / N,
      Oddratio = k / exp_count,
      Pvalue = stats::phyper(k - 1, M, N - M, n, lower.tail = FALSE)
    )

  results <- stat %>%
    dplyr::transmute(CompoundId = compound, Pvalue, OddsRatio = Oddratio,
                     ExpCount = exp_count, Count = k, Size = M)

  # Add the intersection of compound herbal medicine and background herbal medicine
  herb_map <- tmp %>%
    dplyr::filter(herb %in% herb_name) %>%
    dplyr::group_by(compound) %>%
    dplyr::summarise(herb = paste(unique(herb), collapse = "/"), .groups = "drop")
  results <- results %>%
    dplyr::left_join(herb_map, by = c("CompoundId" = "compound")) %>%
    dplyr::mutate(herb = ifelse(is.na(herb), "", herb))

  results$p.adjust <- stats::p.adjust(results$Pvalue, method = "BH") %>%
    base::format(digits = digits.value, nsmall = digits.value) %>%
    as.numeric()
  results <- results[order(results$Count, decreasing = TRUE), ]
  results <- as.data.frame(results)
  rownames(results) <- 1:nrow(results)
  results$Pvalue <- as.numeric(results$Pvalue) %>%
    base::format(digits = digits.value, nsmall = digits.value) %>%
    as.numeric()
  results$OddsRatio <- as.numeric(results$OddsRatio) %>%
    base::format(digits = digits.value, nsmall = digits.value) %>%
    as.numeric()
  results$ExpCount <- as.numeric(results$ExpCount) %>%
    base::format(digits = digits.value, nsmall = digits.value) %>%
    as.numeric()
  results$Count <- as.numeric(results$Count)
  results$Size <- as.numeric(results$Size)
  return(list(herb_frq, results))
}

