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
#' data(disease_data, package = "tcmnetpharma")
#' herbs <- tcm_prescription(disease_data)[[1]]
#' compound <- tcm_prescription(disease_data)[[2]]

# ⚠️ 此处**不得**插入任何代码 —— roxygen 注释块必须**紧邻**它所描述的对象，
#   中间夹一段 `utils::globalVariables("herb_id")` 会让 roxygen 报
#   「Block must have a @name」并**删除 man/tcm_prescription.Rd**
#   （2026-10-15 实测：roxygenise 因此删文档，R CMD check 随之失败）。
#   该变量已在 `R/tcmnetpharma-package.R` 的包级 `globalVariables(...)` 里统一声明
#   （同一事实只在一处声明，MEMORY 铁律 54）→ 文件级重复声明既冗余又破坏解析。

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

  # 手拼 SQL 时凡引用 parquet 列名一律经 .tcm_col_db() 映射（大小写折叠碰撞，
  # 见 docs/data-management-spec-2026-08-24.md 3. 命名规则）。
  tcm_network2 <- .tcm_query(sprintf(
    "SELECT * FROM read_parquet('%s') WHERE \"%s\" IN (%s) AND %s",
    .tcm_parquet_path(), .tcm_col_db("Target"), .sql_in(genes),
    .tcm_adme_where(OB.value, DL.value))) %>%
    tidyr::drop_na() %>%
    dplyr::distinct(., .keep_all = TRUE)

  # ---- 实体层归一（2026-10-14，分析模块接入）--------------------------------
  # 原实现按 `Herb_cn_name`（中文名）统计，有两处口径错误：
  #   ① 同名异实体被当成一味药（6,349 个中文名 vs 6,926 个实体）；
  #   ② 复方表 compoundfinal 的 herb 含大量 ETCM2 原始 CamelCase 拼音，
  #      与中文名空间**不匹配** —— 实测 8,624 味药里只有 1,364（15.8%）能进入
  #      统计 → N / M / k 全部低估，超几何 p 值不可信。
  # 现统一到**实体（herb_id）**空间：身份用 id、展示用规范中文名。
  km <- .tcm_keymap()
  key2id <- function(x) km$herb_id[match(x, km$herb_key)]
  id2cn <- .tcm_canonical_cn()

  tcm_network2$herb_id <- key2id(tcm_network2$Herb_name_pin_yin)

  # 高频草药（按**实体**计数，展示规范中文名）
  herb_frq <- tcm_network2 %>%
    dplyr::filter(!is.na(herb_id)) %>%
    dplyr::group_by(herb_id) %>%
    dplyr::summarise(freq = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(freq)) %>%
    dplyr::filter(freq > freq.value)
  herb_frq$Herb_cn_name <- unname(id2cn[herb_frq$herb_id])
  # ⚠️ herb_id **必须保留**：后续 N / k 的计算都走实体空间（曾因这里把它删掉，
  #    导致 herb_name=NULL → 结果 0 行 → `rownames<-` 报 invalid 'row.names' length）
  herb_frq <- as.data.frame(herb_frq[, c("Herb_cn_name", "freq", "herb_id")])

  # Construct a data set of traditional Chinese medicine prescriptions
  tmp <- compoundfinal %>%
    dplyr::filter(type == compound.source) %>%
    as.data.frame()
  if (!"herb_id" %in% names(tmp)) {
    # 向后兼容：旧 rda 无 herb_id 时用 herb 键反查实体
    tmp$herb_id <- key2id(tmp$herb)
  }
  tmp <- tmp[!is.na(tmp$herb_id) & nzchar(as.character(tmp$herb_id)),
             c("compound", "herb", "herb_id")]

  # Hypergeometric distribution calculation (vectorized, P3.1 2026-09-02)
  # 背景**实体**数 N；复方实体数 M；命中背景集的实体数 k。
  herb_name <- herb_frq$herb_id
  universeherb <- unique(key2id(.tcm_distinct("Herb_name_pin_yin")))
  universeherb <- universeherb[!is.na(universeherb)]
  N <- length(universeherb)
  n <- length(herb_name)
  herb_name_has_compound <- intersect(herb_name, unique(tmp$herb_id))

  stat <- tmp %>%
    dplyr::filter(herb_id %in% universeherb) %>%          # 仅背景集内实体计入 M
    dplyr::group_by(compound) %>%
    dplyr::summarise(M = dplyr::n_distinct(herb_id), .groups = "drop") %>%
    dplyr::filter(M >= 5)                                 # 原逻辑：M < 5 跳过

  hf <- tmp %>%
    dplyr::filter(herb_id %in% herb_name_has_compound) %>% # 高频实体计数 k
    dplyr::group_by(compound) %>%
    dplyr::summarise(k = dplyr::n_distinct(herb_id), .groups = "drop")

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
  # （按**实体**取交集；`herb` 列是展示名，`herb_name` 现在是 herb_id）
  herb_map <- tmp %>%
    dplyr::filter(herb_id %in% herb_name) %>%
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
  # ⚠️ 用 seq_len 而非 1:nrow：空结果时 `1:0` = c(1,0)（长度 2）→
  #    `row.names<-` 会报 "invalid 'row.names' length"（2026-10-14 实测踩到）。
  rownames(results) <- seq_len(nrow(results))
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

