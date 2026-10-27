#' Screen drug mechanism-of-action (MOA) targets
#'
#' From a drug (molecule name / ChEMBL ID / InChIKey) to its **mechanism of
#' action** — the targets it acts on and how — using the ChEMBL 37
#' `drug_mechanism` layer (`tcm_target_moa.parquet`, 7,561 rows; a drug-target
#' **binary** reference table, kept out of the standard herb-mol-target triple).
#'
#' @param drug A vector of drug identifiers (interpretation depends on `type`).
#' @param type One of `"mol_name"` (default), `"mol_chembl_id"`, `"inchikey"`.
#' @param match `"exact"` (default) or `"contains"` (substring; only meaningful
#'   for `type = "mol_name"`). Name matching is case-insensitive.
#' @param human_only Logical (default `TRUE`). Keep only human targets
#'   (`tax_id = 9606`). Set `FALSE` to include all species (they are flagged via
#'   the `organism` / `is_human` columns).
#' @param ... additional parameters
#'
#' @return data.frame with columns `mol_chembl_id`, `mol_name`, `inchikey`,
#'   `max_phase`, `mechanism_of_action`, `action_type`, `target_name`,
#'   `target_type`, `gene_symbol`, `uniprot`, `organism`, `is_human`. Returns 0
#'   rows (same column contract) when nothing matches.
#'
#' @details
#' `action_type` is ChEMBL's controlled vocabulary (e.g. `INHIBITOR`, `AGONIST`,
#' `ANTAGONIST`); `mechanism_of_action` is the free-text description. Targets
#' carry gene symbols and UniProt accessions where ChEMBL resolves them.
#'
#' This table is the **open-licence** stand-in for the withheld TTD MOA: it
#' provides the same drug-target mechanism + development-phase value under
#' ChEMBL 37 (CC BY-SA 3.0).
#'
#' @seealso [mol_indication()], [disease_target()]
#' @export
#' @examples
#' head(mol_moa("Aspirin"))
#' head(mol_moa("Imatinib", human_only = FALSE))
mol_moa <- function(drug, type = "mol_name",
                    match = c("exact", "contains"),
                    human_only = TRUE, ...) {
  type <- match.arg(type, c("mol_name", "mol_chembl_id", "inchikey"))
  match <- match.arg(match)

  if (!is.logical(human_only) || length(human_only) != 1L || is.na(human_only)) {
    stop("human_only must be a single TRUE/FALSE")
  }

  empty <- .tcm_mol_moa_empty()
  # 复用 .tcm_mol_where（与 mol_indication 同一匹配语义，铁律 54 单一实现）
  w <- .tcm_mol_where(drug, type = type, match = match)
  if (!length(w$x)) return(empty)

  extra <- if (isTRUE(human_only)) "is_human" else "TRUE"

  res <- .tcm_query(sprintf(
    "SELECT mol_chembl_id, mol_name, inchikey, max_phase, mechanism_of_action,
            action_type, target_name, target_type, gene_symbol, uniprot,
            organism, is_human
       FROM read_parquet('%s')
      WHERE %s AND %s
      ORDER BY action_type, target_name",
    .tcm_parquet_path("mol_moa"), w$where, extra))
  if (!nrow(res)) {
    print(paste0("No mechanism-of-action row for: ", paste(w$x, collapse = ", ")))
    return(empty)
  }
  rownames(res) <- NULL
  res
}

#' @noRd
.tcm_mol_moa_empty <- function() {
  data.frame(mol_chembl_id = character(), mol_name = character(),
             inchikey = character(), max_phase = numeric(),
             mechanism_of_action = character(), action_type = character(),
             target_name = character(), target_type = character(),
             gene_symbol = character(), uniprot = character(),
             organism = character(), is_human = logical())
}
