#' Extract Genus and Species as the first two whitespace-separated words of Taxa
#'
#' @param taxon A single taxon name string.
#' @return A one-row tibble with Genus and Species.
parse_genus_species <- function(taxon) {
  words <- stringr::str_split(taxon, "\\s+")[[1]]
  
  tibble::tibble(
    taxon_original = taxon,
    Genus = words[1],
    Species = words[2],
  )
}

#' Read a region-contribution summary file and extract the top-contributing
#' record per taxon
#'
#' @param file_path Path to the region-contribution summary CSV.
#' @return A tibble with one row per taxon (the record with the highest
#'   `Max region contrib (%)`), plus Genus and Species columns.
summarise_max_regcont <- function(file_path) {
  
  # Step 1: Read the raw summary file.
  raw <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # Step 2: For each taxon, keep only its single highest-contribution record.
  top_per_taxon <- raw |>
    dplyr::group_by(Taxa) |>
    dplyr::slice_max(`Max region contrib (%)`, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  
  # Step 3: Parse Genus/Species from the first two words of Taxa.
  parsed <- top_per_taxon$Taxa |>
    purrr::map_dfr(parse_genus_species) |>
    dplyr::select(Genus, Species)
  
  # Step 4: Row-identity guard before bind_cols() — bind_cols() assumes row
  # alignment silently, so this converts a potential silent misalignment
  # into a loud failure if top_per_taxon and parsed ever diverge in row count.
  stopifnot(nrow(top_per_taxon) == nrow(parsed))
  
  # Step 5: Bind the parsed columns back onto the summary data.
  top_per_taxon |>
    dplyr::bind_cols(parsed)
}