#' Classify species and split binomial names into Genus/Species
#'
#' Splits a `search_term` column (binomial name, "Genus species") into
#' separate `Genus` and `Species` columns, and joins kingdom and class
#' from a pre-built lookup table.
#'
#' @param df A data frame containing a column of binomial names.
#' @param lookup A data frame with columns `taxa`, `kingdom`, `order`, and `class`.
#' @param search_term_col Name of the column containing binomial names
#'   (default "search_term").
#'
#' @return The input data frame with added columns: Genus, Species,
#'   ala_kingdom, ala_class. Taxa absent from the lookup get NA.
classify_species <- \(df, lookup, search_term_col = "search_term") {
  
  stopifnot(
    search_term_col %in% names(df),
    all(c("taxa", "kingdom", "order", "class") %in% names(lookup))
  )
  
  # step 1: split binomial into Genus/Species
  df_split <- df |>
    tidyr::separate(
      dplyr::all_of(search_term_col),
      into = c("Genus", "Species"),
      sep = " ",
      remove = FALSE
    )
  
  # step 2: prepare lookup
  classification <- lookup |>
    na.omit() |> 
    dplyr::distinct(taxa, kingdom, order, class)
  
  # step 3: guard against duplicate keys, which would multiply rows in the join
  stopifnot(!anyDuplicated(classification$taxa))
  
  # step 4: flag taxa that will end up with NA classification
  unmatched <- df_split[[search_term_col]] |>
    stats::na.omit() |>
    unique() |>
    setdiff(classification$taxa)
  
  if (length(unmatched) > 0) {
    warning(
      length(unmatched), " taxa not found in lookup, e.g. ",
      paste(utils::head(unmatched, 5), collapse = ", ")
    )
  }
  
  # step 5: join classification back onto the split data frame
  out <- df_split |>
    dplyr::left_join(classification, by = stats::setNames("taxa", search_term_col))
  
  stopifnot(nrow(out) == nrow(df_split))
  out
}