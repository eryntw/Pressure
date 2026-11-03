#' Summarise a single threshold raster: total presence pixels and how many
#' fall within a polygon layer
#'
#' @param path File path to a threshold raster (0/1 presence/absence)
#' @param vector A single dissolved SpatVector representing the polygon layer
#'   of interest as one geometry (e.g. from merge_capad())
#' @param label Short string used to name the output columns, e.g. "capad"
#'   gives t1_in_capad and pct_in_capad. Must be a valid column-name suffix.
#'
#' @return A 1-row tibble: path, t1 (total presence pixels),
#'   t1_in_<label> (coverage-weighted presence pixels inside the polygon),
#'   pct_in_<label> (%)
summarise_sdm <- \(path, vector, label = "capad") {
  
  stopifnot(is.character(label), length(label) == 1, nzchar(label))
  
  # step 1: read the threshold raster fresh from disk (0/1 presence/absence)
  r <- terra::rast(path)
  
  # step 2: reproject vector to match the raster's CRS if needed
  if (!terra::same.crs(r, vector)) {
    vector <- terra::project(vector, terra::crs(r))
  }
  
  # step 3: total presence pixel count across the whole raster
  t1 <- terra::global(r, fun = "sum", na.rm = TRUE)[[1]]
  
  # step 4: exact_extract needs an sf object, not a SpatVector
  vector_sf <- sf::st_as_sf(vector)
  
  # step 5: coverage-weighted extraction (sum of presence values weighted
  # by the fraction of each pixel covered by the polygon)
  t1_in <- exactextractr::exact_extract(
    r, vector_sf,
    fun = "sum",
    progress = FALSE
  )
  
  # step 5b: sum defensively in case of multipart geometry / >1 row
  t1_in <- sum(t1_in, na.rm = TRUE)
  
  # step 6: assemble the result with label-based column names
  out <- tibble::tibble(
    path = path,
    t1   = t1,
    in_n = t1_in,
    pct  = ifelse(t1 > 0, t1_in / t1 * 100, NA_real_)
  )
  
  names(out)[names(out) == "in_n"] <- paste0("t1_in_", label)
  names(out)[names(out) == "pct"]  <- paste0("pct_in_", label)
  
  out
}