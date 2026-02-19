#' wrap_gwdt
#'
#' Perform the grey-weighted distance transform (GWDT) on the DEM.
#'
#' @param dem GRIDobj | SpatRaster
#'
#' Digital elevation model
#'
#' @return GRIDobj | SpatRaster
#'
#' GWDT distances for each grid cell.
#'
#' @export

gwdt <- function(dem) {
  # Input validation and value extraction
  dem <- processgrid(dem)
  provided_go <- dem$provided_go
  dem <- dem$r

  demf <- fillsinks(dem)
  flats <- identifyflats(demf)
  costs <- gwdt_computecosts(flats, dem, demf)

  fl <- get_grid_data(flats)
  co <- get_grid_data(costs)

  # Compute costs using libtopotoolbox
  outputs <- single(length(co$z))
  result <- .C(
    "wrap_gwdt",
    distR = as.single(outputs), # float
    costsR = as.single(co$z), # float
    flatsR = as.integer(fl$z), # int32_t
    dimsR = as.integer(fl$dims), # ptrdiff_t
    NAOK = TRUE
  )$distR

  # Write results into grid
  dist <- dem
  terra::values(dist) <- result
  if (provided_go) {
    dist <- GRIDobj(dist)
  }
  dist
}