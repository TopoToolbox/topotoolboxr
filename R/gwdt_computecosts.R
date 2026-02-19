#' wrap_gwdt_computecosts
#'
#' Compute the cost array used in the gradient-weighted distance
#' transform (GWDT) algorithm.
#'
#' @param flats GRIDobj | SpatRaster
#'
#' Flat pixels as returned by identifyflats()
#'
#' @param original_dem GRIDobj | SpatRaster
#'
#' Raw digital elevation model
#'
#' @param filled_dem GRIDobj | SpatRaster
#'
#' Processed digital elevation model
#'
#' @return GRIDobj | SpatRaster
#'
#' Costs corresponding to each grid cell in the DEM
#'
#' @export

gwdt_computecosts <- function(flats, original_dem, filled_dem) {

  # Input validation
  flats <- processgrid(flats)
  provided_go_fl <- flats$provided_go
  flats <- flats$r
  original_dem <- processgrid(original_dem)
  provided_go_or <- original_dem$provided_go
  original_dem <- original_dem$r
  filled_dem <- processgrid(filled_dem)
  provided_go_fi <- filled_dem$provided_go
  filled_dem <- filled_dem$r
  provided_go <- any(provided_go_fl, provided_go_or, provided_go_fi)

  if (!validatealignment(flats, original_dem, filled_dem)) {
    stop("All inputs must have the same dimensions.")
  }
  if (!all(unique(terra::values(flats)) %in% c(0, 1, 2, 5))){
    stop("'flats' contains invalid values.")
  }

  fl <- get_grid_data(flats)
  dr <- get_grid_data(original_dem)
  df <- get_grid_data(filled_dem)

  # Compute costs using libtopotoolbox
  outputs <- single(length(fl$z))
  results <- .C("wrap_gwdt_computecosts",
                costsR = as.single(outputs),
                flatsR = as.integer(fl$z),
                original_demR = as.single(dr$z),
                filled_demR = as.single(df$z),
                dimsR = as.integer(fl$dims),
                NAOK = TRUE)

  # Write results into SpatRaster
  costs <- flats
  terra::values(costs) <- results$costsR
  if (provided_go) {
    costs <- GRIDobj(costs)
  }
  costs
}
