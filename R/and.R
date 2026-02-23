#' Perform logical AND operation with two GRIDobj data sets
#'
#' The function performs a logical AND operation with all values of the two
#' input data sets, which must be GRIDobj objects.
#'
#' @param ... \code{GRIDobj} (or \code{SpatRaster}) data sets to be handled
#'
#' @return \code{GRIDobj} with logical values
#'
#' @note Preserves the type of the first object in case inputs have differing
#' classes.
#'
#' @examples
#'
#' ## Load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#'
#' ## Test which values of both DEMs are TRUE
#' DEM_and <- and(DEM > 0, DEM - 500 > 0)
#' plot(DEM_and)
#'
#' ## Perform operation on three data sets
#' DEM_and <- and(DEM > 0, DEM - 500 > 0, DEM - 1000 > 0)
#' plot(DEM_and)
#'
#' @author Michael Dietze, Wolfgang Schwanghart
#'
#' @export
and <- function(...) {
  ## CHECKS OF INPUT DATA -----------------------------------------------------
  dots <- list(...)
  if (length(dots) < 2) {
    stop("Less than two objects present!")
  }

  # Single vectorized check instead of lapply
  invalid <- !vapply(dots, function(x) {
    inherits(x, "GRIDobj") || inherits(x, "SpatRaster")
  }, TRUE)
  if (any(invalid)) {
    stop("At least one object seems to be no GRIDobj or SpatRaster!")
  }

  ## ANALYSIS PART ------------------------------------------------------------
  # Extract rasters vectorized (no loop needed)
  rasters <- lapply(dots, \(x) if (inherits(x, "GRIDobj")) x$raster else x)
  result <- rasters[[1]]
  for (i in 2:length(rasters)) {
    result <- result & rasters[[i]]
  }

  # Preserve class of first input
  x <- dots[[1]]
  if (inherits(x, "GRIDobj")) {
    x$raster <- result
    return(x)
  }
  result
}