#' Test if GRIDobj contains any TRUE value
#'
#' The function checks if there is any TRUE value in the grid
#' values and returns a logical value on the result.
#'
#' @param grid \code{GRIDobj} | \code{SpatRaster}
#'
#' Object to be tested
#'
#' @param ... Additional arguments (ignored)
#'
#' @param na.rm logical
#'
#' If TRUE \code{NA} values are removed before the result is computed
#'
#' @return \code{Logcial} value of the test result
#'
#' @examples
#'
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#'
#' ## test if any FALSE values are contained
#' any(DEM)
#'
#' @author Michael Dietze, Wolfgang Schwanghart
#'
#' @exportS3Method base::any

any.GRIDobj <- function(grid, ..., na.rm = FALSE) {
  return(base::any(terra::values(grid$raster), na.rm = na.rm))
}
