#' Convert GRIDobj to a data frame of X, Y and z values
#'
#' The function extracts the grid values and coordinates from a \code{GRIDobj}
#' and returns a data frame of tupels.
#'
#' @param grid \code{GRIDobj} or \code{SpatRaster} to be converted
#'
#' @return A \code{list} with the elements \code{$X} (vector of X-values),
#' \code{$Y} (vector of Y-values), \code{$z} (numeric matrix of grid values).
#'
#' @examples
#'
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#'
#' ## extract XYZ information
#' XYZ <- GRIDobj2xyz(grid = DEM)
#'
#' ## plot elevation profile of the first horizontal line of the DEM
#' plot(x = XYZ$X[1:1197], y = XYZ$Z[1:1197])
#'
#' @author Michael Dietze, Wolfgang Schwanghart
#'
#' @export GRIDobj2xyz
GRIDobj2xyz <- function(grid) {

  ## CHECKS OF INPUT DATA -----------------------------------------------------

  ## check input data set
  if (inherits(grid, "GRIDobj")) {
    grid <- grid$raster
  } else if (!inherits(grid, "SpatRaster")) {
    stop("grid seems to be no GRIDobj or SpatRaster!")
  }

  ## EXTRACT X,Y,z data -------------------------------------------------------

  ## extract coordinates
  xy <- terra::crds(x = grid)

  ## extract grid values and build correctly flipped and transposed matrix
  z <- terra::values(grid)[, 1]

  ## RETURN OUTPUT ------------------------------------------------------------

  ## return output
  return(data.frame(X = xy[, 1],
                    Y = xy[, 2],
                    Z = z))
}
