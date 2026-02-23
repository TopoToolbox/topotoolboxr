#' Convert GRIDobj to a matrix and coordinate vectors
#'
#' The function extracts the grid values and coordinates from a \code{GRIDobj}
#' and returns a list of X and Y coordinates as well as the corresponding grid
#' values as matrix.
#'
#' @param grid \code{GRIDobj} to be converted
#'
#' @return A \code{list} with the elements \code{$X} (vector of X-values),
#' \code{$Y} (vector of Y-values), \code{$Z} (numeric matrix of grid values).
#'
#' @examples
#'
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#'
#' ## extract XYZ information
#' XYZ <- GRIDobj2mat(grid = DEM)
#'
#' ## plot image of DEM
#' image(x = XYZ$X, y = XYZ$Y, z = t(XYZ$Z), col = terrain.colors(100))
#'
#' @author Michael Dietze, Wolfgang Schwanghart
#'
#' @export GRIDobj2mat

GRIDobj2mat <- function(grid) {

  ## CHECKS OF INPUT DATA -----------------------------------------------------

  ## check input data set
  if (inherits(grid, "GRIDobj")) {
    grid <- grid$raster
  } else if (!inherits(grid, "SpatRaster")) {
    stop("grid seems to be no GRIDobj or SpatRaster!")
  }

  ## EXTRACT X,Y,Z data -------------------------------------------------------

  ## extract coordinates
  XY <- terra::crds(x = grid)
  X <- sort(unique(XY[, 1]))
  Y <- sort(unique(XY[, 2]))

  ## extract grid values and build correctly flipped and transposed matrix
  Z <- matrix(data = terra::values(grid),
              nrow = dim(grid)[1],
              byrow = TRUE)

  ## RETURN OUTPUT ------------------------------------------------------------
  return(list(X = X,
              Y = Y,
              Z = Z))
}
