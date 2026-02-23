#' Aggregate a GRIDobj or point data set to the resolution of a GRIDobj
#'
#' This function resamples the grid grid1 to match the extent and resolution of
#' grid B. grid2 must spatially overlap with A. By default, the function uses the
#' mean to calculate new grid values, but any other function that takes
#' returns a scalar (e.g. median, std, ...) can be used, too.
#'
#' Values to be aggregated can also be supplied as list of coordinates
#' (and attributes). This is particularly useful if point density for
#' each pixel is greater than one.
#'
#' @param grid1 \code{GRIDobj} that will be aggregated
#'
#' @param grid2 \code{GRIDobj} of coarser resolution to be aggregated to
#'
#' @param xy \code{data.frame} or \code{matrix} with x and y coordinates
#' of irregular spaced geodata points. The output will be converted to a
#' spatial grid of the same resolution and CRS as B. Grid values will be
#' assigned \code{TRUE}.
#'
#' @param xyz \code{data.frame} or \code{matrix} with x, y and z coordinates
#' of irregular spaced geodata points. The output will be converted to a
#' spatial grid of the same resolution and CRS as B.
#'
#' @param aggfun \code{Character} value, name of the function that is used
#' to calculate the new aggregated values. Keywords may be
#' \code{"mean"}, \code{"median"}, \code{"sd"}, \code{"min"}, \code{"max"}.
#' Default is \code{"mean"}.
#'
#' @return Returns an aggregated \code{GRIDobj}
#'
#' @examples
#'
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#'
#' ## create GRIDobj with 100 m resolution and the same extent as DEM
#' grid2 <- GRIDobj(terra::rast(res = c(1000, 1000), ext = ext(DEM$raster)))
#'
#' ## aggregate and plot DEM based on coarse GRIDobj grid2
#' grid3 <- aggregate(DEM, grid2)
#' plot(grid3)
#'
#' ## create 100 random points and aggregate to geometry of GRIDobj grid2
#' pts <- data.frame(x = runif(100, ext(DEM$raster)[1], ext(DEM$raster)[2]),
#'                   y = runif(100, ext(DEM$raster)[3], ext(DEM$raster)[4]),
#'                   z = rnorm(100))
#' D <- aggregate(grid2 = grid2, xyz = pts)
#' plot(D)
#'
#' @author Michael Dietze, Wolfgang Schwanghart
#'
#' @exportS3Method stats::aggregate
aggregate.GRIDobj <- function(grid1, grid2, xy, xyz, aggfun = "mean") {
  ## CHECKS OF INPUT DATA -----------------------------------------------------
  ## check input data set grid2
  if (missing(grid2)) {
    stop("GRIDobj grid2 is missing. Cannot aggregate!")
  }

  ## ANALYSIS PART ------------------------------------------------------------
  grid2 <- grid2$raster
  ## Case 1: GRIDobj grid1 is given
  if (!missing(grid1)) {
    grid1 <- grid1$raster
    ## convert GRIDobj to xyz form
    xyz <- GRIDobj2xyz(grid1)
    coords <- terra::crs(grid1)
    ## case grid2, xy or xyz points are given
  } else if (missing(xy) || !missing(xyz)) {
    ## check/set z values if not provided
    if (missing(xyz)) {
      xyz <- cbind(xy, rep(TRUE, nrow(xy)))
    }
  }

  # build GRIDobj from spatial points
  grid3 <- terra::rasterize(x = xyz, y = grid2, value = xyz[, 3], fun = aggfun)
  if (!missing(grid1)) {
    terra::crs(grid3) <- coords
  }

  ## RETURN OUTPUT ------------------------------------------------------------
  return(GRIDobj(grid3))
}
