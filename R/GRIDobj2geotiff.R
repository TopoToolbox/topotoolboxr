#' Save GRIDobj as geotiff file
#'
#' GRIDobj2geotiff writes a GRIDobj to a geotiff file. The
#' \code{file} must be a character indicating the relative or absolute
#' file path. The extension should be either \code{.tif} or \code{.tiff}.
#' A potentially existing file will be overwritten without notice.
#'
#' This function is a convenience function, mainly for mapping the Matlab
#' Topotoolbox capabilities to R. It wraps the workhorse function
#' \code{terra::writeRaster()} (see examples) that in turn uses the
#' [GDAL driver names](https://gdal.org/en/latest/drivers/raster/index.html).
#'
#' @param grid \code{GRIDobj} to be exported
#'
#' @param file \code{Character} value, file name and path where to save
#' the exported \code{GRIDobj}.
#'
#' @return Writes a geotiff data set as file.
#'
#' @examples
#'
#' \dontrun{
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#'
#' ## save example data set as ASCII file
#' GRIDobj2geotiff(grid = DEM, file = "export/srtm_bigtujunga30m_utm11.tif")
#'
#' ## the same but using the terra function
#' terra::writeRaster(grid = DEM,
#'                    filename = "export/srtm_bigtujunga30m_utm11.txt",
#'                    filetype = "GTiff")
#' }
#'
#' @export GRIDobj2geotiff

GRIDobj2geotiff <- function(grid, file) {

  ## CHECKS OF INPUT DATA -----------------------------------------------------

  ## check input data set
  if (inherits(grid, "GRIDobj")) {
    grid <- grid$raster
  } else if (!inherits(grid, "SpatRaster")) {
    stop("grid seems to be no GRIDobj or SpatRaster!")
  }

  ## strip path and check if it exists
  path <- strsplit(x = file, split = "/", fixed = TRUE)[[1]]
  path <- paste(path[-length(path)], collapse = "/")

  if (!dir.exists(paths = path)) {
    stop("Path to save GRIDobj does not exist!")
  }

  ## EXPORT OF GRIDobj --------------------------------------------------------
  terra::writeRaster(x = grid,
                     filename = file,
                     filetype = "GTiff",
                     overwrite = TRUE)
}