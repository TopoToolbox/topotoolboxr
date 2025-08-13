#' Hillshade
#' 
#' Compute a hillshade of the supplied digital elevation model
#'
#' @param DEM SpatRaster (terra); Digital elevation model
#' @param azimuth float; The azimuth angle of the light source measured in degrees clockwise from north. Defaults to 315 degrees.
#' @param altitude float; The altitude angle of the light source measured in degrees above the horizon. Defaults to 60 degrees.
#' @param exaggerate float; The amount of vertical exaggeration. Increase to emphasize elevation differences in flat terrain. Defaults to 1.0
#' @param fused logical; If TRUE use the fused hillshade computation in libtopotoolbox, which requires less memory but can be slightly slower. If you have a small DEM, and are repeatedly creating hillshades consider setting to False for increased performance. Defaults to True.
#'
#' @import terra
#'
#' @return SpatRaster (terra); Hillshade
#' 
#' @export

hillshade <- function(DEM,
                      azimuth = 315.0,
                      altitude = 60.0,
                      exaggerate = 1.0,
                      fused = TRUE) {
  # Input validation
  if (!is.numeric(azimuth) || azimuth < 0 || azimuth > 360) {
    stop("`azimuth` must be numeric and between 0 and 360 degrees.")
  }
  if (!is.numeric(altitude) || altitude < 0 || altitude > 360) {
    stop("`alitude` must be numeric and between 0 and 360 degrees.")
  }
  if (!is.numeric(exaggerate) || exaggerate < 0) {
    stop("`exaggerate` must be numeric and positive.")
  }
  if(!is.logical(fused)) {
    stop("`fused` must be logical.")
  }

  # Preallocate output
  HS <- DEM

  # Get DEM input
  DEM <- get_grid_data(DEM)

  # Exaggerating elevations
  if (exaggerate != 1) {
    DEM$z <- DEM$z * exaggerate
  }

  # Conversion to radians for libtopotoolbox
  azimuth <- (-90+azimuth) * pi / 180 # azimuth is measured anticlockwise
  altitude <- altitude * pi / 180

  # Computation of hillshades using libtopottolbox
  output <- single(length(DEM$z))
  if (!fused) {
    result <- .C("wrap_hillshade",
                 outputR=as.single(output), # float
                 dxR=as.single(output), # float
                 dyR=as.single(output), # float
                 demR=as.single(DEM$z), # float
                 azimuthR=as.single(azimuth), # float
                 altitudeR=as.single(altitude), # float
                 cellsizeR=as.single(DEM$cellsize), # float
                 dimsR=as.integer(DEM$dims), # ptrdiff_t
                 NAOK = TRUE)$outputR
  } else {
    result <- .C("wrap_hillshade_fused",
                 outputR=as.single(output), # float
                 demR=as.single(DEM$z), # float
                 azimuthR=as.single(azimuth), # float
                 altitudeR=as.single(altitude), # float
                 cellsizeR=as.single(DEM$cellsize), # float
                 dimsR=as.integer(DEM$dims), # ptrdiff_t
                 NAOK = TRUE)$outputR
  }
  
  # Write result into the pre-allocated SpatRaster (terra)
  terra::values(HS) <- result
  return(HS)
}
