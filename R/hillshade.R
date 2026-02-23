#' Hillshade
#'
#' Compute a hillshade of the supplied digital elevation model
#'
#' @param dem GRIDobj | SpatRaster (terra); Digital elevation model
#' @param azimuth float; The azimuth angle of the light source measured in
#' degrees clockwise from north. Defaults to 315 degrees.
#' @param altitude float; The altitude angle of the light source measured in
#' degrees above the horizon. Defaults to 60 degrees.
#' @param exaggerate float; The amount of vertical exaggeration. Increase to
#' emphasize elevation differences in flat terrain. Defaults to 1.0
#' @param fused logical; If TRUE use the fused hillshade computation in
#' libtopotoolbox, which requires less memory but can be slightly slower. If you
#' have a small DEM, and are repeatedly creating hillshades consider setting to
#' FALSE for increased performance. Defaults to TRUE.
#'
#' @return GRIDobj | SpatRaster (terra); Hillshade
#'
#' @examples
#'
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#' HS <- hillshade(DEM)
#'
#' @export

hillshade <- function(dem,
                      azimuth = 315.0,
                      altitude = 60.0,
                      exaggerate = 1.0,
                      fused = TRUE) {
  # Input validation
  dem <- processgrid(dem)
  provided_go <- dem$provided_go
  dem <- dem$r
  if (!is.numeric(azimuth) || azimuth < 0 || azimuth > 360) {
    stop("'azimuth' must be numeric and between 0 and 360 degrees.")
  }
  if (!is.numeric(altitude) || altitude < 0 || altitude > 360) {
    stop("'alitude' must be numeric and between 0 and 360 degrees.")
  }
  if (!is.numeric(exaggerate) || exaggerate < 0) {
    stop("'exaggerate' must be numeric and positive.")
  }
  if (!is.logical(fused)) {
    stop("'fused' must be logical.")
  }

  # Preallocate output
  hs <- dem

  # Get DEM input
  dem <- get_grid_data(dem)

  # Exaggerating elevations
  if (exaggerate != 1) {
    dem$z <- dem$z * exaggerate
  }

  # Conversion to radians for libtopotoolbox
  azimuth <- (-90 + azimuth) * pi / 180 # azimuth is measured anticlockwise
  altitude <- altitude * pi / 180

  # Computation of hillshades using libtopottolbox
  output <- single(length(dem$z))
  if (!fused) {
    result <- .C(
      "wrap_hillshade",
      outputR = as.single(output), # float
      dxR = as.single(output), # float
      dyR = as.single(output), # float
      demR = as.single(dem$z), # float
      azimuthR = as.single(azimuth), # float
      altitudeR = as.single(altitude), # float
      cellsizeR = as.single(dem$cellsize), # float
      dimsR = as.integer(dem$dims), # ptrdiff_t
      NAOK = TRUE
    )$outputR
  } else {
    result <- .C(
      "wrap_hillshade_fused",
      outputR = as.single(output), # float
      demR = as.single(dem$z), # float
      azimuthR = as.single(azimuth), # float
      altitudeR = as.single(altitude), # float
      cellsizeR = as.single(dem$cellsize), # float
      dimsR = as.integer(dem$dims), # ptrdiff_t
      NAOK = TRUE
    )$outputR
  }

  # Write result into the pre-allocated SpatRaster (terra)
  terra::values(hs) <- result
  if (provided_go) {
    hs <- GRIDobj(hs)
  }
  return(hs)
}
