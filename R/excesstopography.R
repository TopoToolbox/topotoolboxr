#' Excess topography
#'
#' Reconstruct surface with threshold-slope surface
#'
#' The excess topography (Blöthe et al. 2015) is computed by solving
#' an eikonal equation (Anand et al. 2023) constrained to lie below
#' the original DEM. Where the slope of the DEM is greater than the
#' threshold slope, the eikonal solver limits the output topography to
#' that slope, but where the slope of the DEM is lower that the
#' threshold slope, the output follows the DEM.
#'
#' The eikonal equation is solved using the fast sweeping method (Zhao
#' 2004), which iterates over the DEM in alternating directions and
#' updates the topography according to an upwind discretization of the
#' gradient. To constrain the solution by the original DEM, the output
#' topography is initiated with the DEM and only updates lower than
#' the DEM are accepted.
#'
#' @param dem GRIDobj; Digital elevation model
#' @param threshold_slope float | GRIDobj; The threshold
#'     slope. Spatially variable slopes can be provided as a GRIDobj.
#'
#' @return GRIDobj; The reconstructed topography. The excess
#'     topography is the difference between the original DEM and the
#'     reconstructed one.
#'
#' @examples
#'
#' \dontrun{
#'
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#' DEMext <- excesstopography(DEM, tan(20*pi/180))
#'
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#' R <- randomize(DEM,0,.5)
#' E <- excesstopography(DEM,R)
#' plot(hillshade(E),col = "Grays")
#' 
#' }
#' 
#' @export

excesstopography <- function(dem, threshold_slope) {
  # Input validation
  dem <- processgrid(dem)
  provided_go <- dem$provided_go
  dem <- dem$r

  # Preallocate output
  ext <- dem

  # Get DEM input
  dem <- get_grid_data(dem)

  # Pass grids to libtopotoolbox
  if (is.numeric(threshold_slope)) {
    threshold_grid <- as.single(rep(threshold_slope, length(dem$z)))
  } else {
    threshold_grid <- as.single(get_grid_data(processgrid(threshold_slope)$r)$z)
  }
  output <- single(length(dem$z))
  result <- .C(
    "wrap_excesstopography",
    outputR = as.single(output),
    demR = as.single(dem$z),
    thresholdR = threshold_grid,
    cellsizeR = as.single(dem$cellsize),
    dimsR = as.integer(dem$dims),
    NAOK = TRUE
  )$outputR

  terra::values(ext) <- result
  if (provided_go) {
    ext <- GRIDobj(ext)
  }
  return(ext)
}
