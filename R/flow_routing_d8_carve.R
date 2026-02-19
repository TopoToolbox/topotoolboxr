#' wrap_flow_routing_d8_carve
#'
#' Compute the flow routing using the D8 algorithm with carving for flat areas.
#'
#' @param dem GRIDobj | SpatRaster
#'
#' Digital elevation model
#'
#' @param bc numeric array | matrix, optional
#'
#' Boundary conditions for sink filling. `bc` should match the shape of the DEM.
#' Values of 1 indicate pixels that should be fixed to their values in the
#' original DEM and values of 0 indicate pixels that should be filled.
#'
#' @param hybrid logical, optional
#'
#' Should hybrid reconstruction algorithm be used to fill sinks? Defaults to
#' True. Hybrid reconstruction is faster but requires additional memory be
#' allocated for a queue.
#'
#' @return list
#'
#' A list containing two GRIDobj or SpatRaster representing the source cells for
#' flow routing (source) and the flow direction for each grid cell (direction).
#'
#' @export

flow_routing_d8_carve <- function(dem,
                                  bc = NULL,
                                  hybrid = TRUE) {

  # Input validation and value extraction
  dem <- processgrid(dem)
  provided_go <- dem$provided_go
  dem <- dem$r

  # Prepare inputs for flow routing based on raw DEM
  demf <- fillsinks(dem, bc = bc, hybrid = hybrid)
  flats <- identifyflats(demf)
  dist <- gwdt(dem)

  df <- get_grid_data(demf)
  di <- get_grid_data(dist)
  fl <- get_grid_data(flats)

  # Compute flow routing using libtopotoolbox
  outputs <- single(length(df$z))
  results <- .C(
    "wrap_flow_routing_d8_carve",
    sourceR = as.integer(outputs), # ptrdiff_t
    directionR = as.integer(outputs), # uint8_t
    demR = as.single(df$z), # float
    distR = as.single(di$z), # float
    flatsR = as.integer(fl$z), # uint32_t
    dimsR = as.integer(df$dims), # ptrdiff_t
    NAOK = TRUE
  )

  # Write outputs into SpatRaster
  source <- demf
  terra::values(source) <- results$sourceR
  direction <- demf
  terra::values(direction) <- results$directionR
  if (provided_go) {
    source <- GRIDobj(source)
    direction <- GRIDobj(direction)
  }
  list("source" = source, "direction" = direction)
}
