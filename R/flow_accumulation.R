#' Compute accumulation of flow
#'
#' @description
#' Computes the flow accumulation for a given flow network using optional
#' weights. The flow accumulation represents the amount of flow each cell
#' receives from its upstream neighbors.
#'
#' @param fd FLOWobj
#'
#' The FLOWobj that will be the basis of the computation.
#'
#' @param weights numeric array or matrix, optional
#'
#' An array of the same shape as the flow grid representing weights for each
#' cell, or a constant float value used as the weight for all cells.
#' If `weights=1.0` (default), the flow accumulation is unweighted.
#' If an ndarray is provided, it must match the shape of the flow grid., by
#' default 1.0
#'
#' @return GRIDobj
#'
#' A new GRIDobj containing the flow accumulation grid.
#'
#' @examples
#' \dontrun{
#' DEM <- terra::rast(system.file("ex/elev.tif",package="terra"))
#' DEM <- terra::project(DEM,"EPSG:32632",res=90.0)
#' FD <- FLOWobj(DEM)
#' FA <- flow_accumulation(FD)
#' }
#'
#' @export

flow_accumulation <- function(fd,
                              weights = 1.0) {
  # Input checks
  if (!inherits(fd, "FLOWobj")) {
    stop("fd must be a FLOWobj.")
  }

  # Input delineations
  dims <- dim_cr(fd)
  edge_count <- length(fd$source)

  # Compute flow routing using libtopotoolbox
  output <- single(prod(dims))
  result <- .C(
    "wrap_flow_accumulation_edgelist",
    accR = as.single(output), # float
    sourceR = as.integer(fd$source), # ptrdiff_t
    targetR = as.integer(fd$target), # ptrdiff_t
    fractionR = as.single(rep(1, edge_count)), # float
    weightsR = as.single(ezgetnal(fd, weights)), # float
    edge_countR = as.integer(edge_count), # ptrdiff_t
    dimsR = as.integer(dims), # ptrdiff_t
    NAOK = TRUE
  )$accR

  # Write result into SpatRaster (for now)
  fa <- fd$raster
  terra::values(fa) <- result
  GRIDobj(fa)
}