#' wrap_gwdt
#'
#' Perform the grey-weighted distance transform (GWDT) on the DEM.
#' 
#' @param DEM (terra::SpatRaster) Digital elevation model
#'
#' @import terra
#'
#' @return A terra::SpatRaster representing the GWDT distances for each grid cell.
#' 
#' @export

gwdt <- function(DEM){
  DEMf <- fillsinks(DEM)
  FLATS <- identifyflats(DEMf)
  COSTS <- gwdt_computecosts(FLATS, DEM, DEMf)
  
  fl <- get_grid_data(FLATS)
  co <- get_grid_data(COSTS)
  
  # Compute costs using libtopotoolbox
  outputs <- single(length(co$z))
  result <- .C("wrap_gwdt",
               distR = as.single(outputs), # float
               costsR = as.single(co$z), # float
               flatsR = as.integer(fl$z), # int32_t
               dimsR = as.integer(fl$dims), # ptrdiff_t
               NAOK = TRUE)$distR
  
  # Write results into SpatRaster
  dist <- DEM
  terra::values(dist) <- result
  
  return(dist)
}