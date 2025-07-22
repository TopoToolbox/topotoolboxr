#' wrap_gwdt_computecosts
#'
#' This function makes the libopotoolbox cost computation for the gray-weighted 
#' distance transform available in R
#' 
#' @param flats (terra::SpatRaster) Flat pixels as returned by identifyflats()
#' @param original_dem (terra::SpatRaster) Raw digital elevation model 
#' @param filled_dem (terra::SpatRaster) Processed DEM
#'
#' @import terra
#'
#' @return List containing gray-weighted distance transform costs and labeled
#' connected components for each flat pixel
#' 
#' @export

gwdt_computecosts <- function(flats, original_dem, filled_dem){
  fl <- get_grid_data(flats)
  dr <- get_grid_data(original_dem)
  df <- get_grid_data(filled_dem)

  # Check inputs
  ## Grid dimensions
  if (!identical(fl$dims, dr$dims) || !identical(fl$dims, df$dims)) {
    stop("All input grids must have the same dimensions.")
  }
  ## Range of values for flats
  if (!all(unique(terra::values(flats)) %in% c(0,1,2,5))){
    stop("flats contains invalid values.")
  }

  # Compute costs using libtopotoolbox
  outputs <- single(length(fl$z))
  results <- .C("wrap_gwdt_computecosts",
                costsR = as.single(outputs),
                conncompsR = as.integer(outputs),
                flatsR = as.integer(fl$z),
                original_demR = as.single(dr$z),
                filled_demR = as.single(df$z),
                dimsR = as.integer(fl$dims),
                NAOK = TRUE)

  # Write results into SpatRaster
  costs <- flats
  terra::values(costs) <- results$costsR
  comps <- flats
  terra::values(comps) <- results$conncompsR

  return(list("costs" = costs,
              "conncomps" = comps))
}
