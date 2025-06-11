#' wrap_gradient8
#'
#' This will make the Gradient8 function available to R from the libtotopotoolbox subdirectory
#' 
#' @param dem Input of type Spatrast from Terra
#' @param use_mp Future feature that will allow to parallelize the code
#'
#' @import terra
#'
#' @return A Spatrast
#' @export

gradient8 <- function(dem,use_mp=0) {
  
    # Extract input data
    d <- get_grid_data(dem)
    
    # Handle missing data
    fill_value <- min(d$z, na.rm=T) - 999
    log_nans <- is.na(d$z)
    d$z[log_nans] <- fill_value
    
    # Compute gradient8 using libtopotoolbox
    output <- single(length(d$z))
    result <- .C("wrap_gradient8",outputR=as.single(output),as.single(d$z),as.single(d$cellsize),as.integer(use_mp), as.integer(d$dims))$outputR
    result[log_nans] <- NaN
    
    # Store results as terra SpatRaster
    G <- dem
    terra::values(G) <- result
    
    return(G)
}
