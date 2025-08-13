#' Sink filling
#'
#' Fill sinks in the digital elevation model (DEM).
#' 
#' @param DEM SpatRaster (terra); Digital elevation model 
#' @param bc numeric array or matrix; Boundary conditions for sink filling. `bc` should match the shape of the DEM. Values of 1 indicate pixels that should be fixed to their values in the original DEM and values of 0 indicate pixels that should be filled.
#' @param hybrid logical; Should hybrid reconstruction algorithm be used? Defaults to True. Hybrid reconstruction is faster but requires additional memory be allocated for a queue.
#'
#' @return SpatRaster (terra); The filled DEM
#' @export

fillsinks <- function(DEM,
                      bc = NULL,
                      hybrid = TRUE) {
  # Input validation
  if (!inherits(DEM, "SpatRaster")) {
    stop("`DEM` must be a terra SpatRaster object")
  }
  
  # Extract DEM data for further checks
  d <- get_grid_data(DEM)
  
  # Check requirements for bc provided by user
  if (!is.null(bc)){
    if((is.matrix(bc)|is.array(bc))&!all(bc %in% c(0,1))) {
      # Check if bc is (matrix OR array) AND all values either 0 or 1
      stop("`bc` must be either NULL, a matrix or an array containing 0s and 1s")
    }
    if(all(d$dims != c(nrow(bc), ncol(bc)))){
      stop("`DEM` and `bc` dimensions do not match")
    }
  } else { # If the user does not provide bcs they are manually created
    bc <- matrix(0,d$dims[1],d$dims[2])
    # Missing data and borders are fixed to their original value
    bc[is.na(terra::values(DEM))] <- 1
    bc[1,] <- 1
    bc[,1] <- 1
    bc[nrow(bc),] <- 1
    bc[,ncol(bc)] <- 1
  }
  if (!is.logical(hybrid)){
    stop("`hybrid` has to be boolean")
  }
  
  # Handling missing data
  fill_value = min(d$z, na.rm=TRUE) - 999
  nans = is.na(d$z)
  d$z[nans] = fill_value
  
  # Fill sinks using libtopotoolbox
  output <- single(length(d$z))
  if (hybrid){
    result <- .C("wrap_fillsinks_hybrid",
                 outputR=as.single(output), # float
                 as.single(d$z), # float
                 as.integer(bc), # uint8_t
                 as.integer(d$dims) # ptrdiff_t
                 )$outputR
  } else {
    result <- .C("wrap_fillsinks",
                 outputR=as.single(output), # float
                 as.single(d$z), # float
                 as.integer(bc), # uint8_t
                 as.integer(d$dims) # ptrdiff_t
                 )$outputR
  }
  
  # Reintroducing missing data
  result[nans] = NaN
  
  # Overwrite DEM with filled values
  terra::values(DEM) <- result
  return(DEM)
}