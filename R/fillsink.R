#' wrap_fillsink
#'
#' This will make the fillsinks funktion available to R from the libtotopotoolbox subdirectory
#' 
#' @param dem Takes a Spatrast as input 
#' @param bc Array of the same size as the input matrix
#'
#' @return A Spatrast
#' @export

fillsinks <- function(dem,bc=NULL) {
    
    d <- get_grid_data(dem) # Extract input data
    # if user didn't set bc, then set it manual
    if (is.null(bc))
    {
        bc <- matrix(0,d$dims[1],d$dims[2])
        bc[terra::values(dem)=="NaN"] <- 1
        bc[1,] <- 1
        bc[,1] <- 1
        # set last row to 1
        bc[nrow(bc),] <- 1
        # set last col to 1
        bc[,ncol(bc)] <- 1
    }

    output <- single(length(d$z)) #create output array
    fill_value = min(d$z, na.rm=TRUE) - 999
    nans = is.na(d$z)
    d$z[nans] = fill_value

    result <- .C("wrap_fillsink",outputR=as.single(output),as.single(d$z),as.integer(bc),as.integer(d$dims))$outputR

    result[nans] = NaN

    demf <- dem
    terra::values(demf) <- result
    return(demf);
}
