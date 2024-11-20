#' Takes a Spatrast and gives back a list with the values of the Spatrast, its cellsize and dimensions
#' @param r Takes a Spatrast



get_grid_data <- function(r) {
    
    if(terra::nlyr(r) != 1)
        stop("error: Spatrast has more than one layer");
    if(terra::xres(r) != terra::yres(r))
        stop("error: Spatrast's y and x resolutions are not equal");
    if(terra::is.lonlat(r))
        stop("error: the coordinate system is not geographic");
    return(invisible(list(z = terra::values(r,mat=FALSE),
     cellsize = terra::xres(r),
     dims = c(terra::ncol(r), terra::nrow(r)))))
    
}
