#' Checks a SpatRaster for GRIDobj requirements
#'
#' @description
#' `validateraster` checks whether a provided raster fulfills the requirements
#' of a TopoToolbox GRIDobj.
#'
#' @param raster character | SpatRaster
#'
#' The input can be either a path to a file containing the raster or a
#' SpatRaster from terra
#'
#' @return logical
#'
#' Returns TRUE if the raster fulfills the requirements
validateraster <- function(raster) {
  if (inherits(raster, "character")) {
    # Check path validity
    if (!file.exists(raster)) {
      stop("File does not exist: '", raster, "'", call. = FALSE)
    }
    # Read the file as a SpatRaster
    r <- terra::rast(raster)
  } else if (inherits(raster, "SpatRaster")) {
    r <- raster
  } else {
    stop("Input must be a file path (character) or SpatRaster object. ",
         "Got class: '", paste(class(raster), collapse = "', '"), "'",
         call. = FALSE)
  }

  # Raster checks
  if (terra::nlyr(r) > 1) {
    stop("Input must be single-layer SpatRaster. Found: ", terra::nlyr(r),
         " layers", call. = FALSE)
  }
  if (!identical(terra::xres(r), terra::yres(r))) {
    stop(sprintf("X/Y resolutions must be identical. Found: xres=%.3f,
                  yres=%.3f", terra::xres(r), terra::yres(r)), call. = FALSE)
  }
  if (!identical(terra::crs(r), "")) {
    if (terra::is.lonlat(r)) {
      stop("Input must use projected CRS (not lon/lat).", call. = FALSE)
    }
  }
  TRUE
}

#' Checks a SpatRaster for GRIDobj requirements
#'
#' @description
#' `validateraster` checks whether a provided raster fulfills the requirements
#' of a TopoToolbox GRIDobj.
#'
#' @param grid GRIDobj | SpatRaster
#'
#' Input must be either a GRIDobj or SpatRaster
#'
#' @return list
#'
#' A list with the components 'r', the SpatRaster corresponding to the input
#' and 'provided_go', a logical value stating whether the input was a GRIDobj
processgrid <- function(grid) {
  # Input validation
  if (!(inherits(grid, "GRIDobj") || inherits(grid, "SpatRaster"))) {
    stop("Input must be of class 'GRIDobj' or 'SpatRaster'.", call. = FALSE)
  }

  # Processing
  if (inherits(grid, "GRIDobj")) {
    grid <- grid$raster
    provided_go <- TRUE
  } else if (inherits(grid, "SpatRaster")) {
    provided_go <- FALSE
  }
  list(r = grid,
       provided_go = provided_go)
}

#' Get grid data
#'
#' @description
#' `get_grid_data` retrieves the values, cellsize and dimension of a provided
#' GRIDobj or SpatRaster.
#'
#' @param dem GRIDobj | SpatRaster
#'
#' Object for which to retrieve grid data
#'
#' @returns list
#'
#' A list with the components 'z', a vector containing the grid data in
#' row-major order, 'cellsize', the horizontal resolution of the grid, and
#' 'dims' a two-element vector with the number of columns and rows of the grid.
get_grid_data <- function(dem) {
  if (inherits(dem, "SpatRaster")) {
    list(z = terra::values(dem, mat = FALSE),
         cellsize = terra::xres(dem),
         dims = dim_cr(dem))
  } else {
    stop("Input must be of class 'SpatRaster'.")
  }
}