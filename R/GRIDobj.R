#' Create instance of a GRIDobj
#'
#' @description
#' GRIDobj creates an instance of the GRIDobj class, which basically builds on
#' the \code{SpatRaster} structure.
#'
#' Coordinate reference systems must be provided in PROJ-string nomenclature.
#' See the official PROJ website for further information, examples and an
#' introduction: https://proj.org/en/stable/usage/quickstart.html.
#'
#' The function will stop if the resolution of the imported data set in x
#' and y direction is not identical. In that case, consider manual import of
#' the data set via \code{terra::rast()} and resampling to identical
#' resolution (cf. \code{terra::resample()}).
#'
#' DEM = GRIDobj("path/to/file") creates a GRIDobj by reading the file
#' leveraging the \code{terra::rast()} function.
#'
#' DEM = GRIDobj(SpatRaster) creates a GRIDobj from a provided SpatRaster
#' object.
#'
#' DEM = GRIDobj(Z) creates a GRIDobj of a numeric matrix \code{Z} and a
#' default resolution of \code{1} m.
#'
#' DEM = GRIDobj(Z, cs) creates a GRIDobj from the elevations stored in
#' the matrix \code{Z} and \code{cellsize} is a positive value defining the
#' spatial resolution.
#'
#' DEM = GRIDobj(Z, cs, crs) is similar to GRIDobj(Z, cs) but it
#' sets the coordinate reference system.
#'
#' DEM = GRIDobj(Z, x, y) creates a GRIDobj from the coordinate matrices
#' or vectors X and Y and the matrix Z. The elements of Z refer to the
#' elevation of each pixel.
#'
#' DEM = GRIDobj(Z, x, y, crs) is similar to GRIDobj(Z, x, y) but it sets
#' a coordinate reference system.
#'
#' DEM = GRIDobj(GRIDobj or FLOWobj or STREAMobj, class) creates an
#' instance of GRIDobj with all common properties (e.g., spatial
#' referencing) inherited from another instance of a FLOWobj, GRIDobj
#' or STREAMobj class. DEM.Z is set to all zeros where type can be
#' integer or double or logical. By default, class is double.
#'
#' DEM = GRIDobj(GRIDobj or FLOWobj or STREAMobj, Z) creates an
#' instance of GRIDobj with all common properties (e.g., spatial
#' referencing) inherited from another instance of a FLOWobj, GRIDobj
#' or STREAMobj class. The second input argument Z is written to DEM.Z.
#'
#' DEM = GRIDobj(NULL) creates an empty instance of GRIDobj. The MATLAB
#' equivalent is \code{GRIDobj([])}.
#'
#' @param z character | SpatRaster | matrix | array | GRIDobj | FLOWobj | NULL
#'
#' character: Path to a file containing a raster to read as a GRIDobj
#'
#' SpatRaster: A SpatRaster object from terra to transform into a GRIDobj
#'
#' matrix | array: A numeric or logical matrix to transform into a GRIDobj
#'
#' GRIDobj | FLOWobj: A TTobj for which to copy common properties into a GRIDobj
#'
#' @param \dots Further arguments, either:
#'
#' crs: character string describing a coordinate reference system for
#' GRIDobj(Z, cs, crs) and GRIDobj(Z, x, y, crs)
#'
#' cs: float or integer scalar representing the cellsize of the grid for
#' GRIDobj(Z, cs)
#'
#' x: integer or float vector representing x coordinates for GRIDobj(Z, x, y)
#'
#' y: integer or float vector representing y coordinates for GRIDobj(Z, x, y)
#'
#' @returns GRIDobj
#'
#' Returns a GRIDobj containing the SpatRaster object.
#'
#' @note
#' Note that while throughout this help text GRIDobj is associated with
#' gridded digital elevation models, instances of GRIDobj can contain
#' other gridded, single band, datasets such as flow accumulation grids,
#' gradient grids etc.
#'
#' @export
GRIDobj <- function(z, ...) {
  UseMethod("GRIDobj")
}

#' @exportS3Method
GRIDobj.NULL <- function(z, ..., crs = "") {
  r <- terra::rast(
    xmin = 0, xmax = 1,
    ymin = 0, ymax = 1,
    vals = matrix(0),
    crs = crs,
    resolution = c(1, 1),
    names = ""
  )

  # Creating the actual GRIDobj
  structure(
    list(raster = r),
    class = "GRIDobj"
  )
}

#' @exportS3Method
GRIDobj.character <- function(z, ...) {
  # Input validation
  if (!validateraster(z)) {
    stop("The provided raster does not meet the requirements for use with
         topotoolboxr.", call. = FALSE)
  } else {
    r <- terra::rast(z)

    # Create GRIDobj
    structure(
      list(raster = r),
      class = "GRIDobj"
    )
  }
}

#' @exportS3Method
GRIDobj.SpatRaster <- function(z, ...) {
  # Input validation
  if (!validateraster(z)) {
    stop("The provided raster does not meet the requirements for use with
         topotoolboxr.", call. = FALSE)
  } else {
    r <- z

    # Create GRIDobj
    structure(
      list(raster = r),
      class = "GRIDobj"
    )
  }
}

#' @exportS3Method
GRIDobj.matrix <- function(z, ..., crs = "") { # Case 1: GRIDobj(Z, cs)
  args <- list(...)
  if ("cs" %in% names(args) || # (Z, cs = cs)
        (length(args) == 1 && # (Z, cs)
           is.numeric(args[[1]]))) {
    # Retrieve arguments and required info
    if ("cs" %in% names(args)) {
      cs <- args$cs
    } else {
      cs <- args[[1]]
    }
    dims <- dim_cr(z)

    # SpatRaster without georeference information is created
    r <- terra::rast(
      xmin = 0, xmax = dims[1] * cs,
      ymin = 0, ymax = dims[2] * cs,
      crs = crs,
      resolution = c(cs, cs),
      vals = c(t(z)), # Matrix to vector in row-major order
      names = ""
    )

  } else if (all(c("x", "y") %in% names(args)) || # Case 2: (Z,x,y)
               (length(args) == 2 &&
                  is.numeric(args[[1]]) &&
                  is.numeric(args[[2]]))) {
    # Retrieve coordinates and required info
    if (all(c("x", "y") %in% names(args))) { # (Z, x = x, y = y)
      x_coords <- args$x
      y_coords <- args$y
    } else { # (Z, x, y)
      x_coords <- args[[1]]
      y_coords <- args[[2]]
    }

    # Check alignment of coordinates and data
    if (!identical(dim_cr(z), c(length(x_coords), length(y_coords)))) {
      stop("Dimensions of Z and lengths of x and y do not match.")
    }

    # Determine cellsize
    dx <- unique(diff(x_coords))
    dy <- unique(diff(y_coords))
    if (length(dx) != 1 || length(dy) != 1 || !identical(dx, dy)) {
      stop("TopoToolbox requires identical resolution in x and y direction.")
    }
    cs <- dx

    # Create SpatRaster
    r <- terra::rast(
      xmin = min(x_coords - cs / 2), xmax = max(x_coords + cs / 2),
      ymin = min(y_coords - cs / 2), ymax = max(y_coords + cs / 2),
      crs = crs,
      resolution = c(cs, cs),
      vals = c(t(z)),
      names = ""
    )

  } else if (length(args) == 0) { # Case 3: GRIDobj(Z)
    dims <- dim_cr(z)

    # SpatRaster without georeference information is created
    r <- terra::rast(
      xmin = 0, xmax = dims[1],
      ymin = 0, ymax = dims[2],
      crs = crs,
      resolution = c(1, 1), # Cellsize set to 1
      vals = c(t(z)), # Matrix to vector in row-major order
      names = ""
    )
  } else {
    stop("Undefined use case for GRIDobj.matrix() provided.")
  }

  # Create GRIDobj
  structure(
    list(raster = r),
    class = "GRIDobj"
  )
}

#' @exportS3Method
GRIDobj.GRIDobj <- function(z, ...) {
  # Extract input arguments and attibutes
  r <- z$raster
  args <- list(...)

  # Determine whether class was specified
  if (length(args) == 0 ||
        "cl" %in% names(args) ||
        is.character(args[[1]])) {
    if (length(args) == 0) {
      cl <- "double" # No class provided, set to double
    } else if ("cl" %in% names(args)) {
      cl <- args$cl
    } else if (is.character(args[[1]])) {
      cl <- args[[1]]
      # Input validation
      if (!(cl %in% c("double", "integer", "int",
                      "logical", "boolean", "bool"))) {
        stop("Invalid class character provided.")
      }
    }
    # Processing the class string
    if (cl %in% c("integer", "int")) {
      terra::values(r) <- integer(length(terra::values(r)))
    } else if (cl == "double") {
      terra::values(r) <- double(length(terra::values(r)))
    } else if (cl %in% c("logical", "boolean", "bool")) {
      terra::values(r) <- logical(length(terra::values(r)))
    }
  } else if ("Z" %in% names(args) ||
               is.matrix(args[[1]] || is.array(args[[1]]))) {
    if ("Z" %in% names(args)) {
      Z <- args$Z
    } else if (is.matrix(args[[1]] || is.array(args[[1]]))) {
      Z <- args[[1]]
    }
    # Input validation
    if (!validatealignment(r, Z)) {
      stop("Size of TopoToolbox object and input matrix does not match.")
    }
    # Value assignment
    terra::values(r) <- c(t(Z)) # Matrix to vector in row-major order
  } else {
    stop("Second input must be either a character string, a vector, a matrix or
         an array.")
  }

  # Create GRIDobj
  structure(
    list(raster = r),
    class = "GRIDobj"
  )
}

#' @exportS3Method
Ops.GRIDobj <- function(e1, e2) {
  if (nargs() == 1) {
    stop("Unary operations not supported for GRIDobj")
  }

  r1 <- e1$raster
  if (inherits(e2, "GRIDobj")) r2 <- e2$raster else r2 <- e2

  op_func <- get(.Generic, envir = asNamespace("terra"))
  result <- do.call(op_func, list(r1, r2))

  GRIDobj(result)
}

#' Get the dimensions
#'
#' @description
#' `dim.GRIDobj` retrieves the  dimensions of the GRIDobj.
#'
#' @param x GRIDobj
#' @return numeric vector
#'
#' Dimensions of the GRIDobj
dim.GRIDobj <- function(x) {
  dim(x$raster)
}

#' Get the dimensions
#'
#' @description
#' `dim_cr.GRIDobj` retrieves the  dimensions of the GRIDobj in the correct
#' order for libtopotoolbox.
#'
#' @param x GRIDobj
#' @return numeric vector
#'
#' Dimensions of the GRIDobj
dim_cr.GRIDobj <- function(x) {
  c(terra::ncol(x$raster), terra::nrow(x$raster))
}

#' Change the projection
#'
#' @description
#' `reproject` changes the projection of a GRIDobj.
#'
#' @param grid GRIDobj
#' @param target character | GRIDobj | SpatRaster
#' @param ... Further arguments
#'
#' See \link[terra]{project}
#'
#' @return GRIDobj `x` with projection from `y`
#'
#' @export
reproject <- function(grid, target, ...) {
  if (!inherits(grid, "GRIDobj")) {
    stop("grid is not a GRIDobj.")
  }
  if (inherits(target, "GRIDobj")) {
    target <- target$raster
  } else if (!inherits(target, "character") ||
               !inherits(target, "SpatRaster")) {
    stop("target is no character, GRIDobj or SpatRaster.")
  }
  if (!is.lonlat(crs(target))) {
    stop("Coordinate reference system of y is not projected.")
  }
  grid$raster <- terra::project(grid$raster, target, ...)
}

#' Plot a GRIDobj
#'
#' The function plots a GRIDobj. The data structure is converted to a terra
#' SpatRaster object and then passed to the standard terra plot routine.
#'
#' For the interactive case, the following additional arguments may be useful:
#'
#' 1) \code{agg}, aggregation factor to reduce the resolution of the GRIDobj
#'    to plot, e.g. use \code{agg = 10} to reduce the size by factor 10
#'
#' 2) \code{exa}, exaggeration factor for the elevation, which is useful to
#'    improve visibility of topography. Default is \code{1} (no exaggeration).
#'
#' Available colourspace keywords can be seen on the [HCL palettes website]
#' (https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html)
#' , such as \code{"blues3"}, \code{"BuGn"}, \code{"Viridis"},
#' \code{"Plasma"}, \code{"Batlow"}.
#'
#' @param x \code{GRIDobj} to plot
#'
#' @param interactive \code{Logical} value, option to create an interactive
#' plot via \code{plotly} instead of a static map. Default is \code{FALSE}.
#' The option is only useful if the dataset is a DEM, hence the z-values
#' are elevation.
#'
#' @param \dots Further arguments passed to the plot function. These can be
#' keywords for the colour palette (e.g. \code{col = "inferno"}) and number of
#' colours (e.g. \code{n = 10}). The default colour palette is 256 colours
#' from \code{"terrain"}. See details.
#'
#' @return Graphic output of a spectrogram.
#'
#' @examples
#'
#' \dontrun{
#'
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' srtm_bigtujunga30m_utm11 <- GRIDobj(srtm_bigtujunga30m_utm11)
#'
#' ## plot data set as2D map
#' plot.GRIDobj(GRIDobj = srtm_bigtujunga30m_utm11)
#'
#' ## plot data set as 3D interactive scene (requires package plotly), note
#' ## that the scence is aggregated by factor 10 and exaggerated by factor 2
#' # plot.GRIDobj(x = srtm_bigtujunga30m_utm11,
#' #              interactive = TRUE, agg = 10, exa = 2)
#'
#' ## plot data set in customised colour
#' plot.GRIDobj(x = srtm_bigtujunga30m_utm11, col = "viridis")
#'
#' ## plot data set without legend
#' plot.GRIDobj(x = srtm_bigtujunga30m_utm11, legend = FALSE)
#'
#' }
#'
#' @author Michael Dietze, Wolfgang Schwanghart
#'
#' @exportS3Method graphics::plot
plot.GRIDobj <- function(x, interactive = FALSE, ...) {
  # Extract arguments
  r <- x$raster
  dots <- list(...)

  ## check/set number of colours n
  if ("n" %in% names(dots)) {
    exa <- dots$n
  } else {
    n <- 256
  }

  ## check/set colour scale
  if ("col" %in% names(dots)) {
    col <- dots$col
  } else {
    col <- "terrain"
  }

  ## create plot colour palette
  col_plt <- colorspace::sequential_hcl(n = n, palette = col)

  ## check/set exa
  if ("exa" %in% names(dots)) {
    exa <- dots$exa
  } else {
    exa <- 1
  }

  ## check/set xlim
  if ("xlim" %in% names(dots)) {
    xlim <- dots$xlim
  } else {
    xlim <- terra::ext(r)[1:2, ]
  }

  ## check/set ylim
  if ("ylim" %in% names(dots)) {
    ylim <- dots$ylim
  } else {
    ylim <- terra::ext(r)[3:4, ]
  }

  ## check/set zlim
  if ("zlim" %in% names(dots)) {
    zlim <- dots$zlim
  } else {
    zlim <- range(terra::values(r), na.rm = TRUE)
  }

  ## check/set legend option
  if ("legend" %in% names(dots)) {
    legend <- dots$legend
  } else {
    legend <- TRUE
  }

  ## check/set axes option
  if ("axes" %in% names(dots)) {
    axes <- dots$axes
  } else {
    axes <- TRUE
  }

  ## check/set annotation option
  if ("ann" %in% names(dots)) {
    ann <- dots$ann
  } else {
    ann <- TRUE
  }

  ## check/set add option
  if ("add" %in% names(dots)) {
    add <- dots$add
  } else {
    add <- FALSE
  }

  ## set z values out of range to max/min values
  terra::values(r)[terra::values(r) < zlim[1]] <- zlim[1]
  terra::values(r)[terra::values(r) > zlim[2]] <- zlim[2]
  ## check plot type
  if (!interactive) {

    ## plot terra raster
    terra::plot(r, xlim = xlim, ylim = ylim, zlim = zlim, col = col_plt,
                legend = legend, axes = axes, ann = ann, add = add)

  } else {

    ## check if package plotly is installed
    if (!requireNamespace("plotly", quietly = TRUE)) {

      stop("Package plotly is not installed!")
    }

    ## optionally aggregate raster
    if ("agg" %in% names(dots)) {
      r <- terra::aggregate(x = r, dots$agg)
    }

    ## optionally exaggerate vertically
    if ("exa" %in% names(dots)) {
      r <- r * exa
    }

    ## normalise extents for scaling
    rng_x <- abs(terra::ext(r)[2, ] - terra::ext(r)[1, ])
    rng_y <- abs(terra::ext(r)[4, ] - terra::ext(r)[3, ])
    rng_z <- abs(max(terra::values(r) * exa, na.rm = TRUE) -
                   min(terra::values(r) * exa, na.rm = TRUE))
    rng_max <- max(rng_x, rng_y, rng_z)

    ## plot terra raster via plotly
    fig <- plotly::plot_ly(x = unique(terra::crds(r)[, 1]),
                           y = unique(terra::crds(r)[, 2]),
                           z = matrix(terra::values(r),
                                      nrow = dim(r)[1],
                                      byrow = TRUE),
                           colors = col_plt)
    fig <- plotly::add_surface(fig)
    plotly::layout(fig,
                   scene = list(aspectmode = "manual",
                                xaxis = list(range = xlim),
                                yaxis = list(range = ylim),
                                zaxis = list(range = zlim * exa),
                                aspectratio = list(x = rng_x / rng_max,
                                                   y = rng_y / rng_max,
                                                   z = rng_z / rng_max),
                                camera = list(eye = list(x = 0,
                                                         y = 0,
                                                         z = 1.5),
                                              center = list(x = 0,
                                                            y = 0,
                                                            z = 0))))
  }

}

#' Print key variables and statistics of the GRIDobj
#'
#' @description
#' `info` retrieves metadata of the GRIDobj and computes key statistics, e.g.
#' mean, standard deviation, ...
#'
#' @param grid GRIDobj
#' @param na.rm logical
#'
#' If TRUE \code{NA} values are removed before the result is computed
#'
#' @param show_crs logical
#'
#' If TRUE the coordinate reference system is displayed as well.
#'
#' @export
info <- function(grid, na.rm = FALSE, show_crs = FALSE) {
  grid <- processgrid(grid)$r
  gd <- get_grid_data(grid)
  cat("Name:", terra::names(grid), "\n")
  dims <- dim(grid)
  cat("Rows:", dims[1], "\n")
  cat("Cols:", dims[2], "\n")
  cat("Cellsize:", gd$cellsize, "\n")
  cat("Extent:", paste(ext(grid), collapse = " "),
      "(xmin, xmax, ymin, ymax)\n")
  cat("Z values:\n")
  Z <- gd$z
  q <- stats::quantile(Z, probs = c(0.25, 0.5, 0.75), na.rm = na.rm)
  cat("- Mean:", base::mean(Z, na.rm = na.rm), "\n")
  cat("- Sd:", stats::sd(Z, na.rm = na.rm), "\n")
  cat("- Minimum:", base::min(Z, na.rm = na.rm), "\n")
  cat("- 1st Qu.:", q[1], "\n")
  cat("- Median:", q[2], "\n")
  cat("- 3rd Qu.:", q[3], "\n")
  cat("- Maximum:", base::max(Z, na.rm = na.rm), "\n")
  if (show_crs) cat("CRS:", terra::crs(grid), "\n")
}