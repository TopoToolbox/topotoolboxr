#' fillsinks
#'
#'@description Fill sinks in the digital elevation model (DEM).
#'
#' @param dem GRIDobj | SpatRaster
#'
#' Digital elevation model
#'
#' @param bc numeric array | numeric matrix, optional
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
#' @return GRIDobj | SpatRaster
#'
#' Filled digital elevation model
#'
#' @export

fillsinks <- function(dem,
                      bc = NULL,
                      hybrid = TRUE) {
  # Input validation
  dem <- processgrid(dem)
  provided_go <- dem$provided_go
  dem <- dem$r

  # Extract DEM data for further checks
  d <- get_grid_data(dem)

  # Check requirements for bc provided by user
  if (!is.null(bc)) {
    if ((is.matrix(bc) || is.array(bc)) && !all(bc %in% c(0, 1))) {
      # Check if bc is (matrix OR array) AND all values either 0 or 1
      stop("Input 'bc' must be NULL, numeric matrix/array of 0s and 1s. ",
           "Found class: '", paste(class(bc), collapse = "', '"), "'",
           call. = FALSE)
    }
    #if (all(d$dims != c(nrow(bc), ncol(bc)))){
    if (validatealignment(dem, bc)) {
      stop("'dem' and 'bc' dimensions do not match.")
    }
  } else { # If the user does not provide bcs they are manually created
    bc <- matrix(0, d$dims[1], d$dims[2])
    # Missing data and borders are fixed to their original value
    bc[is.na(terra::values(dem))] <- 1
    bc[1, ] <- 1
    bc[, 1] <- 1
    bc[nrow(bc), ] <- 1
    bc[, ncol(bc)] <- 1
  }
  if (!is.logical(hybrid)) {
    stop("'hybrid' must be logical.")
  }

  # Handling missing data
  fill_value <- min(d$z, na.rm = TRUE) - 999
  nans <- is.na(d$z)
  d$z[nans] <- fill_value

  # Fill sinks using libtopotoolbox
  output <- single(length(d$z))
  if (hybrid) {
    result <- .C(
      "wrap_fillsinks_hybrid",
      outputR = as.single(output), # float
      as.single(d$z), # float
      as.integer(bc), # uint8_t
      as.integer(d$dims) # ptrdiff_t
    )$outputR
  } else {
    result <- .C(
      "wrap_fillsinks",
      outputR = as.single(output), # float
      as.single(d$z), # float
      as.integer(bc), # uint8_t
      as.integer(d$dims) # ptrdiff_t
    )$outputR
  }

  # Reintroducing missing data
  result[nans] <- NaN

  # Overwrite DEM with filled values and optionally transform to GRIDobj
  terra::values(dem) <- result
  if (provided_go) {
    dem <- GRIDobj(dem)
  }
  dem
}