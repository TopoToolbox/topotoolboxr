#' wrap_gradient8
#'
#' Computes the gradient of a digital elevation model (DEM) using an 8-direction
#' algorithm, making the Gradient8 function available to R from the
#' libtotopotoolbox subdirectory.
#'
#' @param dem GRIDobj | SpatRaster
#'
#' Digital elevation model
#'
#' @param unit Unit of returned gradient values. Options are:
#'
#'   - 'tangent': Calculate the gradient as a tangent (default).
#'
#'   - 'radian': Calculate the gradient in radians.
#'
#'   - 'degree': Calculate the gradient in degrees.
#'
#'   - 'sine': Calculate the gradient as the sine of the angle.
#'
#'   - 'percent': Calculate the gradient as a percentage.
#'
#' @param use_mp Logical
#'
#' If TRUE, use parallel processing for computation (future feature).
#' Currently not implemented.
#'
#' @return 8-connected neighborhood gradient of a digital elevation model
#'
#' @examples
#' \dontrun{
#' DEM <- terra::rast(system.file("ex/elev.tif",package="terra"))
#' DEM <- terra::project(DEM,"epsg:32632",res=90.0)
#' g <- gradient8(DEM)
#' plot(g)
#' }
#'
#' @export

gradient8 <- function(dem, unit = "tangent", use_mp = 0) {
  # Input validation and value extraction
  dem <- processgrid(dem)
  provided_go <- dem$provided_go
  dem <- dem$r

  # Extract input data
  d <- get_grid_data(dem)

  # Compute gradient8 using libtopotoolbox
  output <- single(length(d$z))
  result <- .C(
    "wrap_gradient8",
    outputR = as.single(output),
    as.single(d$z),
    as.single(d$cellsize),
    as.integer(use_mp),
    as.integer(d$dims),
    NAOK = TRUE
  )$outputR
  result[is.na(d$z)] <- NA

  # Unit conversion
  if (unit == "degree") {
    result <- atan(result) * 180 / pi
  } else if (unit == "radian") {
    result <- atan(result)
  } else if (unit == "sine") {
    result <- sin(atan(result))
  } else if (unit == "percent") {
    result <- result * 100
  }

  # Return results as provided class
  g <- dem
  terra::values(g) <- result
  if (provided_go) {
    g <- GRIDobj(g)
  }
  g
}
