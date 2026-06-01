#' randomize
#'
#' The function returns a GRIDobj with random uniform number.
#
#' @param x \code{GRIDobj} 
#'
#' @param \dots Further arguments passed to the runif function
#' 
#' @return GRIDobj
#'
#' @examples
#'
#' \dontrun{
#'
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#' R <- randomize(DEM)
#' }
#'
#' @author Wolfgang Schwanghart
#'
#' @export
randomize <- function(x, ...) {
  
  n <- prod(dim_cr(x))
  values(x$raster) <- stats::runif(n,...)
  
  x
}