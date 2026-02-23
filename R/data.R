#' 30 m SRTM DEM for Big Tujunga catchment
#'
#' Wrapped SpatRaster of 30 m SRTM elevation data (643 × 1197 cells, 1 layer,
#' UTM 11N EPSG:32611) for Big Tujunga, California.
#'
#' @format srtm_bigtujunga30m_utm11
#' Packed SpatRaster list created by terra::wrap().
#'
#' @examples
#' library(topotoolboxr)
#' data(srtm_bigtujunga30m_utm11)
#' dem <- GRIDobj(srtm_bigtujunga30m_utm11)
#' plot(dem)
#'
#' @source USGS SRTM
"srtm_bigtujunga30m_utm11"