#' Convert several GRIDobj objects to a predictor matrix
#'
#' The function organises all input \code{GRIDobj} values columnwise into
#' a matrix. It then identifies row (i.e. shared pixels) that exhibit NA
#' values and removed those. Note that all input data set must match in
#' terms of spatial extent and resolution.
#'
#' @param \dots \code{GRIDobj} objects to be processed
#'
#' @return A \code{list} with \code{X}, the predictor matrix (i.e.
#' column-wise organised grid values pruned from NA-values), \code{ix}, an
#' index vector indicating the positions of the NA-free values in the original
#' data sets, and \code{txt}, the names of the input data sets.
#'
#' @examples
#'
#' \dontrun{
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#'
#' ## get gradient, curvature and logs of flow accumulation values
#' G <- gradient8(dem = DEM)
#' C <- curvature(GRIDobj = DEM)
#' A <- flowacc(GRIDobj = fillsinks(GRIDobj = DEM))
#' terra::values(A)[,1] <- log(terra::values(A)[,1])
#'
#' ## build predictor matrix from gradient, curvature and log flow accumulation 
#' P <- GRIDobj2pm(gradient = G, curvature = C, logflow = A)
#'
#' ## normalise values
#' P$X <- (P$X - mean(P$X)) / sd(P$X)
#'
#' ## do simple cluster analysis
#' km <- kmeans(x = P$X, centers = 5)
#'
#' ## assign cluster values to new landform GRIDobj
#' landforms <- DEM
#' terra::values(landforms)[P$ix,1] <- km$cluster
#'
#' ## plot classified data set
#' plot_GRIDobj(landforms)
#' }
#'
#' @author Michael Dietze, Wolfgang Schwanghart
#'
#' @export GRIDobj2pm
GRIDobj2pm <- function(...) {

  ## CHECKS OF INPUT DATA -----------------------------------------------------

  ## extract contained objects
  L <- list(...)

  ## check input data sets for object type
  for (i in 1:length(L)) {

    if (inherits(L[[i]], "GRIDobj")) {
      L[[i]] <- L[[i]]$raster
    } else if (!inherits(L[[i]], "SpatRaster")) {
      stop(paste0(names(L)[i], " seems to be no GRIDobj or SpatRaster!"))
    }
  }

  ## check input data for matching extent and grid size
  L_ext <- lapply(X = L, FUN = terra::ext)
  L_res <- lapply(X = L, FUN = terra::res)

  ## test all data sets
  for(i in 2:length(L)) {
    ## test matching extent
    if(L_ext[[1]] != L_ext[[i]]) {
      stop(paste("Extents of", names(L)[1], "and",
                 names(L)[i], "do not match!"))
    }

    ## test matching resolution
    if(any(L_res[[1]] != L_res[[i]]) == TRUE) {
      stop(paste("Resolutions of", names(L)[1], "and",
                 names(L)[i], "do not match!"))
    }
  }

  ## DO THE ANALYSIS ----------------------------------------------------------

  ## organise input data values as columnwise bind matrix
  X <- do.call(cbind, lapply(X = L, FUN = function(L) {terra::values(L)[, 1]}))

  ## create row index
  ix <- 1:nrow(X)

  ## identify non-NA values
  I <- apply(X = X, MARGIN = 1, FUN = function(x) {!any(is.na(x))})

  ## remove NA-cases
  X <- X[I, ]

  ## collect indices
  ix <- ix[I]

  ## RETURN OUTPUT ------------------------------------------------------------

  ## return output
  return(list(X = X,
              ix = ix,
              txt = names(L)))
}
