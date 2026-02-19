#' Get the dimensions
#'
#' @description
#' `dim_cr` retrieves the dimensions of an object in the correct order for
#' libtopotoolbox.
#'
#' @param x matrix | array | SpatRaster | GRIDobj | FLOWobj
#' @return numeric vector
#'
#' Dimensions of the object
dim_cr <- function(x) {
  UseMethod("dim_cr")
}

#' Generic function for unravel_index
#'
#' @description
#' `unravel_index` converts flat indices into a grid grid indices.
#'
#' @param ttobj FLOWobj or STREAMobj
#'
#' Object from which the array dimensions are computed
#'
#' @param idxs vector or matrix or array
#'
#' One-dimensional indices to convert into grid indices
#'
#' @return n x 2 matrix
#'
#' Row and column indices of each pixel in the corresponding array
#'
#' @note
#' `unravel_index` currently only supports the FLOWobj.
#'
#' @export
unravel_index <- function(ttobj, idxs) {
  UseMethod("unravel_index")
}

#' Generic function for source_indices
#'
#' @description
#' `source_indices` uses `unravel_index` to compute the grid indices of
#' source pixels of a TopoToolbox object.
#'
#' @param ttobj FLOWobj or STREAMobj
#' TopoToolbox object containing the one-dimensional source indices
#'
#' @return n x 2 matrix
#'
#' Row and column indices of each source pixel in the grid.
#'
#' @note
#' `source_indices` currently only supports the FLOWobj.
#'
#' @export
source_indices <- function(ttobj) {
  UseMethod("source_indices")
}

#' Generic function for target_indices
#'
#' @description
#' `target_indices` uses `unravel_index` to compute the grid indices of target
#' pixels of a TopoToolbox object.
#'
#' @param ttobj FLOWobj or STREAMobj
#' TopoToolbox object containing the one-dimensional target indices
#'
#' @return n x 2 matrix
#'
#' Row and column indices of each target pixel in the grid
#'
#' @note
#' `target_indices` currently only supports the FLOWobj.
#'
#' @export
target_indices <- function(ttobj) {
  UseMethod("target_indices")
}

#' Generic function for ezgetnal
#'
#' @description
#' `ezgetnal` retrieves a node attribute list for a TopoToolbox object.
#'
#' @param ttobj FLOWobj or STREAMobj
#'
#' The TopoToolbox object for which to extract the node attribute list.
#'
#' @param k GRIDobj | matrix | array | float
#'
#' The object from which node values will be extracted. If `k` is a `GridObject`
#'  or a `matrix` or an `array` with the same shape as the underlying DEM of
#'  this `TTobj`, the node values will be extracted from the grid by indexing.
#'  If `k` is an array with the same shape as the node attribute list,
#'  `ezgetnal` returns a copy of `k`. If `k` is a scalar value, `ezgetnal`
#'  returns an array of the right shape filled with `k`.
#'
#' @export
ezgetnal <- function(ttobj, k) {
  UseMethod("ezgetnal")
}
