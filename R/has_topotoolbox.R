#' has_topotoolbox
#'
#' It is a dummy function that allows us to check for the inclusion of our Src C-functions.
#'
#' @export
has_topotoolbox <- function(a){
	return(.C(C_has_topotoolbox,as.integer(a)))
}
