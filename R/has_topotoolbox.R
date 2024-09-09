#' has_topotoolbox
#'
#' It is a dummy function that allows us to check for the inclusion of our Src C-functions.
#'
#' @export
has_topotoolbox <- function(){
    a <- .C(C_wrap_has_topotoolbox,a=as.integer(0))$a
    return(a == 1)
}
