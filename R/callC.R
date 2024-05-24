#' callC
#'
#' callC calls the c function hello that prints out hello world. This is only a demo
#'
#' @return None
#'
#' @example
#' callC()
#'
#' @export
callC <-function(){
.C("test")
}
