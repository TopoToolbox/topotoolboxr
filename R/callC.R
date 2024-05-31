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
callC<-function(){
	output <- matrix(0,5,5)
	rows <- 5
	cols <- 5
	input <- matrix(c(
		1,0,0,0,0,
		0,1,1,1,0,
		0,1,1,1,0,
		0,1,1,1,0,
		0,0,0,0,0
	),5,5)
	result_output <- .C("EuclideanDistanceTransformation",as.integer(input),as.integer(rows),as.integer(cols),as.integer(output))
	print(result_output)
}
#call_test_C <-function(){
#	.C("test")
#}
