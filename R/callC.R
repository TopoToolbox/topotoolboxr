#' EuclideanDT_inC
#'
#' callC calls the c function hello that prints out hello world. This is only a demo
#'
#' @return Matrix
#'
#' @example
#' EuclideanDT_inC()
#'
#' @export
EuclideanDT_inC<-function(binary_image,rows,cols,output){
	rx <- matrix(0,rows,cols)
	ry <- matrix(0,rows,cols)
	result_output <- .C("EuclideanDistanceTransformation",as.integer(binary_image),as.integer(rows),as.integer(cols),as.integer(output),as.integer(rx),as.integer(ry))
	return(matrix(result_output[[4]],rows,cols,byrow=TRUE))
}
#call_test_C <-function(){
#	.C("test")
#}
