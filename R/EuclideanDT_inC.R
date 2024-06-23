#' EuclideanDT_inC
#'
#' This function cals a C function that calculates the Euclidean Distance Transformation in two scans for a given binary image, consisting of Zero and Ones, where the Algorithm replaces the One tiles with the euclidean distance to the nearist Zero. The function referes to the paper "Fast Euclidean distance transformation
#'in two scans using a 3 3 neighborhood" by Frank Y. Shih* and Yi-Ta Wu  
#'
#' @return A integer matrix
#' @param binary_image A matrix with Zeros and Ones
#' 
#' @examples
#' EuclideanDT_inC(matrix(c(0,0,0,0,0,
#'	    0,1,1,1,0,
#'	    0,1,1,1,0,
#'	    0,1,1,1,0,
#'	    0,0,0,0,0,
#'	    0,0,0,0,0),6,5,byrow=TRUE))
#'
#' @export
EuclideanDT_inC<-function(binary_image){
	cols <- NCOL(binary_image)
	rows <- NROW(binary_image)
	rx <- matrix(0,rows,cols)
	ry <- matrix(0,rows,cols)
	output <- matrix(0,rows,cols)
	result_output <- .C("EuclideanDistanceTransformation",as.integer(binary_image),as.integer(rows),as.integer(cols),as.integer(output),as.integer(rx),as.integer(ry))
	return(matrix(result_output[[4]],rows,cols,byrow=TRUE))
}
