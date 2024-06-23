test_that("Euclidean Distance Transformation is called in C and takes in a matrix 6x5 and puts out the disired solution as a matrix of the same dimensions", {
input <- matrix(c(0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0),6,5,byrow=TRUE);
expect_equal(EuclideanDT_inC(input),matrix(c(0,0,0,0,0,0,0,1,1,1,0,0,0,1,2,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0),6,5,byrow=TRUE));
})
