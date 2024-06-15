test_that("Euclidean Distance Transformation is called in C and takes in a matrix 6x5 and puts out the disired solution as a matrix of the same dimensions", {
output <- matrix(0,6,5);
rows <- 6;
cols <- 5;
input <- matrix(c(0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0),6,5);
expect_equal(EuclideanDT_inC(input,rows,cols,output),matrix(c(0,0,0,0,0,0,0,1,1,1,0,0,0,1,2,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0),6,5));
})
