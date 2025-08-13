test_that("test-flow_routing_d8_edgelist.R creates a reference DEM and tests known sources, targets and missing data handling.", {
  DEMm <- t(matrix(1,nrow=5,ncol=7)*c(2,1,1,1,2))*c(3,2,2,2,3,4,5)
  DEMm[3,3] <- 1
  DEMm[4,3] <- 1
  DEMr <- terra::rast(DEMm,crs="EPSG:25833")
  ST <- flow_routing_d8_edgelist(DEMr)
  expect_equal(length(ST$source), 32)
  expect_equal(ST$source,
               c(34, 33, 32, 30, 31, 29, 28, 27, 25, 26, 24, 23, 22, 20, 21, 19, 18, 15, 16, 17, 14, 13, 12, 10, 11,  9, 8,  7,  5,  6,  4,  0))
  expect_equal(ST$target,
               c(33, 28, 27, 31, 26, 28, 23, 22, 26, 21, 23, 17, 17, 21, 17, 18, 17, 16, 17, 12, 13,  8, 7, 11,  6,  8,  3,  2,  6,  1,  3,  1))
  # Missing value handling
  DEMm[3,3] <- NA
  DEM <- terra::rast(DEMm ,crs="EPSG:25833")
  expect_no_error(flow_routing_d8_edgelist(DEM))
})