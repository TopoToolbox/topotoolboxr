test_that("test-flow_routing_d8_carve.R creates a reference DEM and tests known directions and missing data handling.", {
  # Funnel shape test
  DEMm <- t(matrix(1,nrow=5,ncol=5)*c(3,2,1,2,3))*c(3,2,1,2,3)
  DEMr <- terra::rast(DEMm,crs="EPSG:25833")
  SouDir <- flow_routing_d8_carve(DEMr)
  expect_equal(as.vector(table(terra::values(SouDir$direction))),
               c(8, 5, 1, 3, 1, 3, 1, 2, 1))
  # Missing value handling
  DEMm[2,4] <- NA
  DEM <- terra::rast(DEMm ,crs="EPSG:25833")
  expect_no_error(flow_routing_d8_carve(DEM))
})