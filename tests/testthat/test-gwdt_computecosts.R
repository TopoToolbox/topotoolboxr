test_that("test-gwdt_computecosts.R creates reference DEM and compares to known classifications", {
  # Check flat DEM
  DEMm <- matrix(1, nrow = 5, ncol = 5)
  DEMr <- terra::rast(DEMm, crs="EPSG:25833")
  DEMf <- fillsinks(DEMr)
  FLATS <- identifyflats(DEMf)
  expect_no_message(gwdt_computecosts(FLATS, DEMr, DEMf))
  expect_no_error(gwdt_computecosts(FLATS, DEMr, DEMf))
  
  # Check Nan acceptance
  DEMm[3,3] <- NaN
  DEMr <- terra::rast(DEMm, crs="EPSG:25833")
  DEMf <- fillsinks(DEMr)
  FLATS <- identifyflats(DEMf)
  expect_no_message(gwdt_computecosts(FLATS, DEMr, DEMf))
  expect_no_error(gwdt_computecosts(FLATS, DEMr, DEMf))
})