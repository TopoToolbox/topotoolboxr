test_that("test-gwdt_computecosts.R creates reference DEM and computes known costs and connected components.", {
  DEMm <- matrix(1,nrow=7,ncol=5)*1:7
  DEMm[2:6,c(2, 4)] = 1
  DEMm[7,3] = NA
  DEMr <- terra::rast(DEMm,crs="EPSG:25833")
  DEMp <- fillsinks(DEMr)
  FLATS <- identifyflats(DEMp)
  costs <- gwdt_computecosts(flats = FLATS, original_dem = DEMr, filled_dem = DEMp)
  expect_equal(as.vector(unique(terra::values(costs))), c(0.0, 0.1))
  expect_no_message(gwdt_computecosts(FLATS, DEMr, DEMp))
  expect_no_error(gwdt_computecosts(FLATS, DEMr, DEMp))
})