test_that("test-hillshade.R checks missing data handling, ensures both methods raise identical results and compares known values", {
  DEM <- terra::rast(system.file("ex/elev.tif",package="terra"))
  DEM <- terra::project(DEM,"EPSG:32632",res=90.0)
  expect_no_error(hillshade(DEM, fused = TRUE))
  HS <- hillshade(DEM, fused = TRUE)
  expect_equal(terra::values(HS),
               terra::values(hillshade(DEM, fused = FALSE)))
  expect_equal(mean(terra::values(HS), na.rm = TRUE),
               0.863098,
               tolerance = 1e-6)
  expect_equal(max(terra::values(HS), na.rm = TRUE),
               0.940817,
               tolerance = 1e-6)
  expect_equal(min(terra::values(HS), na.rm = TRUE),
               0.760938,
               tolerance = 1e-6)
})