test_that("Tests hillshade methods and missing data handling", {
  dem <- terra::rast(system.file("ex/elev.tif", package = "terra"))
  dem <- terra::project(dem, "EPSG:32632", res = 90.0)
  expect_silent(hs <- hillshade(GRIDobj(dem), fused = TRUE))
  expect_true(inherits(hs, "GRIDobj"))
  expect_silent(hs <- hillshade(dem, fused = TRUE))
  expect_true(inherits(hs, "SpatRaster"))
  expect_equal(terra::values(hs),
               terra::values(hillshade(dem, fused = FALSE)))
  expect_equal(mean(terra::values(hs), na.rm = TRUE),
               0.863098,
               tolerance = 1e-6)
  expect_equal(max(terra::values(hs), na.rm = TRUE),
               0.940817,
               tolerance = 1e-6)
  expect_equal(min(terra::values(hs), na.rm = TRUE),
               0.760938,
               tolerance = 1e-6)
})