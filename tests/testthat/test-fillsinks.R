test_that("Tests fillsinks missing data handling against reference.", {
  # Tests on functioning
  dem <- terra::rast(system.file("ex/elev.tif", package = "terra"))
  dem <- terra::project(dem, "EPSG:32632", res = 90.0)
  expect_silent(fd <- fillsinks(GRIDobj(dem)))
  expect_true(inherits(fd, "GRIDobj"))
  expect_silent(fd <- fillsinks(dem))
  expect_true(inherits(fd, "SpatRaster"))
  expect_equal(terra::values(fillsinks(dem, hybrid = TRUE)),
               terra::values(fillsinks(dem, hybrid = FALSE)))
  vals <- terra::values(fd)
  expect_equal(c(min(vals, na.rm = TRUE),
                 mean(vals, na.rm = TRUE),
                 max(vals, na.rm = TRUE)),
               c(141.0609, 349.0915, 546.6021),
               tolerance = 1e-6)
})