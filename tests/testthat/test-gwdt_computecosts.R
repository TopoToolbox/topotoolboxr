test_that("Tests gwdt_computecosts on reference DEM", {
  demm <- matrix(1, nrow = 7, ncol = 5) * 1:7
  demm[2:6, c(2, 4)] <- 1
  demm[7, 3] <- NA
  demr <- terra::rast(demm, crs = "EPSG:25833")
  demp <- fillsinks(demr)
  flats <- identifyflats(demp)
  expect_silent(costs <- gwdt_computecosts(flats = GRIDobj(flats),
                                           original_dem = demr,
                                           filled_dem = demp))
  expect_true(inherits(costs, "GRIDobj"))
  expect_silent(costs <- gwdt_computecosts(flats = flats,
                                           original_dem = demr,
                                           filled_dem = GRIDobj(demp)))
  expect_true(inherits(costs, "GRIDobj"))
  expect_silent(costs <- gwdt_computecosts(flats = flats,
                                           original_dem = demr,
                                           filled_dem = demp))
  expect_true(inherits(costs, "SpatRaster"))
  expect_equal(as.vector(unique(terra::values(costs))), c(0.0, 0.1))
})
