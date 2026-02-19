test_that("test-gwdt.R creates reference dem and computes known distances.", {
  dem <- matrix(1, nrow = 7, ncol = 5) * 1:7
  dem[2:6, c(2, 4)] <- 1
  dem[7, 3] <- NA
  dem <- terra::rast(dem, crs = "EPSG:25833")
  expect_silent(dist <- gwdt(GRIDobj(dem)))
  expect_true(inherits(dist, "GRIDobj"))
  expect_silent(dist <- gwdt(dem))
  expect_true(inherits(dist, "SpatRaster"))
  expect_equal(round(as.vector(unique(terra::values(dist))), 1),
               c(0, 1.0, 1.1))
})