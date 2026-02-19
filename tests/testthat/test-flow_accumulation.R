test_that("Tests flow_accumulation inputs/outputs on synthetic data.", {
  # Create reference features for synthetic DEMs
  funnel <- t(matrix(1, 5, 5) * c(3, 2, 1, 2, 3)) * c(3, 2, 1, 2, 3)
  funnel[funnel > 6] <- 6
  slope <- t(matrix(6, 5, 5) + c(1, 2, 3, 4, 5)) + c(2, 1, 0, 1, 2) - 2
  demm <- cbind(funnel, slope)
  dem <- terra::rast(demm, crs = "EPSG:25833")
  # Check improper input handling
  expect_error(flow_accumulation(dem))
  # Check proper input handling
  fd <- FLOWobj(dem)
  expect_silent(flow_accumulation(fd))
  # Check handling of missing values
  demm[1, 1] <- NA
  demm[2, 6] <- NA
  demm[3, 3] <- NA
  demm[3, 10] <- NA
  dem <- terra::rast(demm, crs = "EPSG:25833")
  expect_no_error(fd <- FLOWobj(dem))
  # Check known outputs
  fa <- flow_accumulation(fd)
  expect_true(inherits(fa, "GRIDobj"))
  expect_equal(
    c(terra::values(fa$raster)),
    c(1, 1, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 5, 3, 1, 1, 3, 2, 2, 1, 3, 4, 1, 28,
      24, 19, 13, 8, 3, 1, 1, 2, 9, 3, 1, 2, 2, 2, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1,
      1, 1)
  )
})