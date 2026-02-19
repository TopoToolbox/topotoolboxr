test_that("Tests identifyflats on reference DEM.", {
  # Check Nan at border
  demm <- matrix(1, nrow = 5, ncol = 5)
  demm[1, 1] <- NaN
  dem <- terra::rast(demm, crs = "EPSG:25833")
  expect_silent(flats <- identifyflats(GRIDobj(dem)))
  expect_true(inherits(flats, "GRIDobj"))
  expect_silent(flats <- identifyflats(dem))
  expect_true(inherits(flats, "SpatRaster"))
  expect_equal(c(sum(terra::values(flats) == 0),
                 sum(terra::values(flats) == 2),
                 sum(terra::values(flats) == 5)),
               c(1, 16, 8))
  # Check Nan elsewhere
  demm <- matrix(1, nrow = 5, ncol = 5)
  demm[2, 2] <- NaN
  dem <- terra::rast(demm, crs = "EPSG:25833")
  flats <- identifyflats(dem)
  expect_equal(c(sum(terra::values(flats) == 0),
                 sum(terra::values(flats) == 2),
                 sum(terra::values(flats) == 5)),
               c(4, 16, 5))
  # Check Nan-free
  demm <- matrix(1, nrow = 5, ncol = 5) * c(3,2,1,2,3)
  dem <- terra::rast(demm, crs = "EPSG:25833")
  flats <- identifyflats(dem)
  expect_equal(c(sum(terra::values(flats) == 0),
                 sum(terra::values(flats) == 1),
                 sum(terra::values(flats) == 2),
                 sum(terra::values(flats) == 5)),
               c(20, 1, 2, 2))
})