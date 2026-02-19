test_that("Tests gradient8 against known reference outputs.", {
  # Plain test
  dem <- terra::rast(system.file("ex/elev.tif", package = "terra"))
  dem <- terra::project(dem, "EPSG:32632", res = 90.0)
  expect_silent(g <- gradient8(GRIDobj(dem)))
  expect_true(inherits(g, "GRIDobj"))
  expect_silent(g <- gradient8(dem))
  expect_true(inherits(g, "SpatRaster"))
  expect_equal(sum(terra::values(g), na.rm = TRUE), 11937.3066)

  # Transposition
  demm <- t(matrix(1, nrow = 5, ncol = 5) * c(5, 2, 1, 3, 4)) * c(4, 3, 2, 1, 3)
  dem <- terra::rast(demm, crs = "EPSG:25833")
  g <- gradient8(dem)
  expect_identical(
    terra::values(g),
    terra::values(terra::t(gradient8(terra::t(dem))))
  )

  # Unit conversion
  expect_identical(
    round(terra::values(g), 8),
    round(tan(terra::values(gradient8(dem, "degree")) * pi / 180), 8)
  )
  expect_identical(
    round(terra::values(g), 8),
    round(tan(asin(terra::values(gradient8(dem, "sine")))), 8)
  )

  # Missing data
  dem <- demm
  dem[2, 2] <- NaN
  dem <- terra::rast(dem, crs = "EPSG:25833")
  g <- gradient8(dem)
  expect_true(is.na(sum(terra::values(g))))
  expect_equal(sum(terra::values(g), na.rm = TRUE), 103.091884)

  # Output
  expect_no_message(gradient8(dem))
  expect_no_error(gradient8(dem))
  expect_visible(gradient8(dem))
})