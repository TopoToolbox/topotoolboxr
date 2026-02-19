test_that("Tests flow_routing_d8_carve on a synthetic DEM.", {
  # Funnel shape test
  demm <- t(matrix(1, nrow = 5, ncol = 5) * c(3, 2, 1, 2, 3)) * c(3, 2, 1, 2, 3)
  demr <- terra::rast(demm, crs = "EPSG:25833")
  # GRIDobj handling
  expect_silent(sou_dir <- flow_routing_d8_carve(GRIDobj(demr)))
  expect_true(inherits(sou_dir$source, "GRIDobj"))
  expect_true(inherits(sou_dir$direction, "GRIDobj"))
  # SpatRaster handling
  expect_silent(sou_dir <- flow_routing_d8_carve(demr))
  expect_true(inherits(sou_dir$source, "SpatRaster"))
  expect_true(inherits(sou_dir$direction, "SpatRaster"))
  # Return check
  expect_equal(as.vector(table(terra::values(sou_dir$direction))),
               c(4, 6, 1, 4, 1, 4, 1, 3, 1))
  # Missing value handling
  demm[2, 4] <- NA
  dem <- terra::rast(demm, crs = "EPSG:25833")
  expect_silent(flow_routing_d8_carve(dem))
})
