test_that("Tests flow_routing_d8_edgelist on a synthetic DEM.", {
  # Create reference features for synthetic DEMs
  funnel <- t(matrix(1, 5, 5) * c(3, 2, 1, 2, 3)) * c(3, 2, 1, 2, 3)
  funnel[funnel > 6] <- 6
  slope <- t(matrix(6, 5, 5) + c(1, 2, 3, 4, 5)) + c(2, 1, 0, 1, 2) - 2
  demm <- cbind(funnel, slope)
  # Check improper input handling
  expect_error(flow_routing_d8_edgelist(demm))
  # Check proper input handling
  dem <- terra::rast(demm, crs = "EPSG:25833")
  expect_silent(sou_tar <- flow_routing_d8_edgelist(GRIDobj(dem)))
  expect_silent(sou_tar <- flow_routing_d8_edgelist(dem))
  expect_equal(sou_tar$source,
               FLOWobj(dem)$source)
  expect_equal(sou_tar$target,
               FLOWobj(dem)$target)
  # Checking missing data handling
  demm[1, 1] <- NA
  demm[2, 6] <- NA
  demm[3, 3] <- NA
  demm[3, 10] <- NA
  dem <- terra::rast(demm, crs = "EPSG:25833")
  expect_silent(sou_tar <- flow_routing_d8_edgelist(dem))
  expect_equal(sou_tar$source,
               FLOWobj(dem)$source)
  expect_equal(sou_tar$target,
               FLOWobj(dem)$target)
})