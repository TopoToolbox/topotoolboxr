test_that("test-gradient8 loads reference data and creates own data to compare outputs to known values.", {
  # Plain test
  DEM <- terra::rast(system.file("ex/elev.tif",package="terra"))
  DEM <- terra::project(DEM,"EPSG:32632",res=90.0)
  expect_equal(sum(values(gradient8(DEM)),na.rm=T), 11937.3066)
  # Transposition
  DEMm <- t(matrix(1, nrow=5, ncol=5)*c(5,2,1,3,4))*c(4,3,2,1,3)
  DEM <- terra::rast(DEMm, crs="EPSG:25833")
  G <- gradient8(DEM)
  expect_identical(values(G),values(t(gradient8(t(DEM)))))
  # Unit conversion
  expect_identical(round(values(G),8),round(tan(values(gradient8(DEM,'degree')) * pi/180),8))
  expect_identical(round(values(G),8),round(tan(asin(values(gradient8(DEM,'sine')))),8))
  # Missing data
  DEM <- DEMm
  DEM[2,2] <- NaN
  DEM <- terra::rast(DEM, crs="EPSG:25833")
  G <- gradient8(DEM)
  expect_true(is.na(sum(values(G))))
  expect_equal(sum(values(G),na.rm=T), 103.091884)
  # Output
  expect_no_message(gradient8(DEM))
  expect_no_error(gradient8(DEM))
  expect_visible(gradient8(DEM))
})
