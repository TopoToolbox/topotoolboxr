test_that("test-fillsinks.R loads reference data and tests missing data handling, matching results for both methods and known values.",{
  # Tests on functioning
  DEM <- terra::rast(system.file("ex/elev.tif",package="terra"))
  DEM <- terra::project(DEM,"EPSG:32632",res=90.0)
  expect_no_error(fillsinks(DEM))
  expect_equal(terra::values(fillsinks(DEM, hybrid=TRUE)),
               terra::values(fillsinks(DEM, hybrid=FALSE)))
  dem <- terra::values(fillsinks(DEM))
  expect_equal(c(min(dem, na.rm=T), mean(dem, na.rm=T), max(dem, na.rm=T)),
               c(141.0609, 349.0915, 546.6021),
               tolerance = 1e-6)
})