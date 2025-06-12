test_that("test-gradient8 loads the elev.tif from terra and produces sum of all gradients", {
  #load_rast<-terra::rast(system.file("ex/elev.tif",package="terra"))
  #load_rast<-terra::project(load_rast,"epsg:32632",res=90.0)
  DEMm <- t(matrix(1, nrow=5, ncol=5)*c(5,2,1,3,4))*c(4,3,2,1,3)
  DEM <- terra::rast(DEMm, crs="EPSG:25833")
  expect_equal(sum(values(gradient8(DEM))), 107.3137090)
  expect_no_message(identifyflats(DEM))
  expect_no_error(identifyflats(DEM))
})
