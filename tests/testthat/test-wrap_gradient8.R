test_that("test-gradient8 loads the elev.tif from terra and produces sum of all gradients", {
  load_rast<-terra::rast(system.file("ex/elev.tif",package="terra"))
  load_rast<-terra::project(load_rast,"epsg:32632",res=90.0)
  expect_no_message(identifyflats(load_rast))
  expect_no_error(identifyflats(load_rast))
  expect_equal(sum(values(gradient8(load_rast)),na.rm=T), 62791.5986)
})
