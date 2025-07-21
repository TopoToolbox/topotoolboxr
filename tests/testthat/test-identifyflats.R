test_that("test-identifyflats.R creates reference DEM and compares to known classifications", {
  # Check Nan at border
  mDEM <- matrix(1, nrow = 5, ncol = 5)
  mDEM[1,1] <- NaN
  DEM <- terra::rast(mDEM, crs="EPSG:25833")
  FLATS <- identifyflats(DEM)
  expect_equal(c(sum(values(identifyflats(dem = DEM)) == 0),
                 sum(values(identifyflats(dem = DEM)) == 2),
                 sum(values(identifyflats(dem = DEM)) == 5)),
               c(1,16,8))
  # Check Nan elsewhere
  mDEM <- matrix(1, nrow = 5, ncol = 5)
  mDEM[2,2] <- NaN
  DEM <- terra::rast(mDEM, crs="EPSG:25833")
  FLATS <- identifyflats(DEM)
  expect_equal(c(sum(values(identifyflats(dem = DEM)) == 0),
                 sum(values(identifyflats(dem = DEM)) == 2),
                 sum(values(identifyflats(dem = DEM)) == 5)),
               c(4,16,5))
  # Check Nan-free 
  mDEM <- matrix(1, nrow = 5, ncol = 5) * c(3,2,1,2,3)
  DEM <- terra::rast(mDEM, crs="EPSG:25833")
  expect_equal(c(sum(values(identifyflats(dem = DEM)) == 0),
                 sum(values(identifyflats(dem = DEM)) == 1),
                 sum(values(identifyflats(dem = DEM)) == 2),
                 sum(values(identifyflats(dem = DEM)) == 5)),
               c(20,1,2,2))
  expect_no_message(identifyflats(DEM))
  expect_no_error(identifyflats(DEM))
})