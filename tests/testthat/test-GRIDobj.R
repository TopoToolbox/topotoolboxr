test_that("Checks if each use case creates a GRIDobj without errors and
          preserves properties.", {
            # Case 1: GRIDobj(NULL)
            expect_silent(
              GRIDobj(NULL)
            )
            expect_true(
              inherits(GRIDobj(NULL), "GRIDobj")
            )
            # Case 2: GRIDobj("path/to/file")
            demp <- system.file("extdata",
                                "srtm_bigtujunga30m_utm11.tif",
                                package = "topotoolboxr")
            expect_silent(
              dem <- GRIDobj(demp)
            )
            expect_true(
              inherits(dem, "GRIDobj")
            )
            # Case 3: GRIDobj(SpatRaster)
            funnel <- t(matrix(1, 5, 5) * c(3, 2, 1, 2, 3)) * c(3, 2, 1, 2, 3)
            funnel[funnel > 6] <- 6
            slope <- t(matrix(6, 5, 5) + c(1, 2, 3, 4, 5)) +
              c(2, 1, 0, 1, 2) - 2
            m <- cbind(funnel, slope)
            crs <- "EPSG:25833"
            demr <- terra::rast(m, crs = crs)
            expect_silent(
              GRIDobj(demr)
            )
            expect_true(
              inherits(GRIDobj(demr), "GRIDobj")
            )
            # Case 4.1: GRIDobj(Z)
            expect_silent(
              dem <- GRIDobj(m)
            )
            expect_true(
              inherits(GRIDobj(m), "GRIDobj")
            )
            expect_true(
              all(m == matrix(terra::values(dem$raster), 5, 10, byrow = TRUE))
            )
            # Case 4.2: GRIDobj(Z, cs)
            cs <- 2
            expect_silent(
              dem <- GRIDobj(m, cs)
            )
            expect_true(
              inherits(GRIDobj(m, cs), "GRIDobj")
            )
            expect_true(
              all(terra::res(dem$raster) == cs)
            )
            # Case 4.3: GRIDobj(Z, cs, crs)
            expect_silent(
              dem <- GRIDobj(m, cs, crs = crs)
            )
            expect_true(
              inherits(GRIDobj(m, cs, crs = crs), "GRIDobj")
            )
            expect_true(
              identical(terra::crs(demr), terra::crs(dem$raster))
            )
            # Case 4.4: GRIDobj(Z, x, y)
            x <- 2:(dim(m)[2] + 1)
            y <- 3:(dim(m)[1] + 2)
            expect_silent(
              dem <- GRIDobj(m, x, y)
            )
            expect_true(
              inherits(GRIDobj(m, x, y), "GRIDobj")
            )
            # Case 5.1: GRIDobj(TTobj)
            expect_silent(
              demg <- GRIDobj(dem)
            )
            expect_true(
              inherits(GRIDobj(dem), "GRIDobj")
            )
            expect_true(
              is.double(get_grid_data(demg$raster)$z)
            )
            # Case 5.2: GRIDobj(TTobj, 'class')
            expect_true(
              is.integer(get_grid_data(GRIDobj(dem, "integer")$raster)$z)
            )
            expect_true(
              is.logical(get_grid_data(GRIDobj(dem, "logical")$raster)$z)
            )
            # Case 5.3: GRIDobj(TTobj, Z)
            expect_error(
              GRIDobj(GRIDobj(demr), t(m))
            )
          })