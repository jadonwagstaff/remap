testthat::skip_on_cran()

testthat::test_that(
  "redist returns a distance matrix with correct dimensions and units", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")

    dists <- remap::redist(
      utsnow, 
      utws, 
      region_id = "HUC2", 
      cores = 1
    )

    testthat::expect_true("units" %in% class(dists))
    testthat::expect_equal(
      dim(dists), 
      c(nrow(utsnow), length(unique(utws$HUC2)))
    )
    testthat::expect_true(all(units::drop_units(dists) >= 0))
  }
)

testthat::test_that(
  "redist correctly names columns based on region_id", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")

    dists <- remap::redist(utsnow, utws, region_id = "HUC2")
    expected_names <- as.character(unique(utws$HUC2))

    testthat::expect_equal(colnames(dists), expected_names)
  }
)

testthat::test_that(
  "redist falls back to row numbers when region_id is missing", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")

    dists <- remap::redist(utsnow, utws)

    testthat::expect_equal(ncol(dists), nrow(utws))
    testthat::expect_equal(colnames(dists), as.character(1:nrow(utws)))
  }
)

testthat::test_that(
  "redist throws appropriate errors for invalid inputs", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")

    # data must be sf points
    testthat::expect_error(
      redist(as.data.frame(utsnow), utws, region_id = "HUC2")
    )

    # regions must be sf polygons
    testthat::expect_error(
      redist(utsnow, as.data.frame(utws), region_id = "HUC2")
    )

    # CRS must match
    utws_3857 <- sf::st_transform(utws, 3857)
    testthat::expect_error(
      redist(utsnow, utws_3857, region_id = "HUC2")
    )

    # cores must be >= 1
    testthat::expect_error(
      redist(utsnow, utws, region_id = "HUC2", cores = 0)
    )

    # region_id must exist
    testthat::expect_error(
      redist(utsnow, utws, region_id = "FAKE_COL")
    )
  }
)

testthat::test_that(
  "process_regions unions duplicate region_ids", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")

    dup_ws <- rbind(utws, utws[1,]) # Duplicate first row
    dup_ws$HUC2[5] <- "14" # Same ID as row 1
    dists <- remap::redist(utsnow[1,], dup_ws, region_id="HUC2")

    testthat::expect_equal(ncol(dists), 4) # Should collapse to 4 unique IDs
  }
)

testthat::test_that(
  "redist max_dist parameter preserves structure while limiting computation", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")

    dists_full <- remap::redist(utsnow, utws, region_id = "HUC2")
    dists_max  <- remap::redist(utsnow, utws, region_id = "HUC2", max_dist = 50)
    dists_mock_max <- dists_full
    dists_mock_max[is.na(dists_max)] <- NA_real_

    testthat::expect_equal(dim(dists_full), dim(dists_max))
    testthat::expect_equal(colnames(dists_full), colnames(dists_max))

    # max_dist should introduce NAs or larger values where points are far away,
    # but structure remains intact
    testthat::expect_true("units" %in% class(dists_max))
    testthat::expect_equal(
      units::deparse_unit(dists_max), 
      units::deparse_unit(dists_full)
    )
    testthat::expect_gt(
      min(dists_full[is.na(dists_max)]), 
      units::as_units(50, "km")
    )
    testthat::expect_equal(dists_max, dists_mock_max)
  }
)

testthat::test_that(
  "redist results are consistent across single and multi-core runs", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")

    dists_1 <- remap::redist(utsnow, utws, region_id = "HUC2", cores = 1)

    # Only run multi-core if >1 core is available (CI-friendly)
    if (parallel::detectCores() > 1) {
      dists_2 <- remap::redist(utsnow, utws, region_id = "HUC2", cores = 2)
      testthat::expect_equal(dists_1, dists_2, tolerance = 1e-5)
    }
  }
)

testthat::test_that(
  "redist correctly calculates distances for specific Utah weather stations", 
  {
    # Load package datasets
    data(utsnow, package = "remap")
    data(utws, package = "remap")

    # Filter to only the two stations of interest
    target_ids <- c("US1UTCH0022", "USC00429111")
    test_pts <- utsnow[utsnow$ID %in% target_ids, ]

    # Safely identify row indices regardless of input ordering
    idx_logan <- which(test_pts$ID == "US1UTCH0022")
    idx_vernal <- which(test_pts$ID == "USC00429111")

    # Compute distances
    dist_mat <- remap::redist(
      data = test_pts,
      regions = utws,
      region_id = HUC2,
      cores = 1
    )

    # Ensure expected HUC2 columns exist
    testthat::expect_true("16" %in% colnames(dist_mat))
    testthat::expect_true("14" %in% colnames(dist_mat))

    # 1. US1UTCH0022 should be inside HUC2 16 (distance == 0)
    testthat::expect_equal(
      dist_mat[idx_logan, "16"], 
      c("16" = units::as_units(0, "km"))
    )

    # 2. USC00429111 should be inside HUC2 14 (distance == 0)
    testthat::expect_equal(
      dist_mat[idx_vernal, "14"], 
      c("14" = units::as_units(0, "km"))
    )

    # 3. Distance from US1UTCH0022 to HUC2 14 should be 77.21342
    testthat::expect_equal(
      dist_mat[idx_logan, "14"], 
      c("14" = units::as_units(77.21342, "km")), 
      tolerance = 1e-3
    )

    # 4. Distance from USC00429111 to HUC2 16 should be 102.4582
    testthat::expect_equal(
      dist_mat[idx_vernal, "16"], 
      c("16" = units::as_units(102.4582, "km")), 
      tolerance = 1e-3
    )
  }
)