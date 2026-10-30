testthat::skip_on_cran()

testthat::test_that(
  "remap returns a valid remap S3 object with expected structure", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")
    utsnz <- utsnow[utsnow$WESD > 0, ]

    mod <- remap::remap(
      utsnz, 
      utws, 
      region_id = "HUC2",
      model_function = lm, 
      formula = log(WESD) ~ ELEVATION,
      buffer = 20, 
      min_n = 10
    )

    testthat::expect_s3_class(mod, "remap")
    testthat::expect_type(mod, "list")
    testthat::expect_named(mod, c("models", "regions", "call"))
    testthat::expect_type(mod$models, "list")
    testthat::expect_equal(length(mod$models), length(unique(utws$HUC2)))
  }
)

testthat::test_that(
  "remap correctly forwards ... arguments to model_function", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")
    utsnz <- utsnow[utsnow$WESD > 0, ]

    mod <- remap::remap(
      utsnz, 
      utws, 
      region_id = "HUC2",
      model_function = lm, 
      formula = log(WESD) ~ ELEVATION,
      buffer = 20, 
      min_n = 10
    )

    # Verify each model contains the expected coefficient
    has_elev <- sapply(
      mod$models, 
      \(m) any(grepl("ELEVATION", names(m$coefficients)))
    )

    testthat::expect_true(all(has_elev))
  }
)

testthat::test_that(
  "remap throws errors for invalid parameters",
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")
    utsnz <- utsnow[utsnow$WESD > 0, ]

    testthat::expect_error(
      remap::remap(
        utsnz, 
        utws, 
        region_id = "HUC2", 
        model_function = lm,
        formula = log(WESD) ~ ELEVATION, 
        buffer = 20, 
        min_n = 0
      )
    )
    testthat::expect_error(
      remap::remap(
        utsnz, 
        utws, 
        region_id = "HUC2", 
        model_function = lm,
        formula = log(WESD) ~ ELEVATION,
        buffer = -5,
        min_n = 10)
    )
  }
)

testthat::test_that(
  "remap errors when distances lack units class", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")

    bad_dists <- units::drop_units
      (remap::redist(utsnow[1:10,], utws, region_id="HUC2")
    )

    testthat::expect_error(
      remap::remap(
        utsnow[1:10,], 
        utws, 
        region_id = "HUC2", 
        model_function = lm,
        formula = log(WESD)~ELEVATION, 
        buffer = 20, 
        distances = bad_dists)
    )
  }
)

testthat::test_that(
  "predict.remap returns predictions of correct length and type", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")
    utsnz <- utsnow[utsnow$WESD > 0, ]

    mod <- remap::remap(
      utsnz, 
      utws, 
      region_id = "HUC2",
      model_function = lm, 
      formula = log(WESD) ~ ELEVATION,
      buffer = 20, 
      min_n = 10
    )

    preds <- predict(mod, utsnz, smooth = 10)

    testthat::expect_equal(length(preds), nrow(utsnz))
    testthat::expect_type(preds, "double")
  }
)

testthat::test_that(
  "predict.remap smooth parameter changes predictions", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")
    utsnz <- utsnow[utsnow$WESD > 0, ]

    mod <- remap::remap(
      utsnz, 
      utws, 
      region_id = "HUC2",
      model_function = lm, 
      formula = log(WESD) ~ ELEVATION,
      buffer = 20, 
      min_n = 10
    )

    preds_smooth <- predict(mod, utsnz, smooth = 15)
    preds_sharp  <- predict(mod, utsnz, smooth = 0)

    # Smoothing should alter at least some predictions
    testthat::expect_false(all(preds_smooth == preds_sharp))
  }
)

testthat::test_that(
  "pre-computed distances yield consistent results vs internal calculation", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")
    utsnz <- utsnow[utsnow$WESD > 0, ]

    dists <- remap::redist(utsnz, utws, region_id = "HUC2")

    mod1 <- remap::remap(
      utsnz, 
      utws, 
      region_id = "HUC2", 
      model_function = lm,
      formula = log(WESD) ~ ELEVATION, 
      buffer = 20, 
      min_n = 10
    )
    mod2 <- remap::remap(
      utsnz, 
      utws, 
      region_id = "HUC2", 
      model_function = lm,
      formula = log(WESD) ~ ELEVATION, 
      buffer = 20, 
      min_n = 10,
      distances = dists
    )

    preds1 <- predict(mod1, utsnz, smooth = 10)
    preds2 <- predict(mod2, utsnz, smooth = 10, distances = dists)

    testthat::expect_equal(preds1, preds2, tolerance = 1e-6)
  }
)

testthat::test_that(
  "predict.se calculates weighted standard errors correctly", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")
    utsnz <- utsnow[utsnow$WESD > 0, ]
    smth <- 20

    # Make lm model wrapper that returns se values when predict is called
    lm_wrapper <- function(x, ...) {
      out = list(lm(x, ...))
      class(out) <- "lm_wrapper"
      return(out)
    }
    predict.lm_wrapper <- function(x, ...) {
      predict(x[[1]], se.fit = TRUE, ...)$se.fit
    }
    registerS3method("predict", "lm_wrapper", predict.lm_wrapper)
    
    mod <- remap::remap(
      utsnz, 
      utws, 
      region_id = "HUC2", 
      model_function = lm_wrapper,
      formula = log(WESD) ~ ELEVATION, 
      buffer = 50, 
      min_n=10
    )
    se0_preds <- predict(mod, utsnz, smooth = 0, se = TRUE)
    se_preds <- predict(mod, utsnz, smooth = smth, se = TRUE)

    # Distances for utsnz[20, ]: 
    # d = c("14" = 13.13397, "15" = 503.9255, "16" = 3.721765, "17" = 0.00000)
    # Prediction weights for utsnz[20, ]: 
    # w = (pmax((smth - d) / smth, 0))^2
    # 0.1178559 0.0000000 0.6624523 1.0000000 
    # Model standard errors for utsnz[20, ]: 
    # s = sapply(mod$models, \(model){predict(model, utsnz[20, ])})
    # 0.10393026 0.31653854 0.06683692 0.14081281
    # Standard error upper bound: 
    # b = sqrt(sum((w * s) %*% t(w * s)) / (sum(w))^2)
    # 0.1108448

    testthat::expect_all_true(se_preds >= 0)
    testthat::expect_equal(se0_preds[20], 0.14081281, tolerance = 1e-5)
    testthat::expect_equal(se_preds[20], 0.1108448, tolerance = 1e-5)
  }
)

testthat::test_that(
  "S3 methods (print, summary, plot) execute without error", 
  {
    data(utsnow, package = "remap")
    data(utws, package = "remap")
    utsnz <- utsnow[utsnow$WESD > 0, ]

    mod <- remap::remap(
      data = utsnz, 
      regions = utws, 
      region_id = "HUC2",
      model_function = lm, 
      formula = log(WESD) ~ ELEVATION,
      buffer = 20, 
      min_n = 10
    )

    testthat::expect_no_error(print(mod))
    testthat::expect_no_error(summary(mod))
    testthat::expect_no_error(plot(mod))
  }
)
