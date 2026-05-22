test_that("character covariates are accepted and coerced", {

  items <- paste0("pain", 1:5)

  out <- M3(
    dataset = toy_spadi_pain,
    items = items,
    covariates = c("sex","age")#, include_pvalues = FALSE
  )

  expect_true(is.data.frame(out$correlations))
})



test_that("M3 runs end-to-end and returns expected output", {

  items <- paste0("pain", 1:5)
  covariates <- c("sex", "age")

  out <- M3(
    dataset = toy_spadi_pain,
    items = items,
    covariates = covariates,
    corr_method = "pearson"#, include_pvalues = TRUE
  )

  expect_s3_class(out$correlations, "data.frame")
  expect_equal(
    nrow(out$correlations),
    length(covariates) * (length(items) + 1)
  )

  expect_true(all(c("covariate", "target_type", "target_name",
                    "correlation", "method") %in%
                    names(out$correlations)))
})

test_that("M3 complete-case filtering is covariate-specific", {

  dataset <- data.frame(
    item1 = c(0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    item2 = c(0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0),
    exo1 = c(1, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2),
    exo2 = c(NA, NA, 1, 1, 2, 2, 1, 2, 1, 2, 1, 2)
  )

  out_both <- M3(dataset, c("item1", "item2"), c("exo1", "exo2"),
                 corr_method = "gamma")
  out_one <- M3(dataset, c("item1", "item2"), "exo1",
                corr_method = "gamma")

  both_exo1 <- out_both$correlations[out_both$correlations$covariate == "exo1",
                                     , drop = FALSE]
  row.names(both_exo1) <- NULL
  row.names(out_one$correlations) <- NULL

  expect_equal(both_exo1, out_one$correlations)
})

test_that("M3 ignores missingness in unrelated covariates", {

  dataset <- toy_spadi_pain
  items <- paste0("pain", 1:5)
  dataset$unrelated <- NA_real_

  out_with_unrelated <- M3(dataset, items, "sex")
  out_without_unrelated <- M3(dataset[names(dataset) != "unrelated"],
                              items, "sex")

  expect_equal(out_with_unrelated, out_without_unrelated)
})
