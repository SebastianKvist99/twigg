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

