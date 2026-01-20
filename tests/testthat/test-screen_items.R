test_that("screen_items passes all steps on toy SPADI data", {

  items <- paste0("pain", 1:5)
  covariates <- c("age", "sex")

  res <- screen_items(
    dataset = toy_spadi_pain,
    items = items,
    covariates = covariates
  )

  expect_s3_class(res, "item_screening")
  expect_true(res$passed)
  expect_null(res$failed_step)

  expect_true("M1" %in% names(res))# || !"M1" %in% names(res))
  expect_true("M2" %in% names(res))
  expect_true("M3" %in% names(res))
})

## ---------------------------------------- ##
test_that("screen_items stops early when M1 fails", {

  bad_data <- toy_spadi_pain
  # induce negative correlation to make M1 fail
  bad_data$pain1 <- rev(bad_data$pain1)

  items <- paste0("pain", 1:5)

  res <- screen_items(
    dataset = bad_data,
    items = items,
    covariates = c("age", "sex")
  )

  expect_false(res$passed)
  expect_equal(res$failed_step, "M1")
  expect_true("M1" %in% res$failed_step)
})
## ---------------------------------------- ##
# We should make a similar test as above but for M2 failure and M3 failure


# ## ---------------------------------------- ##
# test_that("screen_items fails on M3 when covariate shows negative association", {
#
#   bad_data <- toy_spadi_pain
#   bad_data$bad_cov <- -rowSums(bad_data[paste0("pain", 1:5)])
#
#   items <- paste0("pain", 1:5)
#
#   res <- screen_items(
#     dataset = bad_data,
#     items = items,
#     covariates = "bad_cov"
#   )
#
#   expect_false(res$passed)
#   expect_equal(res$failed_step, "M3")
# })

## ---------------------------------------- ##
test_that("screen_items skips M3 when requested", {

  items <- paste0("pain", 1:5)

  res <- screen_items(
    dataset = toy_spadi_pain,
    items = items,
    covariates = NULL,
    run_M3 = FALSE
  )

  expect_true(res$passed)
  expect_false("M3" %in% names(res))
})

## ---------------------------------------- ##
test_that("screen_items errors on invalid input", {

  expect_error(
    screen_items("not_a_df", items = "x"),
    "must be a data.frame"
  )

  expect_error(
    screen_items(toy_spadi_pain, items = 1:5),
    "must be a character vector"
  )
})

