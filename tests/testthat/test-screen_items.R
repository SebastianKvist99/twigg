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
  expect_equal(res$failed_steps, character(0))
  expect_null(res$failed_step)

  expect_true("M1" %in% names(res))
  expect_true("M2" %in% names(res))
  expect_true("M3" %in% names(res))
})

## ---------------------------------------- ##
test_that("screen_items records and prints M3 failures", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    exo = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1)
  )

  local_mocked_bindings(
    M1 = function(dataset, items, method) {
      list(status = TRUE)
    },
    M2 = function(dataset, items, method) {
      list(status = TRUE)
    },
    M3 = function(dataset, items, covariates, corr_method) {
      list(status = FALSE)
    },
    .package = "twigg"
  )

  res <- screen_items(
    dataset = dataset,
    items = c("item1", "item2"),
    covariates = "exo"
  )

  expect_false(res$passed)
  expect_equal(res$failed_steps, "M3")
  expect_equal(res$failed_step, "M3")
  expect_output(print(res), "Failed steps: M3")
})

## ---------------------------------------- ##
test_that("screen_items records multiple failed steps", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    exo = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1)
  )

  local_mocked_bindings(
    M1 = function(dataset, items, method) {
      list(status = FALSE)
    },
    M2 = function(dataset, items, method) {
      list(status = TRUE)
    },
    M3 = function(dataset, items, covariates, corr_method) {
      list(status = FALSE)
    },
    .package = "twigg"
  )

  res <- screen_items(
    dataset = dataset,
    items = c("item1", "item2"),
    covariates = "exo"
  )

  expect_false(res$passed)
  expect_equal(res$failed_steps, c("M1", "M3"))
  expect_output(print(res), "Failed steps: M1, M3")
})

## ---------------------------------------- ##
test_that("screen_items does not globally complete-case all columns", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    exo = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1),
    unrelated = NA_real_
  )

  n_seen <- integer(0)

  local_mocked_bindings(
    M1 = function(dataset, items, method) {
      n_seen <<- c(n_seen, nrow(dataset))
      list(status = TRUE)
    },
    M2 = function(dataset, items, method) {
      n_seen <<- c(n_seen, nrow(dataset))
      list(status = TRUE)
    },
    M3 = function(dataset, items, covariates, corr_method) {
      n_seen <<- c(n_seen, nrow(dataset))
      list(status = TRUE)
    },
    .package = "twigg"
  )

  res <- screen_items(
    dataset = dataset,
    items = c("item1", "item2"),
    covariates = "exo"
  )

  expect_true(res$passed)
  expect_equal(n_seen, c(10L, 10L, 10L))
})

## ---------------------------------------- ##
# Drop this test as the early stop is no longer desired. We want to run the full
# screening even if we fail in the first step, since we want to see if we also fail
# any other steps
# test_that("screen_items stops early when M1 fails", {
#
#   bad_data <- toy_spadi_pain
#   # induce negative correlation to make M1 fail
#   bad_data$pain1 <- rev(bad_data$pain1)
#
#   items <- paste0("pain", 1:5)
#
#   res <- screen_items(
#     dataset = bad_data,
#     items = items,
#     covariates = c("age", "sex")
#   )
#
#   expect_false(res$passed)
#   expect_equal(res$failed_step, "M1")
#   expect_true("M1" %in% res$failed_step)
# })
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
