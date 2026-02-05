test_that("Early stopping occurs when too few complete cases", {
  # Create dummy df
  df <- base::data.frame(
    "id" <- c(seq(1,15,1)),
    "sex" <- c(as.factor(sample(c("f","m"), size = 15, replace = TRUE))),
    "item1" <- c(rep(NA, 7), sample(1:10, size = 15-7, replace = TRUE)),
    "item2" <- c(rep(sample(1:10, size = 15, replace = TRUE))),
    "item3" <- c(rep(sample(1:10, size = 15, replace = TRUE))),
    "item4" <- c(rep(sample(1:10, size = 15, replace = TRUE)))
  )
  expect_error(
    complete_cases(df, 10),
    "Too few observations in data set to perform meaningful screening"
  )
})

test_that("No early stopping occurs when sufficient amount of few complete cases", {
  # Create dummy df
  df <- base::data.frame(
    "id" <- c(seq(1,15,1)),
    "sex" <- c(as.factor(sample(c("f","m"), size = 15, replace = TRUE))),
    "item1" <- c(rep(NA, 3), sample(1:10, size = 15-3, replace = TRUE)),
    "item2" <- c(rep(sample(1:10, size = 15, replace = TRUE))),
    "item3" <- c(rep(sample(1:10, size = 15, replace = TRUE))),
    "item4" <- c(rep(sample(1:10, size = 15, replace = TRUE)))
  )
  expect_silent(
    complete_cases(df, 10)
  )
})


test_that("Error message is informative and stable", {
  df <- data.frame(
    x = c(1, NA, NA, NA, NA, NA, NA, 2, 3),
    y = c(1:9)
  )
  expect_error(
    complete_cases(df, 10),
    regexp = "Too few observations"
  )
})


test_that("Test edge case with exactly 10 observations", {
  df <- data.frame(
    x = c(1:10),
    y = c(1:10)
  )
  expect_silent(
    complete_cases(df, 10)
  )
})

test_that("complete_cases returns data frame when successful", {

  df <- data.frame(
    x = 1:15,
    y = 1:15
  )

  out <- complete_cases(df, 10)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 15)
  expect_false(anyNA(out))
})


test_that("Test behaviour with all missing data", {
  df <- data.frame(
    x = rep(NA,15),
    y = rep(NA,15)
  )
  expect_error(
    complete_cases(df, 10),
    regexp = "Too few observations"
  )
})

test_that("Non-data-frame input errors", {
  expect_error(
    complete_cases(1:20, 10)
  )
})

# sub-test of first test, but seems nice to know that the function does
# the intended
test_that("Partially missing rows are not counted as complete", {
  df <- data.frame(
    x = c(1:9, 10),
    y = c(1:9, NA)
  )
  expect_error(
    complete_cases(df, 10)
  )
})

##############################################################################
##############################################################################
################ Simple input validation function checks tests ###############
test_that("Are items in df function works well", {
  df <- toy_spadi_pain
  wrong_items <- paste0("pain", 1:6)
  right_items <- paste0("pain", 1:5)

  expect_error(are_items_in_df(df, wrong_items),
               "One or more of the given item names are not in the data frame")
  expect_silent(are_items_in_df(df, right_items))
})

test_that("Are covariates in df function works well", {
  df <- toy_spadi_pain
  wrong_covs <- c("height", "weight")
  right_covs <- c("age", "sex")

  expect_error(are_covaraites_in_df(df, wrong_covs),
               "One or more of the given covariate names are not in the data frame")
  expect_silent(are_covaraites_in_df(df, right_covs))
})

test_that("Are items numerical function works well", {
  bad_df <- data.frame("item1" = 1:10,
                   "item2" = letters[1:10],
                   "item3" = factor(sample(c("m", "f"), 10, replace = TRUE)))
  good_df <- data.frame("item1" = 1:10,
                        "item2" = 10:19,
                        "item3" = 5:14)
  items <- paste0("item", 1:3)

  expect_error(are_items_numeric(bad_df, items),
               "One or more items are not numerical")
  expect_silent(are_items_numeric(good_df, items))
})





