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
    complete_cases(df),
    "Too few observations in data set ot perform meaningful screening"
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
    complete_cases(df)
  )
})


test_that("Error message is informative and stable", {
  df <- data.frame(
    x = c(1, NA, NA, NA, NA, NA, NA, 2, 3),
    y = c(1:9)
  )
  expect_error(
    complete_cases(df),
    regexp = "Too few observations"
  )
})


test_that("Test edge case with exactly 10 observations", {
  df <- data.frame(
    x = c(1:10),
    y = c(1:10)
  )
  expect_silent(
    complete_cases(df)
  )
})

test_that("Function returns invisible NULL when the succesfull", {
  df <- data.frame(
    x = c(1:15),
    y = c(1:15)
  )
  expect_equal(complete_cases(df),
               invisible(NULL))
})


test_that("Test behaviour with all missing data", {
  df <- data.frame(
    x = rep(NA,15),
    y = rep(NA,15)
  )
  expect_error(
    complete_cases(df),
    regexp = "Too few observations"
  )
})

test_that("Non-data-frame input errors", {
  expect_error(
    complete_cases(1:20)
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
    complete_cases(df)
  )
})
