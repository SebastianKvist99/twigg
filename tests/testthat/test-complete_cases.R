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
    "Too few observations in data set ot perform meaningfull screening"
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
  # df_na_omit <- stats::na.omit(df)
  #
  # expect_equal(
  #   complete_cases(df),
  #   df_na_omit
  # )
  expect_silent(
    complete_cases(df)
  )
})
