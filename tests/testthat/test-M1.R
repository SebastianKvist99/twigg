test_that("M1 function works", {
  expect_equal(M1(toy_spadi_pain, paste0("pain", 1:5)), TRUE)
})

test_that("M1 test on data which is negativly correlated", {
  df <- data.frame(
    x1 = 1:10,
    x2 = 10:1, # this column is negatively correlated
    x3 = 1:10
  )
  expect_false(M1(df, c("x1", "x2", "x3")))
})


test_that("M1 test on data with zero correlated (edge case)", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100) # should have corr 0.
  )
  expect_false(M1(df, c("x1", "x2")))
})

test_that("M1 test on data with perfect correlation, i.e. corr =1 (edge case)", {
  df <- data.frame(
    x1 = 1:10,
    x2 = 1:10,
    x3 = 1:10
  )
  expect_true(M1(df, c("x1", "x2", "x3")))
})

# test_that("M1 handles missing data explicitly", {
#
#   df <- toy_spadi_pain
#   df$pain1[1:10] <- NA
#
#   expect_error(
#     M1(df, paste0("pain", 1:5))
#   )
# })

test_that("M1 errors when y contains invalid column names", {

  expect_error(
    M1(toy_spadi_pain, c("pain1", "pain999"))
  )
})

test_that("M1 errors with non-numeric items", {

  df <- data.frame(
    x1 = 1:10,
    x2 = letters[1:10]
  )

  expect_error(
    M1(df, c("x1", "x2"))
  )
})





