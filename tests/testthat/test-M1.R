test_that("M1 function works with different methods", {
  res <- M1(toy_spadi_pain, paste0("pain", 1:5), method = "pearson")
  corr_should_be <- cor(toy_spadi_pain[paste0("pain", 1:5)])

  res1 <- M1(toy_spadi_pain, paste0("pain", 1:5), method = "spearman")
  corr_should_be1 <- cor(toy_spadi_pain[paste0("pain", 1:5)], method = "spearman")

  res2 <- M1(toy_spadi_pain, paste0("pain", 1:5), method = "kendall")
  corr_should_be2 <- cor(toy_spadi_pain[paste0("pain", 1:5)], method = "kendall")

  expect_equal(res$associations, corr_should_be)
  expect_equal(res1$associations, corr_should_be1)
  expect_equal(res2$associations, corr_should_be2)
  expect_true(res$status)
  expect_true(res1$status)
  expect_true(res2$status)

  res4 <- M1(toy_spadi_pain, paste0("pain", 1:5))

})

test_that("M1 test on data which is negativly correlated", {
  df <- data.frame(
    x1 = 1:10,
    x2 = 10:1, # this column is negatively correlated
    x3 = 1:10
  )
  res <- M1(df, paste0("x", 1:3))
  expect_false(res$status)
})


test_that("M1 test on data with zero correlated (edge case)", {
  x <- rep(0,10); x[1] <- 1; x[2] <- -1
  y <- rep(0,10); y[3] <- 1; y[4] <- -1
  df <- data.frame(x1 = x, x2 = y)
  res <- M1(df, paste0("x", 1:2))
  expect_false(res$status)
})

test_that("M1 test on data with perfect correlation, i.e. corr =1 (edge case)", {
  df <- data.frame(
    x1 = 1:10,
    x2 = 1:10,
    x3 = 1:10
  )
  res <- M1(df, paste0("x", 1:3))
  expect_true(res$status)
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

test_that("M1 errors when items argument contains invalid column names", {

  expect_error(
    M1(toy_spadi_pain, c("pain1", "pain999")),
    "One or more of the given item names are not in the data frame"
  )
})

test_that("M1 errors with non-numeric items", {

  df <- data.frame(
    x1 = 1:10,
    x2 = letters[1:10]
  )

  expect_error(
    M1(df, c("x1", "x2")), "One or more items are not numerical"
  )
})

test_that("M1 throws the correct error when an
          invalid method is passed",{
  expect_error(M1(toy_spadi_pain, paste0("pain", 1:5), method = "invalid"),
               "please input a valid method of associations measure")
})





