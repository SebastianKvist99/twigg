test_that("M2 pass for well behaved data", {
  df <- toy_spadi_pain
  items <- paste0("pain", 1:5)
  res <- M2(df, items)

  expect_true(res$status)
})

test_that("M2 stops and throws appropriate error when the input is invalid", {
  df <- toy_spadi_pain
  wrong_items <- paste0("disability", 1:5)

  bad_df <- df
  bad_df$pain1 <- letters[1:nrow(df)]
  right_items <- paste0("pain", 1:5)

  expect_error(M2(df, wrong_items),
               "One or more of the given item names are not in the data frame")
  expect_error(M2(bad_df, right_items),
               "One or more items are not numerical")
})


test_that("M2 fails when we reverse an item in an otherwise well behaved data set",{
  bad_df <- toy_spadi_pain
  bad_df$pain3 <- -bad_df$pain3
  items <- paste0("pain", 1:5)

  res <- M2(bad_df, items)

  expect_false(res$status)
})

test_that("M2 is invariant to item order", {
  items_right_order <- paste0("pain", 1:5)
  items_reverse_order <- rev(items_right_order)

  res1 <- M2(toy_spadi_pain, items_right_order)
  res2 <- M2(toy_spadi_pain, items_reverse_order)

  expect_equal(res1$status, res2$status)
})


test_that("M2 returns expected structure", {
  df <- toy_spadi_pain
  items <- paste0("pain", 1:5)
  res <- M2(df, items)


  expect_named(res, c("associations_measure", "associations", "status"))
  expect_type(res$status, "logical")
})

test_that("M2 should stop if invalid method is passed", {
  df <- toy_spadi_pain
  items <- paste0("pain", 1:5)

  expect_error(M2(df, items, method = "invalid"),
               "please input a valid method of associations measure")
})



















