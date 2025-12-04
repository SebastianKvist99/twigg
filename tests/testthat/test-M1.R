test_that("M1 function works", {
  expect_equal(M1(toy_spadi_pain, paste0("pain", 1:5)), TRUE)
})
