test_that("M1 function works", {

  expect_equal(M1(cars, c("dist")), TRUE)
  expect_equal(M1(cars, c("speed")), TRUE)
})
