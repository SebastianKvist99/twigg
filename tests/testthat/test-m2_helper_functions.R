test_that("total socre computer and column appender works", {
  expect_equal(compute_and_include_total_score(
    data.frame("id" = c(1,2,3,4), "item1" = c(1,1,1,1), "item2" = c(2,2,2,2)),
    c("item1", "item2")
  ),
  data.frame("item1" = c(1,1,1,1), "item2" = c(2,2,2,2),
  "total_score" = c(3,3,3,3)))
})


