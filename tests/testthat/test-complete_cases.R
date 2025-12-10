test_that("Early stopping due to no enough complete cases workds", {
  expect_error(
    complete_cases(
      data.frame("id" = seq(from = 1, to = 15, by = 1),
                 "sex" = as.factor(sample(c("M", "F"), size = length(seq(1,15,1)), replace = TRUE)),
                 "item1" = sample(c(1,2,3,4,5,6,7,8,NA, NA, NA, NA, NA, NA, NA),
                                  size = length(seq(1,15,1)), replace = FALSE),
                 "item2" = sample(c(1,2,3,4,5,6,7,8),
                                  size = length(seq(1,15,1)), replace = TRUE),
                 "item3" = sample(c(1,2,3,4,5,6,7,8),
                                  size = length(seq(1,15,1)), replace = TRUE),
                 "item4" = sample(c(1,2,3,4,5,6,7,8),
                                  size = length(seq(1,15,1)), replace = TRUE)
                 )),
      stop("Too few observations in data set ot perform meaningfull screening"))
  })



test_that("No early stop", {
  expect_equal(
    complete_cases(
      data.frame("id" = seq(from = 1, to = 15, by = 1),
                 "sex" = as.factor(sample(c("M", "F"), size = length(seq(1,15,1)), replace = TRUE)),
                 "item1" = sample(c(1,2,3,4,5,6,7,8,5, 5, 4, 3, 2, 1, 2),
                                  size = length(seq(1,15,1)), replace = FALSE),
                 "item2" = sample(c(1,2,3,4,5,6,7,8),
                                  size = length(seq(1,15,1)), replace = TRUE),
                 "item3" = sample(c(1,2,3,4,5,6,7,8),
                                  size = length(seq(1,15,1)), replace = TRUE),
                 "item4" = sample(c(1,2,3,4,5,6,7,8),
                                  size = length(seq(1,15,1)), replace = TRUE)
      )),
    )
})
