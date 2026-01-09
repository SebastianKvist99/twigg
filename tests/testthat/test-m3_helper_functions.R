test_that("check_covariate handles numeric and factor covariates", {

  num <- c(1, 2, 3)
  fac <- factor(c("A", "B", "A"))

  out_num <- check_covariate(num, "num")
  out_fac <- check_covariate(fac, "fac")

  expect_true(is.numeric(out_num))
  expect_true(is.numeric(out_fac))
  expect_equal(out_fac, c(0, 1, 0))
})

test_that("check_covariate errors on unsupported types", {

  bad <- list(1, 2, 3)

  expect_error(
    check_covariate(bad, "bad_covariate"),
    "must be numeric, factor, or character"
  )
})


#############################################
test_that("compute_total_score sums rows correctly", {

  items <- toy_spadi_pain[, paste0("pain", 1:5)]

  score <- compute_total_score(items)

  expect_length(score, nrow(items))
  expect_equal(score[1], sum(as.numeric(items[1, ])))
})

#############################################
test_that("cor_one_pair returns correlation and p-value", {

  x <- rnorm(50)
  y <- x + rnorm(50, sd = 0.1)

  res <- cor_one_pair(x, y, method = "pearson", include_pvalues = TRUE)

  expect_true(is.list(res))
  expect_named(res, c("correlation", "p_value"))
  expect_true(res$correlation > 0.8)
  expect_true(res$p_value < 0.001)
})


test_that("cor_one_pair omits p-values when requested", {

  x <- rnorm(30)
  y <- rnorm(30)

  res <- cor_one_pair(x, y, include_pvalues = FALSE)

  expect_true(is.na(res$p_value))
})


#############################################
test_that("M3_one_covariate returns correct structure", {

  items <- paste0("pain", 1:5)

  out <- M3_one_covariate(
    covariate_name = "age",
    dataset = toy_spadi_pain,
    items = items,
    method = "pearson",
    include_pvalues = TRUE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), length(items) + 1)

  expect_setequal(
    out$target_type,
    c("item", "score")
  )

  expect_true(all(out$target_name %in% c(items, "total_score")))
})



