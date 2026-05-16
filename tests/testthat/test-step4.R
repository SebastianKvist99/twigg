test_that("step4_structure_screen removes one covariate at a time and retests", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    A = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1),
    B = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1),
    C = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0)
  )

  calls <- list()

  local_mocked_bindings(
    step4_one_test = function(dataset, Xj, covariates, B = 10000) {
      calls[[length(calls) + 1]] <<- list(Xj = Xj, covariates = covariates)

      b_is_spurious <- Xj == "B" && "A" %in% covariates

      list(
        gamma = if (Xj == "C") 0.7 else 0.2,
        p_value = if (Xj == "A" || b_is_spurious) 0.50 else 0.01,
        strata_vars = setdiff(covariates, Xj)
      )
    },
    .package = "twigg"
  )

  out <- step4_structure_screen(
    data = dataset,
    items = c("item1", "item2"),
    covariates = c("A", "B", "C"),
    B = 10
  )

  expect_s3_class(out, "gllrm_step4")
  expect_equal(out$retained_covariates, c("B", "C"))
  expect_equal(out$removed_covariates, "A")
  expect_equal(vapply(calls, `[[`, character(1), "Xj"),
               c("A", "B", "C", "B", "C"))
  expect_equal(calls[[4]]$covariates, c("B", "C"))
  expect_equal(out$tests$conditioned_on[out$tests$iteration == 2 &
                                          out$tests$covariate == "B"],
               "C")
  expect_true(all(c(
    "initial_criterion_validity",
    "criterion_validity",
    "criterion_validity_comparison"
  ) %in% names(out)))
  expect_equal(out$initial_criterion_validity$covariate, c("A", "B", "C"))
  expect_equal(out$criterion_validity$covariate, c("B", "C"))
  expect_equal(out$criterion_validity_comparison$conclusion,
               c("Removed", "Retained", "Retained"))
})

test_that("step4_structure_screen supports adjusted p-values", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    A = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1),
    B = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1)
  )

  local_mocked_bindings(
    step4_one_test = function(dataset, Xj, covariates, B = 10000) {
      list(
        gamma = 0.4,
        p_value = if (Xj == "A") 0.04 else 0.01,
        strata_vars = setdiff(covariates, Xj)
      )
    },
    .package = "twigg"
  )

  out <- step4_structure_screen(
    data = dataset,
    items = c("item1", "item2"),
    covariates = c("A", "B"),
    adjust_method = "bonferroni",
    B = 10
  )

  expect_equal(out$removed_covariates, "A")
  expect_equal(out$tests$adjusted_p_value[out$tests$covariate == "A"][1], 0.08)
  expect_equal(out$tests$decision_p_value[out$tests$covariate == "A"][1], 0.08)
})

test_that("step4_structure_screen labels Monte Carlo p-values reported as zero", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    A = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1)
  )

  local_mocked_bindings(
    step4_one_test = function(dataset, Xj, covariates, B = 10000) {
      list(gamma = 0.4, p_value = 0, strata_vars = character(0))
    },
    .package = "twigg"
  )

  out <- step4_structure_screen(
    data = dataset,
    items = c("item1", "item2"),
    covariates = "A",
    B = 99
  )

  expect_equal(out$tests$p_value, 0)
  expect_equal(out$tests$p_value_label, "< 0.01")
  expect_equal(out$criterion_validity$p_value_label, "< 0.01")
  expect_equal(out$criterion_validity_comparison$initial_p_value_label,
               "< 0.01")
})

test_that("step4_structure_screen accepts a supplied score column", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    total = c(1, 2, 0, 1, 1, 1, 1, 2, 0, 1),
    A = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1)
  )

  local_mocked_bindings(
    step4_one_test = function(dataset, Xj, covariates, B = 10000) {
      expect_equal(dataset$Score, c(1, 2, 0, 1, 1, 1, 1, 2, 0, 1))
      list(gamma = 0.5, p_value = 0.01, strata_vars = character(0))
    },
    .package = "twigg"
  )

  out <- step4_structure_screen(
    data = dataset,
    items = c("item1", "item2"),
    covariates = "A",
    score = "total",
    B = 10
  )

  expect_equal(out$retained_covariates, "A")
  expect_equal(out$tests$conditioned_on, "None")
})

test_that("step4 S3 helpers print, summarize, and tidy", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    A = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1)
  )

  local_mocked_bindings(
    step4_one_test = function(dataset, Xj, covariates, B = 10000) {
      list(gamma = 0.5, p_value = 0.01, strata_vars = character(0))
    },
    .package = "twigg"
  )

  out <- step4_structure_screen(
    data = dataset,
    items = c("item1", "item2"),
    covariates = "A",
    B = 10
  )

  expect_output(print(out), "GLLRM Step 4")
  expect_s3_class(summary(out), "summary.gllrm_step4")
  expect_output(print(summary(out)), "Summary of GLLRM Step 4")
  expect_equal(tidy.gllrm_step4(out), out$tests)
})
