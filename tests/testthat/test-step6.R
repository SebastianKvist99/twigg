test_that("step6_check_gllrm removes unsupported DIF and LD edges", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    item3 = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1),
    sex = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1),
    bmi = c(1, 2, 1, 3, 2, 2, 1, 3, 2, 1)
  )

  step5 <- build_gllrm_graph(
    items = c("item1", "item2", "item3"),
    covariates = c("sex", "bmi"),
    ld = data.frame(
      item1 = "item1",
      item2 = "item2",
      stringsAsFactors = FALSE
    ),
    dif = data.frame(
      item = "item2",
      DIF_source = "sex",
      conclusion = "DIF",
      stringsAsFactors = FALSE
    ),
    step4 = data.frame(
      covariate = "bmi",
      p_value = 0.01,
      stringsAsFactors = FALSE
    )
  )

  local_mocked_bindings(
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars,
                                       B = 10000,
                                       p_value_method = "monte_carlo") {
      list(
        gamma = 0.1,
        p_value = 0.20,
        p_value_method = p_value_method,
        strata_vars = strata_vars
      )
    },
    .package = "twigg"
  )

  out <- step6_check_gllrm(
    data = dataset,
    step5 = step5,
    alpha = 0.05,
    B = 10
  )

  expect_s3_class(out, "gllrm_step6")
  expect_equal(nrow(out$dif_tests), 1)
  expect_equal(nrow(out$ld_tests), 2)
  expect_equal(nrow(out$removed_dif), 1)
  expect_equal(nrow(out$removed_ld), 1)
  expect_equal(out$final_graph$edges$edge_type, "score_association")
  expect_s3_class(out$moralized_graph, "gllrm_moral_graph")
})

test_that("step6_check_gllrm keeps edges supported by any relevant check", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    item3 = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1),
    sex = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1),
    bmi = c(1, 2, 1, 3, 2, 2, 1, 3, 2, 1)
  )

  step5 <- build_gllrm_graph(
    items = c("item1", "item2", "item3"),
    covariates = c("sex", "bmi"),
    ld = data.frame(
      item1 = "item1",
      item2 = "item2",
      stringsAsFactors = FALSE
    ),
    dif = data.frame(
      item = "item2",
      DIF_source = "sex",
      conclusion = "DIF",
      stringsAsFactors = FALSE
    ),
    step4 = data.frame(
      covariate = "bmi",
      p_value = 0.01,
      stringsAsFactors = FALSE
    )
  )

  local_mocked_bindings(
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars,
                                       B = 10000,
                                       p_value_method = "monte_carlo") {
      p <- if (Yi == "item2" && Xj == "sex") 0.01 else
        if (any(grepl("item1", strata_vars))) 0.20 else 0.01
      list(
        gamma = 0.4,
        p_value = p,
        p_value_method = p_value_method,
        strata_vars = strata_vars
      )
    },
    .package = "twigg"
  )

  out <- step6_check_gllrm(
    data = dataset,
    step5 = step5,
    alpha = 0.05,
    B = 10
  )

  expect_equal(nrow(out$removed_dif), 0)
  expect_equal(nrow(out$removed_ld), 0)
  expect_equal(out$final_graph$edges$edge_type,
               c("local_dependence", "DIF", "score_association"))
  expect_true(any(out$ld_tests$supported))
})

test_that("step6 C5 conditioning combines score, other DIF items, and sources", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    item3 = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1),
    sex = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1),
    bmi = c(1, 2, 1, 3, 2, 2, 1, 3, 2, 1)
  )

  step5 <- build_gllrm_graph(
    items = c("item1", "item2", "item3"),
    covariates = c("sex", "bmi"),
    dif = data.frame(
      item = c("item2", "item3", "item2"),
      DIF_source = c("sex", "sex", "bmi"),
      conclusion = "DIF",
      stringsAsFactors = FALSE
    )
  )

  seen <- list()
  local_mocked_bindings(
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars,
                                       B = 10000,
                                       p_value_method = "monte_carlo") {
      seen[[paste(Yi, Xj, sep = "_")]] <<- strata_vars
      list(
        gamma = 0.3,
        p_value = 0.01,
        p_value_method = p_value_method,
        strata_vars = strata_vars
      )
    },
    .package = "twigg"
  )

  step6_check_gllrm(
    data = dataset,
    step5 = step5,
    alpha = 0.05,
    B = 10
  )

  expect_equal(seen$item2_sex, c("Score", "item3", "bmi"))
})

test_that("step6 supports adjusted p-values within DIF and LD families", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    sex = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1)
  )

  step5 <- build_gllrm_graph(
    items = c("item1", "item2"),
    covariates = "sex",
    dif = data.frame(
      item = c("item1", "item2"),
      DIF_source = "sex",
      conclusion = "DIF",
      stringsAsFactors = FALSE
    )
  )

  local_mocked_bindings(
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars,
                                       B = 10000,
                                       p_value_method = "monte_carlo") {
      list(
        gamma = 0.2,
        p_value = if (Yi == "item1") 0.03 else 0.01,
        p_value_method = p_value_method,
        strata_vars = strata_vars
      )
    },
    .package = "twigg"
  )

  out <- step6_check_gllrm(
    data = dataset,
    step5 = step5,
    alpha = 0.05,
    adjust_method = "bonferroni",
    B = 10
  )

  expect_equal(out$dif_tests$adjusted_p_value, c(0.06, 0.02))
  expect_equal(out$dif_tests$supported, c(FALSE, TRUE))
})

test_that("step6_check_gllrm passes through p_value_method", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    sex = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1)
  )

  step5 <- build_gllrm_graph(
    items = c("item1", "item2"),
    covariates = "sex",
    dif = data.frame(
      item = "item1",
      DIF_source = "sex",
      conclusion = "DIF",
      stringsAsFactors = FALSE
    ),
    ld = data.frame(
      item1 = "item1",
      item2 = "item2",
      stringsAsFactors = FALSE
    )
  )

  seen <- character()
  local_mocked_bindings(
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars,
                                       B = 10000,
                                       p_value_method = "monte_carlo") {
      seen <<- c(seen, p_value_method)
      list(
        gamma = 0.2,
        p_value = 0.01,
        p_value_method = p_value_method,
        strata_vars = strata_vars
      )
    },
    .package = "twigg"
  )

  out <- step6_check_gllrm(
    data = dataset,
    step5 = step5,
    p_value_method = "asymptotic",
    B = 10
  )

  expect_true(all(seen == "asymptotic"))
  expect_equal(out$p_value_method, "asymptotic")
  expect_equal(out$dif_tests$p_value_method, "asymptotic")
  expect_equal(out$ld_tests$p_value_method, c("asymptotic", "asymptotic"))
})
