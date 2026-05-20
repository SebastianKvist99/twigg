test_that("step3b tests a full source pass before dropping sources", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1),
    item2 = c(1, 1, 0, 0),
    SRH = c(0, 0, 1, 1),
    Sex = c(0, 1, 0, 1),
    BMI = c(1, 0, 1, 0)
  )

  calls <- list()

  local_mocked_bindings(
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars, B,
                                       p_value_method = "monte_carlo") {
      calls[[length(calls) + 1]] <<- list(Xj = Xj, strata_vars = strata_vars)

      list(
        gamma = 0,
        p_value = if (Xj == "SRH") 0.50 else 0.01
      )
    },
    .package = "twigg"
  )

  out <- step3b_eliminate_sources(
    dataset = dataset,
    Yi = "item1",
    source_set = c("SRH", "Sex", "BMI"),
    items = c("item1", "item2"),
    crit_val = 0.05
  )

  expect_equal(out$remaining_sources, c("Sex", "BMI"))
  expect_equal(vapply(calls, `[[`, character(1), "Xj"),
               c("SRH", "Sex", "BMI", "Sex", "BMI"))
  expect_equal(calls[[1]]$strata_vars, c("Score", "Sex", "BMI"))
  expect_equal(calls[[2]]$strata_vars, c("Score", "SRH", "BMI"))
  expect_equal(calls[[3]]$strata_vars, c("Score", "SRH", "Sex"))
  expect_equal(calls[[4]]$strata_vars, c("Score", "BMI"))
  expect_equal(calls[[5]]$strata_vars, c("Score", "Sex"))
})

test_that("step3b drops the source with the highest nonsignificant p-value", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1),
    item2 = c(1, 1, 0, 0),
    SRH = c(0, 0, 1, 1),
    Sex = c(0, 1, 0, 1),
    BMI = c(1, 0, 1, 0)
  )

  calls <- list()

  local_mocked_bindings(
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars, B,
                                       p_value_method = "monte_carlo") {
      calls[[length(calls) + 1]] <<- list(Xj = Xj, strata_vars = strata_vars)

      list(
        gamma = 0,
        p_value = switch(
          Xj,
          SRH = 0.10,
          BMI = 0.50,
          Sex = 0.01
        )
      )
    },
    .package = "twigg"
  )

  out <- step3b_eliminate_sources(
    dataset = dataset,
    Yi = "item1",
    source_set = c("SRH", "Sex", "BMI"),
    items = c("item1", "item2"),
    crit_val = 0.05
  )

  expect_equal(out$remaining_sources, "Sex")
  expect_equal(vapply(calls, `[[`, character(1), "Xj"),
               c("SRH", "Sex", "BMI", "SRH", "Sex", "Sex"))
  expect_equal(calls[[4]]$strata_vars, c("Score", "Sex"))
  expect_equal(calls[[5]]$strata_vars, c("Score", "SRH"))
  expect_equal(calls[[6]]$strata_vars, "Score")
})

test_that("step3c drops the DIF item with the highest nonsignificant p-value", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1),
    item2 = c(1, 1, 0, 0),
    item3 = c(0, 0, 1, 1),
    item4 = c(1, 0, 1, 0),
    Sex = c(0, 1, 0, 1)
  )

  calls <- list()

  local_mocked_bindings(
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars, B,
                                       p_value_method = "monte_carlo") {
      calls[[length(calls) + 1]] <<- list(Yi = Yi, strata_vars = strata_vars)

      list(
        gamma = 0,
        p_value = switch(
          Yi,
          item1 = 0.10,
          item2 = 0.50,
          item3 = 0.01
        )
      )
    },
    .package = "twigg"
  )

  out <- step3c_eliminate_dif_items(
    dataset = dataset,
    Xj = "Sex",
    dif_set = c("item1", "item2", "item3"),
    items = c("item1", "item2", "item3", "item4"),
    crit_val = 0.05
  )

  expect_equal(out$remaining_dif_items, "item3")
  expect_equal(vapply(calls, `[[`, character(1), "Yi"),
               c("item1", "item2", "item3", "item1", "item3", "item3"))
  expect_equal(calls[[4]]$strata_vars, c("Score", "item3"))
  expect_equal(calls[[5]]$strata_vars, c("Score", "item1"))
  expect_equal(calls[[6]]$strata_vars, "Score")
  expect_equal(out$tests$item3$strata_vars, "Score")
})

test_that("combine_step3bc returns transparent comparison table", {

  step3b_results <- list(
    item1 = list(
      remaining_sources = "Sex",
      tests = list(
        Sex = list(
          gamma = 0.4,
          p_value = 0.01,
          strata_vars = c("Score", "BMI")
        ),
        BMI = list(
          gamma = 0.1,
          p_value = 0.40,
          strata_vars = c("Score", "Sex")
        )
      )
    )
  )

  step3c_results <- list(
    Sex = list(
      remaining_dif_items = "item1",
      tests = list(
        item1 = list(
          gamma = 0.5,
          p_value = 0.02,
          strata_vars = "Score"
        )
      )
    ),
    BMI = list(
      remaining_dif_items = character(0),
      tests = list(
        item1 = list(
          gamma = 0.2,
          p_value = 0.20,
          strata_vars = "Score"
        )
      )
    )
  )

  printed <- utils::capture.output(
    out <- combine_step3bc(
      step3b_results = step3b_results,
      step3c_results = step3c_results,
      original_source_list = list(item1 = c("Sex", "BMI")),
      original_dif_list = list(Sex = "item1", BMI = "item1")
    )
  )

  expect_named(out, c("SOURCE", "DIF", "table"))
  expect_true(length(printed) > 0)
  expect_equal(out$SOURCE$item1, "Sex")
  expect_equal(out$DIF$Sex, "item1")
  expect_equal(nrow(out$table), 2)
  expect_true(all(c(
    "gamma_1", "conditioned_on_1", "p_value_1",
    "gamma_2", "conditioned_on_2", "p_value_2"
  ) %in% names(out$table)))
  expect_equal(out$table$conditioned_on_1[out$table$DIF_source == "Sex"],
               "Score + BMI")
  expect_equal(out$table$conclusion[out$table$DIF_source == "Sex"], "DIF")
  expect_equal(out$table$conclusion[out$table$DIF_source == "BMI"], "Spurious")
})

test_that("partial_gamma_coin_test removes incomplete rows before block checks", {

  dataset <- data.frame(
    item = c(0, 1, 0, 1, 0, 1),
    covariate = c(0, 1, NA, 1, 0, 1),
    strata = c("a", "a", "b", "b", "c", "c")
  )

  expect_no_error(
    out <- partial_gamma_coin_test(
      dataset = dataset,
      Yi = "item",
      Xj = "covariate",
      strata_vars = "strata",
      B = 10
    )
  )

  expect_named(out, c("gamma", "se", "lower", "upper", "p_value",
                      "p_value_method"))
})

test_that("screen_DIF complete-case filtering is covariate-specific", {

  dataset <- data.frame(
    item1 = c(0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    item2 = c(0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0),
    exo1 = c(1, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2),
    exo2 = c(NA, NA, 1, 1, 2, 2, 1, 2, 1, 2, NA, NA)
  )

  capture.output(out_both <- screen_DIF(
    dataset, c("item1", "item2"), c("exo1", "exo2"), method = "none"))
  capture.output(out_one <- screen_DIF(
    dataset, c("item1", "item2"), "exo1", method = "none"))

  both_exo1 <- out_both$full_DIF[out_both$full_DIF$Var == "exo1",
                                 c("Item", "Var", "gamma", "pvalue")]
  one_exo1 <- out_one$full_DIF[, c("Item", "Var", "gamma", "pvalue")]

  row.names(both_exo1) <- NULL
  row.names(one_exo1) <- NULL

  expect_equal(both_exo1, one_exo1)
})

test_that("screen_DIF accepts matrix and tibble-like data", {

  dataset <- data.frame(
    item1 = c(0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    item2 = c(0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0),
    exo = c(1, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2)
  )
  dataset_tbl <- structure(dataset, class = c("tbl_df", "tbl", "data.frame"))
  dataset_mat <- as.matrix(dataset)

  capture.output(expect_no_error(
    screen_DIF(dataset_tbl, c("item1", "item2"), "exo", method = "none")
  ))
  capture.output(expect_no_error(
    screen_DIF(dataset_mat, c("item1", "item2"), "exo", method = "none")
  ))
})

test_that("screen_DIF reports meaningful validation errors", {

  dataset <- data.frame(
    item1 = c(0, 1, 1, 0, 1, 0, 1, 0, 1, 0),
    item2 = letters[1:10],
    exo = c(1, 1, 2, 2, 1, 2, 1, 2, 1, 2)
  )

  expect_error(
    screen_DIF(1:10, c("item1", "item2"), "exo"),
    "rectangular object"
  )
  expect_error(
    screen_DIF(dataset, c("item1", "item2"), "exo"),
    "One or more items are not numerical: item2"
  )
  expect_error(
    screen_DIF(dataset, c("item1"), "missing"),
    "not in the data frame: missing"
  )
})

test_that("partial_gamma_coin_test can use gamma asymptotic p-values", {

  dataset <- data.frame(
    item = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    covariate = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0),
    strata = rep(c("a", "b", "c"), each = 4)
  )

  out <- partial_gamma_coin_test(
    dataset = dataset,
    Yi = "item",
    Xj = "covariate",
    strata_vars = "strata",
    p_value_method = "asymptotic"
  )

  expect_named(out, c("gamma", "se", "lower", "upper", "p_value",
                      "p_value_method"))
  expect_equal(out$p_value_method, "asymptotic")
  expect_true(is.numeric(out$p_value))
  expect_true(is.numeric(out$se))
  expect_equal(
    out$p_value,
    2 * stats::pnorm(abs(out$gamma / out$se), lower.tail = FALSE)
  )
})

test_that("compute_partial_gamma can return asymptotic statistics", {

  dataset <- data.frame(
    item = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    covariate = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0),
    strata = rep(c("a", "b", "c"), each = 4)
  )

  gamma <- compute_partial_gamma(dataset, "item", "covariate", "strata")
  stats <- compute_partial_gamma(
    dataset, "item", "covariate", "strata", return_stats = TRUE)

  expect_named(stats, c("gamma", "se", "lower", "upper", "p_value"))
  expect_equal(stats$gamma, gamma)
  expect_true(is.numeric(stats$se))
})

test_that("partial_gamma_coin_test can still use coin asymptotic p-values", {

  dataset <- data.frame(
    item = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    covariate = c(0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0),
    strata = rep(c("a", "b", "c"), each = 4)
  )

  out <- partial_gamma_coin_test(
    dataset = dataset,
    Yi = "item",
    Xj = "covariate",
    strata_vars = "strata",
    p_value_method = "coin_asymptotic"
  )

  expect_named(out, c("gamma", "se", "lower", "upper", "p_value",
                      "p_value_method"))
  expect_equal(out$p_value_method, "coin_asymptotic")
  expect_true(is.numeric(out$p_value))
})
