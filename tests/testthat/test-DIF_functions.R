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
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars, B) {
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

test_that("step3b drops one source at a time before retesting", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1),
    item2 = c(1, 1, 0, 0),
    SRH = c(0, 0, 1, 1),
    Sex = c(0, 1, 0, 1),
    BMI = c(1, 0, 1, 0)
  )

  calls <- list()

  local_mocked_bindings(
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars, B) {
      calls[[length(calls) + 1]] <<- list(Xj = Xj, strata_vars = strata_vars)

      bmi_is_spurious <- Xj == "BMI" && "SRH" %in% strata_vars

      list(
        gamma = 0,
        p_value = if (Xj == "SRH" || bmi_is_spurious) 0.50 else 0.01
      )
    },
    .package = "twigg"
  )

  out <- step3b_eliminate_sources(
    dataset = dataset,
    Yi = "item1",
    source_set = c("SRH", "BMI", "Sex"),
    items = c("item1", "item2"),
    crit_val = 0.05
  )

  expect_equal(out$remaining_sources, c("BMI", "Sex"))
  expect_equal(vapply(calls, `[[`, character(1), "Xj"),
               c("SRH", "BMI", "Sex", "BMI", "Sex"))
  expect_equal(calls[[4]]$strata_vars, c("Score", "Sex"))
  expect_equal(calls[[5]]$strata_vars, c("Score", "BMI"))
})

test_that("step3c drops one DIF item at a time before retesting", {

  dataset <- data.frame(
    item1 = c(0, 1, 0, 1),
    item2 = c(1, 1, 0, 0),
    item3 = c(0, 0, 1, 1),
    item4 = c(1, 0, 1, 0),
    Sex = c(0, 1, 0, 1)
  )

  calls <- list()

  local_mocked_bindings(
    partial_gamma_coin_test = function(dataset, Yi, Xj, strata_vars, B) {
      calls[[length(calls) + 1]] <<- list(Yi = Yi, strata_vars = strata_vars)

      item2_is_spurious <- Yi == "item2" && "item1" %in% strata_vars

      list(
        gamma = 0,
        p_value = if (Yi == "item1" || item2_is_spurious) 0.50 else 0.01
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

  expect_equal(out$remaining_dif_items, c("item2", "item3"))
  expect_equal(vapply(calls, `[[`, character(1), "Yi"),
               c("item1", "item2", "item3", "item2", "item3"))
  expect_equal(calls[[4]]$strata_vars, c("Score", "item3"))
  expect_equal(calls[[5]]$strata_vars, c("Score", "item2"))
  expect_equal(out$tests$item2$strata_vars, c("Score", "item3"))
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

  expect_named(out, c("gamma", "p_value"))
})
