test_that("build_gllrm_graph combines LD, DIF, and Step 4 edges", {

  ld <- list(genuine_ld = data.frame(
    item1 = "item1",
    item2 = "item2",
    gamma_cond_Ri = 0.30,
    gamma_cond_Rj = 0.50,
    arithmetic_mean_gamma = 0.40,
    weighted_partial_gamma = 0.45,
    stringsAsFactors = FALSE
  ))

  dif <- list(table = data.frame(
    item = c("item2", "item3"),
    DIF_source = c("sex", "bmi"),
    gamma_1 = c(0.20, 0.10),
    conditioned_on_1 = c("Score + bmi", "Score + sex"),
    p_value_1 = c(0.01, 0.40),
    gamma_2 = c(0.30, 0.20),
    conditioned_on_2 = c("Score", "Score"),
    p_value_2 = c(0.02, 0.50),
    conclusion = c("DIF", "Spurious"),
    stringsAsFactors = FALSE
  ))

  step4 <- structure(list(
    retained_covariates = "bmi",
    criterion_validity = data.frame(
      covariate = c("sex", "bmi"),
      gamma = c(0.10, 0.60),
      p_value = c(0.40, 0.01),
      adjusted_p_value = c(0.40, 0.02),
      conditioned_on = c("bmi", "sex"),
      supports_criterion_validity = c(FALSE, TRUE),
      stringsAsFactors = FALSE
    )
  ), class = "gllrm_step4")

  out <- build_gllrm_graph(
    items = c("item1", "item2", "item3"),
    covariates = c("sex", "bmi"),
    ld = ld,
    dif = dif,
    step4 = step4
  )

  expect_s3_class(out, "gllrm_graph")
  expect_equal(nrow(out$nodes), 6)
  expect_equal(out$nodes$type, c("score", "item", "item", "item",
                                 "covariate", "covariate"))
  expect_equal(out$edges$edge_type,
               c("local_dependence", "DIF", "score_association"))
  expect_equal(out$ld_edges$gamma, 0.45)
  expect_equal(out$dif_edges$gamma, 0.25)
  expect_equal(out$dif_edges$p_value, 0.02)
  expect_equal(out$score_edges$to, "bmi")
  expect_equal(out$score_edges$conditioned_on, "sex")
  expect_equal(out$edges$edge_id, 1:3)
})

test_that("build_gllrm_graph accepts direct data frame inputs", {

  ld <- data.frame(
    item1 = "item1",
    item2 = "item2",
    arithmetic_mean_gamma = 0.25,
    stringsAsFactors = FALSE
  )

  dif <- data.frame(
    item = "item2",
    DIF_source = "sex",
    conclusion = "DIF",
    stringsAsFactors = FALSE
  )

  step4 <- data.frame(
    covariate = "sex",
    gamma = 0.30,
    p_value = 0.01,
    stringsAsFactors = FALSE
  )

  out <- build_gllrm_graph(
    items = c("item1", "item2"),
    covariates = "sex",
    ld = ld,
    dif = dif,
    step4 = step4,
    include_score_node = FALSE
  )

  expect_equal(nrow(out$edges), 2)
  expect_false("score_association" %in% out$edges$edge_type)
  expect_false("Score" %in% out$nodes$name)
})

test_that("build_gllrm_graph validates edge endpoints", {

  expect_error(
    build_gllrm_graph(
      items = "item1",
      covariates = "sex",
      ld = data.frame(item1 = "item1", item2 = "missing")
    ),
    "not listed in 'items': missing"
  )

  expect_error(
    build_gllrm_graph(
      items = "item1",
      covariates = "sex",
      dif = data.frame(item = "item1", DIF_source = "missing")
    ),
    "not listed in 'covariates': missing"
  )

  expect_error(
    build_gllrm_graph(
      items = "item1",
      covariates = "sex",
      step4 = data.frame(covariate = "missing")
    ),
    "not listed in 'covariates': missing"
  )
})

test_that("build_gllrm_graph reports meaningful input errors", {

  expect_error(
    build_gllrm_graph(items = character(0), covariates = "sex"),
    "'items' must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    build_gllrm_graph(
      items = "item1",
      covariates = "sex",
      ld = data.frame(item1 = "item1")
    ),
    "'ld' is missing required column\\(s\\): item2"
  )

  expect_error(
    build_gllrm_graph(
      items = "item1",
      covariates = "sex",
      dif = 1:5
    ),
    "'dif' must be a data frame"
  )
})

test_that("gllrm_graph S3 helpers print, summarize, and expose igraph", {

  out <- build_gllrm_graph(
    items = c("item1", "item2"),
    covariates = "sex",
    ld = data.frame(
      item1 = "item1",
      item2 = "item2",
      arithmetic_mean_gamma = 0.25,
      stringsAsFactors = FALSE
    )
  )

  expect_output(print(out), "GLLRM Step 5 graph")
  expect_s3_class(summary(out), "summary.gllrm_graph")
  expect_output(print(summary(out)), "Summary of GLLRM Step 5")

  if (requireNamespace("igraph", quietly = TRUE)) {
    expect_s3_class(as_igraph(out), "igraph")
  } else {
    expect_error(as_igraph(out), "igraph")
  }
})

test_that("step5_build_graph is a convenience wrapper", {

  out <- step5_build_graph(
    items = "item1",
    covariates = "sex"
  )

  expect_s3_class(out, "gllrm_graph")
  expect_equal(nrow(out$edges), 0)
})

