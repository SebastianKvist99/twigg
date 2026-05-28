test_that("genuine_LD reports arithmetic and weighted partial gamma", {
  ld1 <- data.frame(
    Item1 = c("a", "c"),
    Item2 = c("b", "d"),
    gamma = c(0.9, 0.4),
    pvalue = c(0.001, 0.001),
    comparable_pairs = c(1, 100)
  )

  ld2 <- data.frame(
    Item1 = c("b", "d"),
    Item2 = c("a", "c"),
    gamma = c(0.1, 0.4),
    pvalue = c(0.001, 0.001),
    comparable_pairs = c(999, 100)
  )

  capture.output(out <- genuine_LD(
    list(all_LD = list(ld1, ld2)),
    number_of_multiple_tests = 4
  ))

  expect_named(out$genuine_ld, c(
    "item1", "item2",
    "gamma_cond_Ri", "gamma_cond_Rj",
    "arithmetic_mean_gamma", "weighted_partial_gamma"
  ))
  expect_equal(out$genuine_ld$item1[1], "c")
  expect_equal(out$genuine_ld$item2[1], "d")

  ab <- out$all_ld_detected[out$all_ld_detected$pair_id == "a_b", ]
  expect_equal(ab$arithmetic_mean_gamma, 0.5)
  expect_equal(ab$weighted_partial_gamma, 0.1008)
})

test_that("genuine_LD computes pair summaries from both directions", {
  ld1 <- data.frame(
    Item1 = "a",
    Item2 = "b",
    gamma = 0.9,
    pvalue = 0.001,
    comparable_pairs = 1
  )

  ld2 <- data.frame(
    Item1 = "b",
    Item2 = "a",
    gamma = 0.1,
    pvalue = 0.9,
    comparable_pairs = 999
  )

  capture.output(out <- genuine_LD(
    list(all_LD = list(ld1, ld2)),
    number_of_multiple_tests = 2
  ))

  expect_equal(out$genuine_ld$arithmetic_mean_gamma, 0.5)
  expect_equal(out$genuine_ld$weighted_partial_gamma, 0.1008)
})

test_that("genuine_LD reports DIGRAM-like LD evidence blocks", {
  ld1 <- data.frame(
    Item1 = c("a", "c", "e", "g"),
    Item2 = c("b", "d", "f", "h"),
    gamma = c(0.3, 0.5, -0.2, -0.6),
    pvalue = c(0.001, 0.001, 0.001, 0.001),
    comparable_pairs = c(100, 100, 100, 100)
  )

  ld2 <- data.frame(
    Item1 = c("b", "d", "f", "h"),
    Item2 = c("a", "c", "e", "g"),
    gamma = c(0.4, -0.1, 0.1, -0.7),
    pvalue = c(0.001, 0.9, 0.9, 0.001),
    comparable_pairs = c(100, 100, 100, 100)
  )

  capture.output(out <- genuine_LD(
    list(all_LD = list(ld1, ld2)),
    method = "none"
  ))

  blocks <- out$ld_evidence_summary

  expect_equal(
    blocks$two_significant_positive_partial_correlations$item1, "a")
  expect_equal(
    blocks$one_significant_positive_partial_correlation$item1, "c")
  expect_equal(
    blocks$one_significant_negative_partial_correlation$item1, "e")
  expect_equal(
    blocks$two_significant_negative_partial_correlations$item1, "g")
})

test_that("genuine_LD finishes two-significant blocks before one-significant blocks", {
  ld1 <- data.frame(
    Item1 = c("a", "c"),
    Item2 = c("b", "d"),
    gamma = c(-0.9, 0.2),
    pvalue = c(0.001, 0.001),
    comparable_pairs = c(100, 100)
  )

  ld2 <- data.frame(
    Item1 = c("b", "d"),
    Item2 = c("a", "c"),
    gamma = c(-0.9, 0.2),
    pvalue = c(0.9, 0.001),
    comparable_pairs = c(100, 100)
  )

  capture.output(out <- genuine_LD(
    list(all_LD = list(ld1, ld2)),
    method = "none"
  ))

  expect_equal(out$genuine_ld$item1[1], "c")
  expect_equal(out$genuine_ld$item2[1], "d")
  expect_equal(out$genuine_ld$item1[2], "a")
  expect_equal(out$genuine_ld$item2[2], "b")
})

test_that("genuine_LD handles underscored item names with one significant direction", {
  ld1 <- data.frame(
    Item1 = "sat_pre_2",
    Item2 = "sat_pre_3",
    gamma = 0.10,
    pvalue = 0.90,
    comparable_pairs = 100
  )

  ld2 <- data.frame(
    Item1 = "sat_pre_3",
    Item2 = "sat_pre_2",
    gamma = 0.8823529,
    pvalue = 4.46989e-07,
    comparable_pairs = 100
  )

  capture.output(out <- genuine_LD(
    list(all_LD = list(ld1, ld2)),
    number_of_multiple_tests = 2,
    method = "BH"
  ))

  expect_equal(nrow(out$genuine_ld), 1)
  expect_equal(out$genuine_ld$item1, "sat_pre_2")
  expect_equal(out$genuine_ld$item2, "sat_pre_3")
  expect_equal(
    out$ld_evidence_summary$one_significant_positive_partial_correlation$item1,
    "sat_pre_2"
  )
})

test_that("genuine_LD discards evidence conditioned on selected LD items", {
  ld1 <- data.frame(
    Item1 = c("P1", "P3"),
    Item2 = c("P4", "P4"),
    gamma = c(-0.3596987, 0.5495495),
    pvalue = c(1.435686e-03, 5.249030e-09),
    padj.BH = c(2.871373e-02, 1.049806e-07),
    comparable_pairs = c(100, 100)
  )

  ld2 <- data.frame(
    Item1 = c("P4", "P4"),
    Item2 = c("P1", "P3"),
    gamma = c(-0.2943820, 0.3837838),
    pvalue = c(0.20, 0.0003444608),
    padj.BH = c(1.00, 0.006889216),
    comparable_pairs = c(100, 100)
  )

  capture.output(out <- genuine_LD(
    list(all_LD = list(ld1, ld2))
  ))

  expect_equal(nrow(out$genuine_ld), 1)
  expect_equal(out$genuine_ld$item1, "P3")
  expect_equal(out$genuine_ld$item2, "P4")
  expect_equal(out$genuine_ld$gamma_cond_Ri, 0.3837838)
  expect_equal(out$genuine_ld$gamma_cond_Rj, 0.5495495)
})

test_that("genuine_LD honors adjusted p-values from screen_LD output", {
  ld1 <- data.frame(
    Item1 = c("a", "c"),
    Item2 = c("b", "d"),
    gamma = c(0.4, -0.5),
    pvalue = c(0.001, 0.02),
    padj.BH = c(0.004, 0.08),
    comparable_pairs = c(100, 100)
  )

  ld2 <- data.frame(
    Item1 = c("b", "d"),
    Item2 = c("a", "c"),
    gamma = c(0.4, -0.5),
    pvalue = c(0.001, 0.02),
    padj.BH = c(0.004, 0.08),
    comparable_pairs = c(100, 100)
  )

  capture.output(out <- genuine_LD(
    list(all_LD = list(ld1, ld2))
  ))

  expect_equal(nrow(out$genuine_ld), 1)
  expect_equal(out$genuine_ld$item1, "a")
  expect_equal(out$genuine_ld$item2, "b")
  expect_equal(nrow(
    out$ld_evidence_summary$two_significant_negative_partial_correlations
  ), 0)
})

test_that("screen_LD complete-case filtering uses item columns only", {
  dataset <- data.frame(
    item1 = c(0, 1, 1, 0, 1, 0, 1, 0, 1, 0),
    item2 = c(0, 1, 0, 1, 1, 0, 0, 1, 1, 0),
    exo = c(NA, NA, 1, 1, 2, 2, 1, 2, 1, 2)
  )

  capture.output(out_full <- screen_LD(dataset, c("item1", "item2")))
  capture.output(out_items <- screen_LD(dataset[c("item1", "item2")],
                                        c("item1", "item2")))

  expect_equal(out_full$all_LD[[1]]$gamma, out_items$all_LD[[1]]$gamma)
  expect_equal(out_full$all_LD[[2]]$gamma, out_items$all_LD[[2]]$gamma)
})

test_that("screen_LD accepts matrix and tibble-like data", {
  dataset <- data.frame(
    item1 = c(0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    item2 = c(0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0)
  )
  dataset_tbl <- structure(dataset, class = c("tbl_df", "tbl", "data.frame"))
  dataset_mat <- as.matrix(dataset)

  capture.output(expect_no_error(
    screen_LD(dataset_tbl, c("item1", "item2"))
  ))
  capture.output(expect_no_error(
    screen_LD(dataset_mat, c("item1", "item2"))
  ))
})

test_that("screen_LD reports meaningful validation errors", {
  dataset <- data.frame(
    item1 = c(0, 1, 1, 0, 1, 0, 1, 0, 1, 0),
    item2 = letters[1:10]
  )

  expect_error(
    screen_LD(1:10, c("item1", "item2")),
    "rectangular object"
  )
  expect_error(
    screen_LD(dataset, c("item1", "item2")),
    "One or more items are not numerical: item2"
  )
  expect_error(
    screen_LD(dataset, "missing"),
    "not in the data frame: missing"
  )
})
