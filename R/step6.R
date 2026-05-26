#' Prepare data for Step 6 conditional independence tests
#'
#' @keywords internal
step6_prepare_data <- function(data, items, covariates, score = NULL) {
  dataset <- as_screening_data_frame(data)
  are_items_in_df(dataset, items)
  are_items_numeric(dataset, items)
  are_covaraites_in_df(dataset, covariates)

  if (is.null(score)) {
    dataset$Score <- compute_total_score(dataset[items])
  } else if (is.character(score) && length(score) == 1) {
    if (!(score %in% names(dataset))) {
      stop("'score' column is not in 'data'", call. = FALSE)
    }
    dataset$Score <- dataset[[score]]
  } else if (is.numeric(score) && length(score) == nrow(dataset)) {
    dataset$Score <- score
  } else {
    stop("'score' must be NULL, a single column name, or a numeric vector",
         call. = FALSE)
  }

  dataset$Score <- check_covariate(dataset$Score, "Score")
  for (Xj in covariates) {
    dataset[[Xj]] <- check_covariate(dataset[[Xj]], Xj)
  }

  dataset[, unique(c("Score", items, covariates)), drop = FALSE]
}

step6_check_adjust_method <- function(adjust_method) {
  if (is.null(adjust_method)) return(invisible(TRUE))

  if (!(adjust_method %in% stats::p.adjust.methods)) {
    stop("'adjust_method' must be NULL or a valid p.adjust method",
         call. = FALSE)
  }
  invisible(TRUE)
}

step6_format_conditioning <- function(vars) {
  vars <- vars[!is.na(vars) & vars != ""]
  if (length(vars) == 0) "None" else paste(vars, collapse = " + ")
}

step6_empty_dif_tests <- function() {
  data.frame(
    item = character(0),
    covariate = character(0),
    hypothesis = character(0),
    gamma = numeric(0),
    p_value = numeric(0),
    adjusted_p_value = numeric(0),
    decision_p_value = numeric(0),
    p_value_label = character(0),
    adjusted_p_value_label = character(0),
    decision_p_value_label = character(0),
    conditioned_on = character(0),
    supported = logical(0),
    stringsAsFactors = FALSE
  )
}

step6_empty_ld_tests <- function() {
  data.frame(
    item1 = character(0),
    item2 = character(0),
    hypothesis = character(0),
    rest_component = character(0),
    gamma = numeric(0),
    p_value = numeric(0),
    adjusted_p_value = numeric(0),
    decision_p_value = numeric(0),
    p_value_label = character(0),
    adjusted_p_value_label = character(0),
    decision_p_value_label = character(0),
    conditioned_on = character(0),
    supported = logical(0),
    stringsAsFactors = FALSE
  )
}

step6_apply_decisions <- function(x, alpha, adjust_method, B) {
  if (nrow(x) == 0) return(x)

  x$adjusted_p_value <- NA_real_
  x$decision_p_value <- x$p_value
  if (!is.null(adjust_method)) {
    x$adjusted_p_value <- stats::p.adjust(x$p_value, method = adjust_method)
    x$decision_p_value <- x$adjusted_p_value
  }

  x$p_value_label <- step4_format_p_value(x$p_value, B = B)
  x$adjusted_p_value_label <- step4_format_p_value(
    x$adjusted_p_value,
    B = B
  )
  x$decision_p_value_label <- step4_format_p_value(
    x$decision_p_value,
    B = B
  )
  x$supported <- !is.na(x$decision_p_value) & x$decision_p_value <= alpha
  x
}

step6_source_maps <- function(step5) {
  dif <- step5$dif_edges
  if (nrow(dif) == 0) {
    return(list(
      sources_by_item = list(),
      dif_items_by_source = list()
    ))
  }

  list(
    sources_by_item = split(as.character(dif$to), as.character(dif$from)),
    dif_items_by_source = split(as.character(dif$from), as.character(dif$to))
  )
}

step6_c5_tests <- function(dataset, step5, alpha, adjust_method, B) {
  dif <- step5$dif_edges
  if (nrow(dif) == 0) return(step6_empty_dif_tests())

  maps <- step6_source_maps(step5)
  rows <- lapply(seq_len(nrow(dif)), function(i) {
    Yi <- as.character(dif$from[i])
    Xj <- as.character(dif$to[i])
    dif_items <- setdiff(maps$dif_items_by_source[[Xj]], Yi)
    sources <- setdiff(maps$sources_by_item[[Yi]], Xj)
    strata_vars <- unique(c("Score", dif_items, sources))

    test <- partial_gamma_coin_test(
      dataset = dataset,
      Yi = Yi,
      Xj = Xj,
      strata_vars = strata_vars,
      B = B
    )

    data.frame(
      item = Yi,
      covariate = Xj,
      hypothesis = "C5",
      gamma = test$gamma,
      p_value = unname(test$p_value[1]),
      adjusted_p_value = NA_real_,
      decision_p_value = unname(test$p_value[1]),
      p_value_label = NA_character_,
      adjusted_p_value_label = NA_character_,
      decision_p_value_label = NA_character_,
      conditioned_on = step6_format_conditioning(strata_vars),
      supported = FALSE,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  step6_apply_decisions(out, alpha, adjust_method, B)
}

step6_ld_pair_id <- function(item1, item2) {
  paste(pmin(item1, item2), pmax(item1, item2), sep = "\r")
}

step6_component <- function(items, ld_edges, start, exclude_pair) {
  remaining <- ld_edges
  if (nrow(remaining) > 0) {
    pair_id <- step6_ld_pair_id(remaining$from, remaining$to)
    remaining <- remaining[pair_id != exclude_pair, , drop = FALSE]
  }

  seen <- start
  frontier <- start
  repeat {
    neighbors <- character(0)
    for (node in frontier) {
      neighbors <- c(
        neighbors,
        remaining$to[remaining$from == node],
        remaining$from[remaining$to == node]
      )
    }
    neighbors <- setdiff(unique(neighbors), seen)
    if (length(neighbors) == 0) break
    seen <- c(seen, neighbors)
    frontier <- neighbors
  }

  intersect(seen, items)
}

step6_rest_score_name <- function(component) {
  paste0(".step6_rest_", paste(component, collapse = "_"))
}

step6_add_rest_score <- function(dataset, component) {
  nm <- step6_rest_score_name(component)
  if (!nm %in% names(dataset)) {
    dataset[[nm]] <- dataset$Score - rowSums(dataset[, component, drop = FALSE])
  }
  list(dataset = dataset, name = nm)
}

step6_sources_for_items <- function(items, sources_by_item) {
  unique(unlist(sources_by_item[items], use.names = FALSE))
}

step6_one_ld_test <- function(dataset, Yi, Xj, hypothesis, component,
                              sources_by_item, B) {
  rest <- step6_add_rest_score(dataset, component)
  dataset <- rest$dataset
  source_vars <- step6_sources_for_items(component, sources_by_item)
  strata_vars <- unique(c(rest$name, source_vars))

  test <- partial_gamma_coin_test(
    dataset = dataset,
    Yi = Yi,
    Xj = Xj,
    strata_vars = strata_vars,
    B = B
  )

  data.frame(
    item1 = Yi,
    item2 = Xj,
    hypothesis = hypothesis,
    rest_component = paste(component, collapse = " + "),
    gamma = test$gamma,
    p_value = unname(test$p_value[1]),
    adjusted_p_value = NA_real_,
    decision_p_value = unname(test$p_value[1]),
    p_value_label = NA_character_,
    adjusted_p_value_label = NA_character_,
    decision_p_value_label = NA_character_,
    conditioned_on = step6_format_conditioning(strata_vars),
    supported = FALSE,
    stringsAsFactors = FALSE
  )
}

step6_ld_tests <- function(dataset, items, step5, alpha, adjust_method, B) {
  ld <- step5$ld_edges
  if (nrow(ld) == 0) return(step6_empty_ld_tests())

  maps <- step6_source_maps(step5)
  rows <- list()
  for (i in seq_len(nrow(ld))) {
    Ya <- as.character(ld$from[i])
    Yb <- as.character(ld$to[i])
    pair_id <- step6_ld_pair_id(Ya, Yb)
    comp_a <- step6_component(items, ld, Ya, pair_id)
    comp_b <- step6_component(items, ld, Yb, pair_id)

    rows[[length(rows) + 1]] <- step6_one_ld_test(
      dataset = dataset,
      Yi = Ya,
      Xj = Yb,
      hypothesis = "C7",
      component = comp_a,
      sources_by_item = maps$sources_by_item,
      B = B
    )
    rows[[length(rows) + 1]] <- step6_one_ld_test(
      dataset = dataset,
      Yi = Ya,
      Xj = Yb,
      hypothesis = "C9",
      component = comp_b,
      sources_by_item = maps$sources_by_item,
      B = B
    )
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  step6_apply_decisions(out, alpha, adjust_method, B)
}

step6_supported_dif_pairs <- function(dif_tests) {
  if (nrow(dif_tests) == 0) return(character(0))
  with(dif_tests[dif_tests$supported, , drop = FALSE],
       paste(item, covariate, sep = "\r"))
}

step6_supported_ld_pairs <- function(ld_tests) {
  if (nrow(ld_tests) == 0) return(character(0))
  tests <- ld_tests[ld_tests$supported, , drop = FALSE]
  step6_ld_pair_id(tests$item1, tests$item2)
}

step6_rebuild_graph <- function(step5, dif_tests, ld_tests) {
  out <- step5

  if (nrow(out$dif_edges) > 0) {
    keep_dif <- paste(out$dif_edges$from, out$dif_edges$to, sep = "\r") %in%
      step6_supported_dif_pairs(dif_tests)
    out$dif_edges <- out$dif_edges[keep_dif, , drop = FALSE]
  }

  if (nrow(out$ld_edges) > 0) {
    keep_ld <- step6_ld_pair_id(out$ld_edges$from, out$ld_edges$to) %in%
      step6_supported_ld_pairs(ld_tests)
    out$ld_edges <- out$ld_edges[keep_ld, , drop = FALSE]
  }

  out$edges <- step5_topology_edges(step5_bind_edges(list(
    out$ld_edges,
    out$dif_edges,
    out$score_edges
  )))
  out
}

step6_removed_dif <- function(step5, dif_tests) {
  if (nrow(step5$dif_edges) == 0) return(step5$dif_edges)
  supported <- step6_supported_dif_pairs(dif_tests)
  pair_id <- paste(step5$dif_edges$from, step5$dif_edges$to, sep = "\r")
  step5$dif_edges[!(pair_id %in% supported), , drop = FALSE]
}

step6_removed_ld <- function(step5, ld_tests) {
  if (nrow(step5$ld_edges) == 0) return(step5$ld_edges)
  supported <- step6_supported_ld_pairs(ld_tests)
  pair_id <- step6_ld_pair_id(step5$ld_edges$from, step5$ld_edges$to)
  step5$ld_edges[!(pair_id %in% supported), , drop = FALSE]
}

#' Step 6: check the GLLRM defined by Step 5
#'
#' Performs the first Step 6 check of a graphical loglinear Rasch model by
#' retesting the retained DIF and local-dependence edges from Step 5 under
#' conditioning sets implied by the Step 5 graph. DIF edges are checked using
#' C5-style hypotheses. Local-dependence edges are checked using two C7/C9-style
#' hypotheses based on the LD components containing each item.
#'
#' @details
#' This function implements the first confirmatory Step 6 layer. It does not yet
#' search the moralized graph for all minimal separating sets. Unsupported DIF
#' and LD edges are removed from the final graph when the corresponding Step 6
#' test family no longer rejects conditional independence at \code{alpha}.
#'
#' @param data A data frame containing item responses and covariates.
#' @param step5 A \code{"gllrm_graph"} object returned by
#'   \code{\link{build_gllrm_graph}}.
#' @param items Optional character vector naming item columns. If \code{NULL},
#'   item names are taken from \code{step5$nodes}.
#' @param covariates Optional character vector naming covariate columns. If
#'   \code{NULL}, covariate names are taken from \code{step5$nodes}.
#' @param score Optional total score. If \code{NULL}, the score is computed as
#'   the row sum of \code{items}. If a character string, it is interpreted as a
#'   column in \code{data}. If numeric, it must have length \code{nrow(data)}.
#' @param alpha Numeric significance level for retaining Step 5 DIF and LD
#'   edges.
#' @param adjust_method Optional p-value adjustment method passed to
#'   \code{\link[stats]{p.adjust}} separately within the DIF and LD Step 6 test
#'   families. If \code{NULL}, raw p-values are used.
#' @param B Integer. Number of Monte Carlo samples used by the conditional
#'   independence tests.
#'
#' @returns An object of class \code{"gllrm_step6"}, a list containing the Step
#'   6 DIF and LD tests, removed edges, the updated final Step 5 graph, and its
#'   moralized graph.
#' @export
step6_check_gllrm <- function(data, step5, items = NULL, covariates = NULL,
                              score = NULL, alpha = 0.05,
                              adjust_method = NULL, B = 10000) {
  call <- match.call()
  step5_validate_graph_object(step5)
  step6_check_adjust_method(adjust_method)
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be a number between 0 and 1", call. = FALSE)
  }

  if (is.null(items)) {
    items <- step5_graph_nodes_of_type(step5, "item")
  }
  if (is.null(covariates)) {
    covariates <- step5_graph_nodes_of_type(step5, "covariate")
  }

  dataset <- step6_prepare_data(data, items, covariates, score = score)
  dif_tests <- step6_c5_tests(dataset, step5, alpha, adjust_method, B)
  ld_tests <- step6_ld_tests(dataset, items, step5, alpha, adjust_method, B)

  final_graph <- step6_rebuild_graph(step5, dif_tests, ld_tests)
  moralized_graph <- build_moralized_graph(final_graph)

  out <- list(
    dif_tests = dif_tests,
    ld_tests = ld_tests,
    removed_dif = step6_removed_dif(step5, dif_tests),
    removed_ld = step6_removed_ld(step5, ld_tests),
    initial_graph = step5,
    final_graph = final_graph,
    moralized_graph = moralized_graph,
    alpha = alpha,
    adjust_method = adjust_method,
    call = call
  )

  structure(out, class = "gllrm_step6")
}

#' @export
print.gllrm_step6 <- function(x, ...) {
  cat("GLLRM Step 6 model check\n")
  cat("------------------------\n")
  cat("DIF tests: ", nrow(x$dif_tests), "\n", sep = "")
  cat("LD tests: ", nrow(x$ld_tests), "\n", sep = "")
  cat("Removed DIF edges: ", nrow(x$removed_dif), "\n", sep = "")
  cat("Removed LD edges: ", nrow(x$removed_ld), "\n", sep = "")
  cat("Final graph edges: ", nrow(x$final_graph$edges), "\n", sep = "")

  invisible(x)
}
