#' Test score association with one exogenous covariate
#'
#' @param dataset Data frame containing \code{Score} and covariates.
#' @param Xj Character string naming the covariate to test.
#' @param covariates Character vector of the current covariate set.
#' @param B Integer. Number of Monte Carlo samples.
#'
#' @returns A list with gamma, p-value, and conditioning variables.
#' @keywords internal
step4_one_test <- function(dataset, Xj, covariates, B = 10000) {
  cond_vars <- setdiff(covariates, Xj)

  if (length(cond_vars) == 0) {
    dataset$.step4_block <- 1L
    test_cond_vars <- ".step4_block"
  } else {
    test_cond_vars <- cond_vars
  }

  test <- partial_gamma_coin_test(
    dataset = dataset,
    Yi = "Score",
    Xj = Xj,
    strata_vars = test_cond_vars,
    B = B
  )

  test$strata_vars <- cond_vars
  test
}

#' Format Step 4 Monte Carlo p-values
#'
#' @param p Numeric p-values.
#' @param B Integer. Number of Monte Carlo samples.
#'
#' @returns Character vector of formatted p-values.
#' @keywords internal
step4_format_p_value <- function(p, B) {
  p_floor <- 1 / (B + 1)

  vapply(p, function(x) {
    if (is.na(x)) {
      return(NA_character_)
    }

    if (x == 0) {
      return(paste0("< ", format(signif(p_floor, 2), scientific = FALSE)))
    }

    format(signif(x, 4), scientific = FALSE)
  }, character(1))
}

#' Summarise Step 4 test rows for criterion validity
#'
#' @param test_rows Data frame of Step 4 test rows.
#' @param alpha Numeric significance level.
#'
#' @returns Criterion-validity summary data frame.
#' @keywords internal
step4_criterion_summary <- function(test_rows, alpha) {
  data.frame(
    covariate = test_rows$covariate,
    gamma = test_rows$gamma,
    direction = ifelse(
      is.na(test_rows$gamma),
      NA_character_,
      ifelse(test_rows$gamma > 0, "positive",
             ifelse(test_rows$gamma < 0, "negative", "zero"))
    ),
    p_value = test_rows$p_value,
    p_value_label = test_rows$p_value_label,
    adjusted_p_value = test_rows$adjusted_p_value,
    adjusted_p_value_label = test_rows$adjusted_p_value_label,
    conditioned_on = test_rows$conditioned_on,
    supports_criterion_validity = !is.na(test_rows$decision_p_value) &
      test_rows$decision_p_value <= alpha,
    stringsAsFactors = FALSE
  )
}

#' Step 4: structural screening of score-covariate associations
#'
#' Performs Step 4 of the graphical loglinear Rasch model item-screening
#' procedure. Step 4 is a backwards elimination search for exogenous covariates
#' on which the total score depends. For each covariate, the function tests the
#' conditional association between the total score and that covariate given all
#' other currently retained covariates.
#'
#' This is a structural-model screening step, not a DIF or LD test. The retained
#' covariates are candidates for connections to the score/latent trait structure
#' in later graph construction.
#'
#' @param data A data frame containing item responses and exogenous covariates.
#' @param items Character vector naming the item response variables. Used to
#'   compute the total score when \code{score = NULL}.
#' @param covariates Character vector naming exogenous covariates to screen.
#' @param score Optional score. If \code{NULL}, the score is computed as the row
#'   sum of \code{items}. If a character string, it is interpreted as a column in
#'   \code{data}. If numeric, it must have length \code{nrow(data)}.
#' @param alpha Numeric significance level used for backwards elimination.
#' @param adjust_method Optional p-value adjustment method passed to
#'   \code{\link[stats]{p.adjust}} within each elimination pass. If \code{NULL},
#'   raw p-values are used.
#' @param method Character string. Currently only \code{"partial_gamma"} is
#'   supported.
#' @param B Integer. Number of Monte Carlo samples used by the conditional
#'   independence test.
#'
#' @returns An object of class \code{"gllrm_step4"}, a list containing:
#' \describe{
#'   \item{retained_covariates}{Covariates retained after backwards elimination.}
#'   \item{removed_covariates}{Covariates removed as nonsignificant.}
#'   \item{elimination_path}{Data frame describing each removal step.}
#'   \item{tests}{Data frame containing all conditional association tests.}
#'   \item{initial_criterion_validity}{Summary of the initial all-covariates
#'     Step 4 tests.}
#'   \item{criterion_validity}{Summary of retained score-covariate associations
#'     after backwards elimination.}
#'   \item{criterion_validity_comparison}{Initial and final Step 4 summaries
#'     joined by covariate.}
#'   \item{call}{The matched function call.}
#' }
#'
#' @export
#'
#' @examples
#' data <- toy_spadi_pain
#' items <- paste0("pain", 1:5)
#'
#' step4_structure_screen(
#'   data = data,
#'   items = items,
#'   covariates = c("age", "sex"),
#'   B = 100
#' )
step4_structure_screen <- function(data, items, covariates,
                                   score = NULL,
                                   alpha = 0.05,
                                   adjust_method = NULL,
                                   method = "partial_gamma",
                                   B = 10000) {
  call <- match.call()

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame", call. = FALSE)
  }
  if (!is.character(items)) {
    stop("'items' must be a character vector of column names", call. = FALSE)
  }
  if (!is.character(covariates) || length(covariates) == 0) {
    stop("'covariates' must be a non-empty character vector", call. = FALSE)
  }
  if (method != "partial_gamma") {
    stop("Currently only method = 'partial_gamma' is supported", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be a number between 0 and 1", call. = FALSE)
  }
  if (!is.null(adjust_method)) {
    p.adjust.methods <- stats::p.adjust.methods
    if (!(adjust_method %in% p.adjust.methods)) {
      stop("'adjust_method' must be NULL or a valid p.adjust method",
           call. = FALSE)
    }
  }

  are_covaraites_in_df(data, covariates)

  dataset <- data
  if (is.null(score)) {
    are_items_in_df(dataset, items)
    are_items_numeric(dataset, items)
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

  dataset <- dataset[, unique(c("Score", covariates)), drop = FALSE]
  dataset <- complete_cases(dataset, 10)

  current_covariates <- covariates
  removed_covariates <- character(0)
  all_tests <- list()
  elimination_steps <- list()
  iteration <- 1L

  repeat {
    pass_covariates <- current_covariates
    pass_tests <- lapply(pass_covariates, function(Xj) {
      test <- step4_one_test(dataset, Xj, pass_covariates, B = B)
      data.frame(
        iteration = iteration,
        covariate = Xj,
        gamma = test$gamma,
        p_value = unname(test$p_value[1]),
        adjusted_p_value = NA_real_,
        decision_p_value = unname(test$p_value[1]),
        conditioned_on = if (length(test$strata_vars) == 0) {
          "None"
        } else {
          paste(test$strata_vars, collapse = " + ")
        },
        removed_after_pass = FALSE,
        stringsAsFactors = FALSE
      )
    })

    pass_df <- do.call(rbind, pass_tests)
    row.names(pass_df) <- NULL

    if (!is.null(adjust_method)) {
      pass_df$adjusted_p_value <- stats::p.adjust(
        pass_df$p_value,
        method = adjust_method
      )
      pass_df$decision_p_value <- pass_df$adjusted_p_value
    }

    pass_df$p_value_label <- step4_format_p_value(pass_df$p_value, B = B)
    pass_df$adjusted_p_value_label <- step4_format_p_value(
      pass_df$adjusted_p_value,
      B = B
    )
    pass_df$decision_p_value_label <- step4_format_p_value(
      pass_df$decision_p_value,
      B = B
    )

    removable <- which(!is.na(pass_df$decision_p_value) &
                         pass_df$decision_p_value > alpha)

    if (length(removable) == 0) {
      all_tests[[length(all_tests) + 1]] <- pass_df
      break
    }

    remove_idx <- removable[1]
    remove_covariate <- pass_df$covariate[remove_idx]
    pass_df$removed_after_pass[remove_idx] <- TRUE

    all_tests[[length(all_tests) + 1]] <- pass_df
    elimination_steps[[length(elimination_steps) + 1]] <- data.frame(
      iteration = iteration,
      removed_covariate = remove_covariate,
      gamma = pass_df$gamma[remove_idx],
      p_value = pass_df$p_value[remove_idx],
      p_value_label = pass_df$p_value_label[remove_idx],
      adjusted_p_value = pass_df$adjusted_p_value[remove_idx],
      adjusted_p_value_label = pass_df$adjusted_p_value_label[remove_idx],
      decision_p_value = pass_df$decision_p_value[remove_idx],
      decision_p_value_label = pass_df$decision_p_value_label[remove_idx],
      conditioned_on = pass_df$conditioned_on[remove_idx],
      stringsAsFactors = FALSE
    )

    removed_covariates <- c(removed_covariates, remove_covariate)
    current_covariates <- setdiff(current_covariates, remove_covariate)

    if (length(current_covariates) == 0) break

    iteration <- iteration + 1L
  }

  tests <- if (length(all_tests) == 0) {
    data.frame()
  } else {
    do.call(rbind, all_tests)
  }
  row.names(tests) <- NULL

  elimination_path <- if (length(elimination_steps) == 0) {
    data.frame(
      iteration = integer(0),
      removed_covariate = character(0),
      gamma = numeric(0),
      p_value = numeric(0),
      p_value_label = character(0),
      adjusted_p_value = numeric(0),
      adjusted_p_value_label = character(0),
      decision_p_value = numeric(0),
      decision_p_value_label = character(0),
      conditioned_on = character(0)
    )
  } else {
    do.call(rbind, elimination_steps)
  }
  row.names(elimination_path) <- NULL

  final_tests <- tests[tests$iteration == max(tests$iteration), , drop = FALSE]
  final_tests <- final_tests[final_tests$covariate %in% current_covariates, ,
                             drop = FALSE]

  initial_tests <- tests[tests$iteration == 1L, , drop = FALSE]
  initial_criterion_validity <- step4_criterion_summary(initial_tests, alpha)
  criterion_validity <- step4_criterion_summary(final_tests, alpha)

  last_tests <- do.call(rbind, lapply(covariates, function(Xj) {
    rows <- tests[tests$covariate == Xj, , drop = FALSE]
    rows[nrow(rows), , drop = FALSE]
  }))
  row.names(last_tests) <- NULL

  criterion_validity_comparison <- data.frame(
    covariate = covariates,
    initial_gamma = initial_tests$gamma[match(covariates,
                                              initial_tests$covariate)],
    initial_p_value = initial_tests$p_value[match(covariates,
                                                  initial_tests$covariate)],
    initial_p_value_label = initial_tests$p_value_label[match(
      covariates,
      initial_tests$covariate
    )],
    initial_adjusted_p_value = initial_tests$adjusted_p_value[match(
      covariates,
      initial_tests$covariate
    )],
    initial_adjusted_p_value_label = initial_tests$adjusted_p_value_label[
      match(covariates, initial_tests$covariate)
    ],
    initial_conditioned_on = initial_tests$conditioned_on[match(
      covariates,
      initial_tests$covariate
    )],
    final_gamma = last_tests$gamma[match(covariates, last_tests$covariate)],
    final_p_value = last_tests$p_value[match(covariates,
                                             last_tests$covariate)],
    final_p_value_label = last_tests$p_value_label[match(
      covariates,
      last_tests$covariate
    )],
    final_adjusted_p_value = last_tests$adjusted_p_value[match(
      covariates,
      last_tests$covariate
    )],
    final_adjusted_p_value_label = last_tests$adjusted_p_value_label[match(
      covariates,
      last_tests$covariate
    )],
    final_conditioned_on = last_tests$conditioned_on[match(
      covariates,
      last_tests$covariate
    )],
    retained = covariates %in% current_covariates,
    conclusion = ifelse(covariates %in% current_covariates,
                        "Retained", "Removed"),
    stringsAsFactors = FALSE
  )

  out <- list(
    retained_covariates = current_covariates,
    removed_covariates = removed_covariates,
    elimination_path = elimination_path,
    tests = tests,
    initial_criterion_validity = initial_criterion_validity,
    criterion_validity = criterion_validity,
    criterion_validity_comparison = criterion_validity_comparison,
    alpha = alpha,
    adjust_method = adjust_method,
    method = method,
    call = call
  )

  structure(out, class = "gllrm_step4")
}

#' @export
print.gllrm_step4 <- function(x, ...) {
  cat("GLLRM Step 4 structural screen\n")
  cat("--------------------------------\n")
  cat("Method: ", x$method, "\n", sep = "")
  cat("Alpha: ", x$alpha, "\n", sep = "")
  cat("P-value adjustment: ",
      ifelse(is.null(x$adjust_method), "none", x$adjust_method),
      "\n", sep = "")
  cat("Retained covariates: ",
      ifelse(length(x$retained_covariates) == 0,
             "None",
             paste(x$retained_covariates, collapse = ", ")),
      "\n", sep = "")
  cat("Removed covariates: ",
      ifelse(length(x$removed_covariates) == 0,
             "None",
             paste(x$removed_covariates, collapse = ", ")),
      "\n", sep = "")

  if (nrow(x$initial_criterion_validity) > 0) {
    cat("\nInitial criterion-validity tests:\n")
    print(x$initial_criterion_validity, row.names = FALSE)
  }

  if (nrow(x$criterion_validity) > 0) {
    cat("\nFinal retained criterion-validity summary:\n")
    print(x$criterion_validity, row.names = FALSE)
  }

  invisible(x)
}

#' @export
summary.gllrm_step4 <- function(object, ...) {
  out <- list(
    retained_covariates = object$retained_covariates,
    removed_covariates = object$removed_covariates,
    initial_criterion_validity = object$initial_criterion_validity,
    criterion_validity = object$criterion_validity,
    criterion_validity_comparison = object$criterion_validity_comparison,
    elimination_path = object$elimination_path,
    n_iterations = if (nrow(object$tests) == 0) 0L else
      max(object$tests$iteration)
  )

  class(out) <- "summary.gllrm_step4"
  out
}

#' @export
print.summary.gllrm_step4 <- function(x, ...) {
  cat("Summary of GLLRM Step 4 structural screen\n")
  cat("-----------------------------------------\n")
  cat("Iterations: ", x$n_iterations, "\n", sep = "")
  cat("Retained covariates: ",
      ifelse(length(x$retained_covariates) == 0,
             "None",
             paste(x$retained_covariates, collapse = ", ")),
      "\n", sep = "")
  cat("Removed covariates: ",
      ifelse(length(x$removed_covariates) == 0,
             "None",
             paste(x$removed_covariates, collapse = ", ")),
      "\n", sep = "")

  if (nrow(x$elimination_path) > 0) {
    cat("\nElimination path:\n")
    print(x$elimination_path, row.names = FALSE)
  }

  if (nrow(x$initial_criterion_validity) > 0) {
    cat("\nInitial criterion-validity tests:\n")
    print(x$initial_criterion_validity, row.names = FALSE)
  }

  if (nrow(x$criterion_validity) > 0) {
    cat("\nFinal retained criterion-validity summary:\n")
    print(x$criterion_validity, row.names = FALSE)
  }

  invisible(x)
}

#' Tidy a Step 4 structural screen
#'
#' @param x A \code{"gllrm_step4"} object.
#' @param ... Ignored.
#'
#' @returns A data frame containing all Step 4 conditional association tests.
#' @export
tidy.gllrm_step4 <- function(x, ...) {
  x$tests
}
