#' Check covaraite type
#'
#' @param x A covariate vector
#' @param name name of the covariate, as a string. (only used in case of error)
#'
#' @returns If covariate is numeric, returns origional covariate. If covariate
#' is a factor, it will be converted to numeric starting from 0. If neither,
#' the function stops and returns cause, that covariate is neither numeric
#' nor a factor
#'
#' @keywords internal
#'
#' @examples
#' check_covariate(c(20,22,45), "age")
#'
#' check_covariate(factor(c("f", "f", "m")), "sex")
#'
check_covariate <- function(x, name) {

  if (is.numeric(x)) {
    return(x)
  }

  if (is.factor(x)) {
    return(as.numeric(x) - 1)
  }

  if (is.character(x)) {
    return(as.numeric(factor(x)) - 1)
  }

  stop(
    "Covariate '", name,
    "' must be numeric, factor, or character",
    call. = FALSE
  )
}



#' Total score computer. Computes the total score of each participant, i.e.
#' the row sum of items per participant
#'
#' @param items_df the sub df only containing the items
#'
#' @returns the row sums of the items data frame
#' @keywords internal
#'
#' @examples
#' compute_total_score(data.frame(item1 = c(1,1,1), item2 = c(2,2,2)))
compute_total_score <- function(items_df) {
  return(rowSums(items_df))
}


#' Compute correlation between two variables. Optionally includes the p-value
#' from the correlation test.
#'
#' @param x Numeric vector
#' @param y Numeric vector of same length as x (variable 1)
#' @param method Character string for deciding which correlation method to use.
#' @param include_pvalues Boolean, indicating whether or not the p-value should
#' be included. Default is set to TRUE.
#'
#' @returns A named list with elements:
#' \describe{
#'   \item{correlation}{Estimated correlation coefficient.}
#'   \item{p_value}{P-value for the test of zero correlation, or \code{NA}.}
#' }
#' @keywords internal
#'
#' @examples
#' x <- rnorm(50)
#' y <- x + rnorm(50, sd = 0.1)
#'
#' cor_one_pair(x, y)
#' cor_one_pair(x, y, include_pvalues = FALSE)
cor_one_pair <- function(x, y, method = "pearson", include_pvalues = TRUE) {
  if (include_pvalues) {
    test <- stats::cor.test(x, y, method = method)
    list(
      correlation = unname(test$estimate),
      p_value = test$p.value
    )
  } else {
    list(
      correlation = stats::cor(x, y, method = method),
      p_value = NA_real_
    )
  }
}


#' Perform M3 screening for a single covariate
#'
#' Computes correlations between one covariate and the total score, as well as
#' between the covariate and each individual item. This corresponds to the M3
#' consistency requirement in Rasch item screening procedures.
#'
#' @param covariate_name Character string giving the name of the covariate.
#' @param dataset A data frame containing item responses and covariates.
#' @param items Character vector with names of item response variables.
#' @param method Character string specifying the correlation method.
#' @param include_pvalues Logical; if \code{TRUE}, p-values are included.
#'
#' @returns A data frame with one row per tested association, containing:
#' \describe{
#'   \item{covariate}{Name of the covariate.}
#'   \item{target_type}{Either \code{"score"} or \code{"item"}.}
#'   \item{target_name}{Name of the score or item.}
#'   \item{correlation}{Estimated correlation coefficient.}
#'   \item{p_value}{P-value for the correlation test, if requested.}
#'   \item{method}{Correlation method used.}
#' }
#' @keywords internal
#'
#' @examples
#' items <- paste0("pain", 1:5)
#'
#' M3_one_covariate(
#'   covariate_name = "age",
#'   dataset = toy_spadi_pain,
#'   items = items,
#'   method = "pearson",
#'   include_pvalues = TRUE
#' )
M3_one_covariate <- function(covariate_name, dataset, items, method, include_pvalues) {

  X_i <- check_covariate(dataset[[covariate_name]], covariate_name)
  items_df <- dataset[items]
  score <- compute_total_score(items_df)

  results <- vector("list", length(items) + 1)
  idx <- 1

  # score-level association
  res <- cor_one_pair(X_i, score, method, include_pvalues)
  results[[idx]] <- data.frame(
    covariate = covariate_name,
    target_type = "score",
    target_name = "total_score",
    correlation = res$correlation,
    p_value = res$p_value,
    method = method,
    stringsAsFactors = FALSE
  )
  idx <- idx + 1

  # item-level associations
  for (item in items) {
    res <- cor_one_pair(X_i, items_df[[item]], method, include_pvalues)
    results[[idx]] <- data.frame(
      covariate = covariate_name,
      target_type = "item",
      target_name = item,
      correlation = res$correlation,
      p_value = res$p_value,
      method = method,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }

  do.call(rbind, results)
}








