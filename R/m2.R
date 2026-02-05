#' M2 function to test for consistency
#'
#' @param dataset A data set with item scores and covariates
#' @param items A character vector
#'
#' @returns A Boolean indicating whether the data set is consistent or not
#' @export
M2 <- function(dataset, items, method = "gamma") {
  # input validation checks
  are_items_in_df(dataset, items)
  are_items_numeric(dataset, items)

  # extract item matrix
  X <- as.matrix(dataset[, items, drop = FALSE])

  # compute total score
  total_score <- rowSums(X)

  # expand total score into matrix for sweep
  total_mat <- matrix(total_score, nrow = nrow(X), ncol = ncol(X))

  # rest score matrix
  rest <- total_mat - X
  # # Type of associations measure
  # associations_method <- method

  # item–rest associations
  if (method == "gamma"){
    associations <- base::vapply(seq_len(ncol(X)), function(i){
      DescTools::GoodmanKruskalGamma(
        x = X[, i],
        y = rest[, i]
      )}, numeric(1))
  } else if (method == "pearson"){
    associations <- diag(stats::cor(X, rest, method = "pearson"))
  } else if (method == "spearman"){
    associations <- diag(stats::cor(X, rest, method = "spearman"))
  } else if (method == "kendall"){
    associations <- diag(stats::cor(X, rest, method = "kendall"))
  } else {
    stop("please input a valid method of associations measure")
  }

  # M2 condition: all positive
  fulfilled <- all(associations > 0)

  return(list(associations_measure = method,
              associations = associations,
              status = fulfilled))
}

















