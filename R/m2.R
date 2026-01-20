#' M2 function to test for consistency
#'
#' @param dataset A data set with item scores and covariates
#' @param items A character vector
#'
#' @returns A Boolean indicating whether the data set is consistent or not
#' @export
M2 <- function(dataset, items) {
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

  # item–rest correlations
  ## Q to self: are we doing this correctly??
  ## A to self: I do think so yes.
  cors <- diag(stats::cor(X, rest))

  # M2 condition: all positive
  fulfilled <- all(cors > 0)
  #print(cors)
  return(list(correlations = cors,
              status = fulfilled))
}
