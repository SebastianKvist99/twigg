#' M2 function to test for consistency
#'
#' @param dataset A data set with item scores and covariates
#' @param items A character vector
#' @param method A string determining which associations method to use. Default is
#' set to gamma, which is Goodman and Kruskal's gamma, other possible methods are
#' "spearman", "pearson" and "kendall".
#'
#' @returns A list with the chosen associations measure, the explicit
#' associations scores and a status indicating whether or not we passed the
#' M2 criteria.
#'
#' @export
#'
#' @examples
#' data <- toy_spadi_pain
#' items <- paste0("pain", 1:5)
#' M2(data, items, method = "gamma")
#'
M2 <- function(dataset, items, method = "gamma") {

  possible_methods <- c("gamma", "pearson", "spearman", "kendall")
  if (!(method %in% possible_methods)){
    stop("please input a valid method of associations measure", call. = FALSE)
  }

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

  # item–rest associations
  if (method == "gamma"){
    associations <- base::vapply(seq_len(ncol(X)), function(i){
      DescTools::GoodmanKruskalGamma(
        x = X[, i],
        y = rest[, i]
      )}, numeric(1))
  } else {
    associations <- diag(stats::cor(X, rest, method = method))
  }

  # M2 condition: all positive
  fulfilled <- all(associations > 0)

  return(list(associations_measure = method,
              associations = associations,
              status = fulfilled))
}

















