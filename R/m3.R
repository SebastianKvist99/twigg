#' Title
#'
#' @param dataset
#' @param items
#' @param covariates
#' @param corr_method
#' @param include_pvalues
#'
#' @returns
#' @export
#'
#' @examples
M3 <- function(dataset, items, covariates,
               corr_method = "pearson",
               include_pvalues = TRUE) {

  dataset <- complete_cases(dataset)

  results <- lapply(
    covariates,
    M3_one_covariate,
    dataset = dataset,
    items = items,
    method = corr_method,
    include_pvalues = include_pvalues
  )

  out <- do.call(rbind, results)
  out$correlation <- round(out$correlation, 3)

  out
}

