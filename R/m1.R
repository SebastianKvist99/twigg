#' M1 test for consistence
#'
#' @param dataset A data set with item scores and covariates
#' @param y A character vector with item names
#'
#' @returns A Boolean indicating whether the dataset is consistent wrt the M1
#' criteria
#' @export
#'
#' @examples
#' df <- toy_spadi_pain
#' items <- paste0("pain", 1:5)
#' M1(df, items)
#'
M1 <- function(dataset, items){
  corr_matrix <- stats::cor(dataset[items])
  fulfilled <- sum(corr_matrix<=0)==0
  return(fulfilled)
}
