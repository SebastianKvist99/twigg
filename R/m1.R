#' M1 test for consistence
#'
#' @param dataset A data set with item scores and covariates
#' @param y A character vector
#'
#' @returns A Boolean indicating whether the dataset is consistent wrt the M1
#' criteria
#' @export
#'
#' @examples
#' df <- data
#' y <- c("item1", "item2", "item3")
#' M1(df, y)
#'
M1 <- function(dataset, y = vector_with_names_of_items){
  corr_matrix <- stats::cor(dataset[y])
  fulfilled <- sum(corr_matrix<=0)==0
  return(fulfilled)
}
