#' Compute and insert total item score
#'
#' @param data item repose data
#' @param y character vector of item names
#'
#' @returns data frame of item response with one additional column containing the total score
#'
#' @examples
#' df <- data.frame("id" = c(1,2,3,4), "item1" = c(1,1,1,1), "item2" = c(2,2,2,2))
#' y <- c("item1", "item2")
#' compute_and_include_total_score(df, y)
compute_and_include_total_score <- function(data, y){
  item_data <- data[y]
  data_w_score <- dplyr::mutate(item_data, total_score = rowSums(item_data))
  return(data_w_score)
}


#' Title
#'
#' @param data
#'
#' @returns
#' @export
#'
#' @examples
pivot_data_longer <- function(data){
  tidyr::pivot_longer()
}












