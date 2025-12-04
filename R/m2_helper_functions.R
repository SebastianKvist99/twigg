#' Compute and insert total item score
#'
#' @param data item repose data
#' @param y character vector of item names
#'
#' @returns data frame of item response with one additional column containing the total score
#'
#' @examples
#' df <- toy_spadi_pain
#' items <- paste0("item", 1:5)
#'
#' ## note to self: should we include a way to handle NA's?
compute_and_include_total_score <- function(data, y){
  item_data <- data[y]
  data_w_score <- dplyr::mutate(item_data, total_score = rowSums(item_data))
  return(data_w_score)
}














