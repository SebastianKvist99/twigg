#' Complete case function
#'
#' @param dataset an item repsosne data set
#'
#' @returns nothing or stops whatever the functions is called inside of if the if statement is fulfilled.
#' @keywords internal
#'

complete_cases <- function(dataset){
  data <- stats::na.omit(dataset)
  if (nrow(data) < 10){
    stop(
      "Too few observations in data set to perform meaningful screening",
      call. = TRUE
    )
  }
  data
}
