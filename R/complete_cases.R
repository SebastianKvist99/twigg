#' Complete case function
#'
#' @param dataset an item repsosne data set
#'
#' @returns nothing or stops whatever the functions is called inside of if the if statement is fulfilled.
#'
#'
complete_cases <- function(dataset){
  data <- stats::na.omit(dataset)
  if (nrow(data)<10){
    stop("Too few observations in data set ot perform meaningfull screening")
  }
  # return(data)
  invisible(NULL)
}
