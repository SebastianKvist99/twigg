#' Complete case function
#'
#' @param dataset an item repsosne data set
#' @param n minimum number of observations
#'
#' @returns nothing or stops whatever the functions is called inside of if the if statement is fulfilled.
#' @keywords internal
#'

complete_cases <- function(dataset, n){
  data <- stats::na.omit(dataset)
  if (nrow(data) < n){
    stop(
      "Too few observations in data set to perform meaningful screening",
      call. = TRUE
    )
  }
  data
}

#' Title
#'
#' @param df
#' @param items
#'
#' @returns
#' @keywords internal
are_items_in_df <- function(df, items){
  if(!all(items %in% colnames(df))){
    stop("One or more of the given item names are not in the data frame")
  }
}

#' Title
#'
#' @param df
#' @param covariates
#'
#' @returns
#' @keywords internal
are_covaraites_in_df <- function(df, covariates){
  if(!all(covariates %in% colnames(df))){
    stop("One or more of the given covariate names are not in the data frame")
  }
}

#' Title
#'
#' @param df
#' @param items
#'
#' @returns
#' @keywords internal
are_items_numeric <- function(df, items){
  if(!all(vapply(df[items], is.numeric, logical(1)))){
    stop("One or more items are not numerical")
  }
}


