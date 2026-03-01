#' Complete case function.
#' Checks how many complete cases the passed dataset contains and drops the
#' observations which contains NA.
#'
#' @param dataset A dataset
#' @param n minimum number of observations, default is set to 10
#'
#' @returns nothing or stops whatever the functions is called inside of if the if statement is fulfilled.
#' @keywords internal
#'

complete_cases <- function(dataset, n = 10){
  data <- stats::na.omit(dataset)
  if (nrow(data) < n){
    stop(
      "Too few observations in data set to perform meaningful screening",
      call. = TRUE
    )
  }
  data
}


#' are_items_in_df, does exactly what the title says, it checks whether or not
#' the passed items are in the passed dataframe
#'
#' @param df A dataset.
#' @param items a vector of charecters, which should be the names of the items
#' in the dataset.
#'
#' @returns NULL, if all items are in df, if any item in the items argument is
#' not in the df the function stops and throws an error.
#' @keywords internal
are_items_in_df <- function(df, items){
  if(!all(items %in% colnames(df))){
    stop("One or more of the given item names are not in the data frame", call. = FALSE)
  }
}

#' Are covariates in df, does exatcly what the names says, it checks whether the
#' passed list of covariate names all are actual dataset passed to the funciton.
#'
#' @param df A dataset
#' @param covariates A vector of charecters all corrospnding to covaraite names
#' in the dataset
#'
#' @returns NULL, if all covariates are in df, if any covaraite in the covaraites
#' argument is not in the df the function stops and throws an error.
#' @keywords internal
are_covaraites_in_df <- function(df, covariates){
  if(!all(covariates %in% colnames(df))){
    stop("One or more of the given covariate names are not in the data frame", call. = FALSE)
  }
}

#' Are items numeric, does exactly what the function says, it checks if the items
#' in the passed df are actually numeric.
#'
#' @param df A dataset
#' @param items A vector of charecters, which should be the names of the items
#' in the dataset.
#'
#' @returns NULL, if all items are in fact numeric. If one or more items does not
#' fulfill the requirement the function stops and throws an appropriate error.
#' @keywords internal
are_items_numeric <- function(df, items){
  if(!all(vapply(df[items], is.numeric, logical(1)))){
    stop("One or more items are not numerical", call. = FALSE)
  }
}


