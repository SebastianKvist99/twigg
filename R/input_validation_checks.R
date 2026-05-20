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
  dataset <- as_screening_data_frame(dataset)
  data <- stats::na.omit(dataset)
  if (nrow(data) < n){
    stop(
      "Too few observations in data set to perform meaningful screening",
      call. = TRUE
    )
  }
  data
}

#' Coerce input to data frame for screening functions
#'
#' @param dataset A tabular object.
#' @param arg_name Name of the argument being checked.
#'
#' @returns A base data frame.
#' @keywords internal
as_screening_data_frame <- function(dataset, arg_name = "dataset") {
  if (is.null(dataset)) {
    stop("'", arg_name, "' must be a data frame or a rectangular object ",
         "that can be converted to a data frame", call. = FALSE)
  }

  if (is.null(dim(dataset)) && !is.data.frame(dataset)) {
    stop("'", arg_name, "' must be a data frame or a rectangular object ",
         "that can be converted to a data frame", call. = FALSE)
  }

  if (!is.data.frame(dataset) && is.null(colnames(dataset))) {
    stop("'", arg_name, "' must have non-empty column names", call. = FALSE)
  }

  data <- tryCatch(
    as.data.frame(dataset),
    error = function(e) {
      stop("'", arg_name, "' must be a data frame or a rectangular object ",
           "that can be converted to a data frame", call. = FALSE)
    }
  )

  if (is.null(names(data)) || any(is.na(names(data))) ||
      any(names(data) == "")) {
    stop("'", arg_name, "' must have non-empty column names", call. = FALSE)
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
  if (!is.character(items) || length(items) == 0) {
    stop("'items' must be a non-empty character vector of column names",
         call. = FALSE)
  }
  if(!all(items %in% colnames(df))){
    missing_items <- setdiff(items, colnames(df))
    stop("One or more of the given item names are not in the data frame: ",
         paste(missing_items, collapse = ", "), call. = FALSE)
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
  if (!is.character(covariates) || length(covariates) == 0) {
    stop("'covariates' must be a non-empty character vector of column names",
         call. = FALSE)
  }
  if(!all(covariates %in% colnames(df))){
    missing_covariates <- setdiff(covariates, colnames(df))
    stop("One or more of the given covariate names are not in the data frame: ",
         paste(missing_covariates, collapse = ", "), call. = FALSE)
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
  non_numeric <- items[!vapply(df[items], is.numeric, logical(1))]
  if(length(non_numeric) > 0){
    stop("One or more items are not numerical: ",
         paste(non_numeric, collapse = ", "), call. = FALSE)
  }
}




#' convert data set to correct data type
#'
#' @param dataset a dataset, which can be a matrix, data frame of tibble like structure.
#'
#' @returns the dataset but as a data frame instead of whatever class it was origional
#' @keywords internal
#'
correct.dataset.structure <- function(dataset){
  as_screening_data_frame(dataset)
}
## Hvad gør vi hvis datasættet ikke kan konverteres til en data frame?

