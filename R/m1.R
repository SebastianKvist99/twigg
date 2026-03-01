#' M1 test for consistency.
#'
#' @param dataset A data set, can be a dataframe or a matrix, with item scores and
#' covariates
#' @param y A character vector with item names
#' @param method A charecter string determining which kind of measure
#' associations we want to use. Default is "gamma" which is Goodman and
#' Kruskal's gamma, other possible values are "pearson", "spearman" and "kendall"
#'
#' @returns A list with the following elements; the associations measure used,
#' the explicit values for the associations measures and finally the result of
#' the test, i.e. a boolean indicating if the dataset passed M1 or not.
#' @export
#'
#' @examples
#' df <- toy_spadi_pain
#' items <- paste0("pain", 1:5)
#' M1(df, items, method = "gamma")
#' M1(df, items, method = "pearson")
#'
M1 <- function(dataset, items, method = "gamma"){

  possible_methods <- c("gamma", "pearson", "spearman", "kendall")
  if (!(method %in% possible_methods)){
    stop("please input a valid method of associations measure", call. = FALSE)
  }

  are_items_in_df(dataset, items)
  are_items_numeric(dataset, items)

  X <- as.matrix(dataset[,items])

  if (method == "gamma"){
    associations <- matrix(data = NA, nrow = ncol(X), ncol = ncol(X),
                      dimnames = list(items, items))
    for (item_i in items){
      for (item_j in items){
        associations[item_i, item_j] <-
          DescTools::GoodmanKruskalGamma(
          x = X[,item_i],
          y = X[,item_j]
        )
      }
    }
  } else {
    associations <- stats::cor(X, method = method)
  }

  status <- all(associations>0)

  return(list(method = method,
              associations = associations,
              status = status))
}
