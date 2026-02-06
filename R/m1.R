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
M1 <- function(dataset, items, method = "gamma"){

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
  } else if (method == "pearson"){
    associations <- stats::cor(X, method = "pearson")
  } else if (method == "spearman"){
    associations <- stats::cor(X, method = "spearman")
  } else if (method == "kendall"){
    associations <- stats::cor(X, method = "kendall")
  } else {
    stop("please input a valid method of associations measure")
  }
  # fulfilled <- sum(corr_matrix<=0)==0
  status <- all(associations>0)

  # return(list(correlations = corr_matrix,
  #             status = fulfilled))
  return(list(method = method,
              associations = associations,
              status = status))
}
