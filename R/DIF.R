#' Title
#'
#' @param dataset
#' @param items
#' @param covariates
#' @param crit_val
#'
#' @returns
#' @export
#'
#' @examples
DIF <- function(df, items, covariates, crit_val = 0.05){

  are_items_numeric(df, items)
  are_items_in_df(df, items)
  are_covaraites_in_df(df, covariates)
  data <- complete_cases(df, 10)

  # create empty df for results
  DIF_df <- data.frame(
    item = character(),
    covariate = character(),
    gamma = numeric(),
    p_adjusted = numeric(),
    stringsAsFactors = FALSE
  )
  for (cov in covariates){
    loop_dif <- iarm::partgam_DIF(data[items], data[cov], p.adj = "BH")
    loop_dif <- loop_dif[Item, Var, gamma, starts_with("padj")]
  }

}
