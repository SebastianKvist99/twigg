#' Differential item functioning (DIF) identifier
#'
#' Performs an initial screening for differential item functioning (DIF)
#' using \code{iarm::partgam_DIF}. The function evaluates whether item responses
#' differ systematically across specified covariates.
#'
#' P-values are adjusted for multiple testing using the Benjamini–Hochberg (BH)
#' procedure. Item–covariate pairs with adjusted p-values below a specified
#' threshold are flagged as exhibiting potential DIF.
#'
#' The function returns both the full output from \code{iarm::partgam_DIF} and a
#' reduced data frame containing only the flagged item–covariate pairs. When called
#' directly only the reduced df is printed.
#'
#'
#' @param df A data frame containing item responses and covariates.
#' @param items A character vector of column names corresponding to the items.
#' @param covariates A character vector of column names corresponding to
#'   covariates to be tested for DIF.
#' @param crit_val Numeric. Significance threshold for the adjusted p-values.
#'   Default is set to \code{0.05}.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{full_DIF}{The complete output from \code{iarm::partgam_DIF}.}
#'   \item{reported_DIF}{A data frame containing only item–covariate pairs
#'   with adjusted p-values less than or equal to \code{crit_val}.}
#' }
#'
#' @details
#' This function is intended as a screening tool for detecting potential DIF.
#' Flagged items should be investigated further using substantive and
#' methodological considerations.
#'
#' @export
#'
#' @examples
#' data <- twigg::llraDat1
#' data <- data[,c(paste0("t5.I", 1:5), "groups")]
#' data_items <- paste0("t5.I", 1:5)
#' covariates <- "groups"
#'
#' screen_DIF(data, data_items, covariates)
#'
screen_DIF <- function(df, items, covariates, crit_val = 0.05 #, p_adj = "BH"
                ){
  ## ** Input validation and non-redundancy in dataset
  are_items_numeric(df, items)
  are_items_in_df(df, items)
  are_covaraites_in_df(df, covariates)
  data <- complete_cases(df, 10)

  ## ** use our modified version of iarm::partgam_DIF to determine DIF
  DIF_df <- quiet_partgam_DIF(dat.items = data[items],
                              dat.exo = data[covariates],
                              p.adj = "BH")
  ## ** Extract the columns to report
  # DIF.2report <- subset(DIF_df, select = c(1,2,3,5,6,7))

  ## ** report only item covariate pairs with a critical adjusted p-value
  DIF.2report <- subset(DIF_df, DIF_df[, 6] <= crit_val)

  results <- list("full_DIF" = DIF_df,
                  "reported_DIF" = DIF.2report)

  ## ** Print only the reported DIF, but return the list containing both DIF df's
  print(results$reported_DIF)
  return(invisible(results))
}



#' Genuine DIF identifyier
#'
#'
#'
#'
# genuine_DIF <- function(){
#
# }








