#' M3 covariate consistency check
#'
#' Performs the M3 screening step of the item screening procedure by assessing
#' monotone associations between external covariates and both the total score
#' and individual item responses. For each covariate, correlations are computed
#' with the total score and with each item separately.
#'
#' The M3 criterion requires that covariates related to the latent trait show
#' consistent directional associations with item responses and the total score.
#'
#' @param dataset A data frame containing item responses and covariates.
#' @param items Character vector giving the names of item response variables
#'   in \code{dataset}.
#' @param covariates Character vector giving the names of covariates in
#'   \code{dataset} to be screened using the M3 criterion.
#' @param corr_method Character string specifying the correlation method to use.
#'   Passed to \code{\link[stats]{cor.test}} (e.g., \code{"pearson"},
#'   \code{"spearman"}).
#' @param include_pvalues Logical; if \code{TRUE}, p-values from correlation tests
#'   are included in the output.
#'
#' @returns A data frame with one row per tested association and the following
#'   columns:
#'   \describe{
#'     \item{covariate}{Name of the covariate.}
#'     \item{target_type}{Either \code{"score"} for the total score or
#'       \code{"item"} for an individual item response.}
#'     \item{target_name}{Name of the score (\code{"total_score"}) or item.}
#'     \item{correlation}{Estimated correlation coefficient (rounded to three
#'       decimal places).}
#'     \item{p_value}{P-value for the correlation test, if requested.}
#'     \item{method}{Correlation method used.}
#'   }
#'
#' @export
#'
#' @examples
#' items <- paste0("pain", 1:5)
#'
#' M3(
#'   dataset = toy_spadi_pain,
#'   items = items,
#'   covariates = c("age", "sex"),
#'   corr_method = "pearson",
#'   include_pvalues = TRUE
#' )
M3 <- function(dataset, items, covariates,
               corr_method = "gamma"){#,
               #include_pvalues = TRUE) {

  dataset <- complete_cases(dataset, 10)

  results <- lapply(
    covariates,
    M3_one_covariate,
    dataset = dataset,
    items = items,
    method = corr_method#,    include_pvalues = include_pvalues
  )

  out <- do.call(rbind, results)
  out$correlation <- round(out$correlation, 3)

  # fulfilled <- all(tapply(sign(out$correlation),
  #                         out$covariate,
  #                         function(x) length(unique(x[x != 0])) <= 1))
  fulfilled <- fulfilled <- tapply(
    seq_len(nrow(out)),
    out$covariate,
    function(idx) check_M3_covariate(out[idx, ])
  )

  list(
    correlations = out,
    status = all(fulfilled, na.rm = TRUE),
    per_covariate = fulfilled
  )

  #return(list(correlations = out, status = fulfilled))
}

