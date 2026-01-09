#' Run item screening procedure
#'
#' Runs the item screening procedure by sequentially applying consistency checks
#' M1, M2, and M3 as proposed in the item screening framework. The procedure stops
#' early if a screening step fails and reports which step caused the failure.
#'
#' @param dataset A data frame containing item responses and covariates.
#' @param items Character vector giving the names of item response variables.
#' @param covariates Optional character vector giving the names of covariates
#'   to be screened using the M3 criterion.
#' @param corr_method Character string specifying the correlation method used
#'   in M3. Passed to \code{\link[stats]{cor.test}}.
#' @param include_pvalues Logical; if \code{TRUE}, p-values are included in the
#'   M3 output.
#' @param run_M1 Logical; should the M1 screening step be performed?
#' @param run_M2 Logical; should the M2 screening step be performed?
#' @param run_M3 Logical; should the M3 screening step be performed?
#'
#' @returns An object of class \code{"item_screening"}, which is a named list
#'   containing:
#'   \describe{
#'     \item{passed}{Logical; \code{TRUE} if all screening steps passed.}
#'     \item{failed_step}{Character string indicating the step at which screening
#'       failed, or \code{NULL} if all steps passed.}
#'     \item{M1}{Result of the M1 screening step (if performed).}
#'     \item{M2}{Result of the M2 screening step (if performed).}
#'     \item{M3}{Data frame with M3 correlation results (if performed).}
#'   }
#'
#' @export
#'
#' @examples
#' items <- paste0("pain", 1:5)
#'
#' res <- screen_items(
#'   dataset = toy_spadi_pain,
#'   items = items,
#'   covariates = c("age", "sex")
#' )
#'
#' res$passed
#' res$failed_step
screen_items <- function(dataset,
                           items,
                           covariates = NULL,
                           corr_method = "pearson",
                           include_pvalues = TRUE,
                           run_M1 = TRUE,
                           run_M2 = TRUE,
                           run_M3 = TRUE) {

  # ---------------------------------------------------------
  # Input validation (minimal, non-redundant)
  # ---------------------------------------------------------
  if (!is.data.frame(dataset)) {
    stop("'dataset' must be a data.frame", call. = FALSE)
  }

  if (!is.character(items)) {
    stop("'items' must be a character vector of column names", call. = FALSE)
  }

  if (!is.null(covariates) && !is.character(covariates)) {
    stop("'covariates' must be NULL or a character vector", call. = FALSE)
  }

  # ---------------------------------------------------------
  # Ensure complete cases once
  # ---------------------------------------------------------
  dataset <- complete_cases(dataset)

  # ---------------------------------------------------------
  # Run screening steps
  # ---------------------------------------------------------
  results <- list()
  passed <- TRUE
  failed_step <- NULL

  # ---- M1 --------------------------------------------------
  if (run_M1) {
    M1_res <- M1(dataset, items)
    results$M1 <- M1_res

    if (is.logical(M1_res) && !M1_res) {
      passed <- FALSE
      failed_step <- "M1"
      return(structure(
        c(list(passed = FALSE,
               failed_step = failed_step),
          results),
        class = "item_screening"
      ))
    }
  }

  # ---- M2 --------------------------------------------------
  if (run_M2) {
    M2_res <- M2(dataset, items)
    results$M2 <- M2_res

    if (is.logical(M2_res) && !M2_res) {
      passed <- FALSE
      failed_step <- "M2"
      return(structure(
        c(list(passed = FALSE,
               failed_step = failed_step),
          results),
        class = "item_screening"
      ))
    }
  }

  # ---- M3 --------------------------------------------------
  if (run_M3 && !is.null(covariates)) {
    M3_res <- M3(
      dataset = dataset,
      items = items,
      covariates = covariates,
      corr_method = corr_method,
      include_pvalues = include_pvalues
    )

    results$M3 <- M3_res

    if (!M3_pass(M3_res)) {
      passed <- FALSE
      failed_step <- "M3"
      return(structure(
        c(list(passed = FALSE,
               failed_step = failed_step),
          results),
        class = "item_screening"
      ))
    }
  }

  # ---------------------------------------------------------
  # Final output
  # ---------------------------------------------------------
  structure(
    c(list(passed = passed,
           failed_step = failed_step),
      results),
    class = "item_screening"
  )
}
