#' Title
#'
#' @param dataset An item reposne data set
#' @param y charecter vector
#' @param X charector vector
#' @param corr_method method used for computation of corrolaion
#' @param include_pvalues Boolean to indicate if p-values should be included in the output or not
#'
#' @returns a data frame
#' @export
#'
M3 <- function(dataset, y = vector_with_names_of_items, X = vector_with_names_of_covariates,
               corr_method = "pearson", include_pvalues = TRUE){

  ## Consider only complete cases
  dataset <- na.omit(dataset)
  if (nrow(dataset) < 10){
    stop("Too few complete cases for the analysis to be useful")
  }

  ## Create empty df for results
  M3_output <- data.frame(
    covariate = character(),
    target_type = character(),
    target_name = character(),   # Added this to distinguish items/scores
    correlation = numeric(),
    p_value = numeric(),         # Added p-values
    correlation_method = character(),
    stringsAsFactors = FALSE
  )

  items_responses <- dataset[y]
  covariates <- dataset[X]
  scores <- rowSums(items_responses)

  cat("Testing M3 for", length(X), "covariates against", length(y), "items + total score\n")

  for (covariate_name in X){
    cat("Processing covariate:", covariate_name, "\n")
    X_i <- covariates[[covariate_name]]

    # Convert factor to numeric (0-based)
    if (!is.numeric(X_i)) {
      if (is.factor(X_i)) {
        X_i <- as.numeric(X_i) - 1
      } else {
        warning(paste("Covariate", covariate_name, "is not numeric or factor — skipping"))
        next
      }
    }

    # Check covariate vs total score
    if (include_pvalues) {
      cor_test <- cor.test(X_i, scores, method = corr_method)
      corr <- cor_test$estimate
      p_val <- cor_test$p.value
    } else {
      corr <- cor(X_i, scores, method = corr_method, use = "complete.obs")
      p_val <- NA
    }
    # Append results
    M3_output <- rbind(M3_output,
                       data.frame(
                         covariate = covariate_name,
                         target_type = "score",
                         target_name = "total_score",
                         correlation = corr,
                         p_value = p_val,
                         correlation_method = corr_method,
                         stringsAsFactors = FALSE
                       ))

    # Check covariate vs individual items
    for (item_name in y){
      item_i <- items_responses[[item_name]]

      if (include_pvalues) {
        cor_test <- cor.test(X_i, item_i, method = corr_method)
        corr <- cor_test$estimate
        p_val <- cor_test$p.value
      } else {
        corr <- cor(X_i, item_i, method = corr_method, use = "complete.obs")
        p_val <- NA
      }

      # append resuts
      M3_output <- rbind(M3_output,
                         data.frame(
                           covariate = covariate_name,
                           target_type = "item",
                           target_name = item_name,
                           correlation = corr,
                           p_value = p_val,
                           correlation_method = corr_method,
                           stringsAsFactors = FALSE
                         ))
    }
  }
  # round the correlation to 3 decimals
  M3_output_corr <- M3_output$correlation
  M3_output$correlation <- round(M3_output_corr, digits = 3)

  cat("M3 analysis complete. Results for", nrow(M3_output), "correlations.\n")
  return(M3_output)
}
