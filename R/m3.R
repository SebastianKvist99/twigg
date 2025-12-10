



M3 <- function(dataset, y, X, corr_method = "pearson", include_pvalues = TRUE){
  complete_cases(dataset)

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

  # store item_scores, covariates and total scores in 3 separate variables.
  items_responses <- dataset[y]
  covariates <- dataset[X]
  scores <- rowSums(items_responses)

  cat("Testing M3 for", length(X), "covariates against", length(y), "items + total score\n")



}
