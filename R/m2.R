#' M2 function to test for consistency
#'
#' @param dataset A data set with item scores and covariates
#' @param y A character vector
#'
#' @returns A Boolean indicating wheter the data set is consitent or not
#' @export
#'
#' @examples
M2 <- function(dataset, y = vector_with_names_of_items){
  is_consistent <- TRUE

  data <- dataset[y]
  data <- data %>% mutate(total_score = rowSums(data))

  data_long <- data %>% pivot_longer(
    cols = !total_score,
    names_to = "item_name",
    values_to = "item_score"
  )

  data_long <- data_long %>% mutate(rest_score = total_score - item_score)

  correlations <- data_long %>%
    group_by(item_name) %>%
    summarise(
      correlation = cor(item_score, rest_score, method = "pearson"),
      .groups = "drop"
    )

  is_consistent <- is_consistent == (sum(correlations$correlation <= 0) == 0)
  return(is_consistent)
}
