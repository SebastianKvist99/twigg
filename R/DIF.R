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
#' directly only the reduced data frame is printed.
#'
#'
#' @param dataset A data frame containing item responses and covariates.
#' @param items A character vector of column names corresponding to the items.
#' @param covariates A character vector of column names corresponding to
#'   covariates to be tested for DIF.
#' @param crit_val Numeric. Significance threshold for the adjusted p-values.
#'   Default is set to \code{0.05}.
#' @param method method The correction method used to adjust the p-values to
#'  control the false discovery rate. Default is set to \code{"BH"} but can be
#'  set to any of the following character strings
#'  \code{"holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"}.
#' @param number_of_multiple_tests The number of multiple tests performed at once. Must
#'  be at least equal to the number of LD tests performed in the screen LD function. I.e. if we let
#'  \eqn{k} denote the number of items and \eqn{l} the number of covaraites we
#'  are testing for differential item functioning, \code{number_of_multiple_tests}
#'  must be at least \eqn{k*l}. Default is \code{NULL}, and the function itself
#'  determines \eqn{k} and \eqn{l} from the the inputs. Only change this parameter
#'  if you know what you are doing.
#'
#' @returns A list with two elements:
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
screen_DIF <- function(dataset, items, covariates, crit_val = 0.05,
                       number_of_multiple_tests = NULL,
                       method = "BH"
                ){
  ## ** Input validation and non-redundancy in dataset
  are_items_numeric(dataset, items)
  are_items_in_df(dataset, items)
  are_covaraites_in_df(dataset, covariates)
  data <- complete_cases(dataset, 10)

  ## ** Number of multiple tests
  if (is.null(number_of_multiple_tests)){
    number_of_multiple_tests <- length(items) * length(covariates)
  }

  ## ** use our modified version of iarm::partgam_DIF to determine partial gamma
  ## ** coefficients
  DIF_dataset <- quiet_partgam_DIF(dat.items = data[items],
                              dat.exo = data[covariates],
                              p.adj = "BH")
  ## ** adjust p-values via determined correction method
  DIF_dataset$your.adjusted.p <- stats::p.adjust(unlist(DIF_dataset$pvalue),
                                            method = method,
                                            n = number_of_multiple_tests)
  ## ** Extract the columns to report
  # DIF.2report <- subset(DIF_dataset, select = c(1,2,3,5,6,7))

  ## ** report only item covariate pairs with a critical adjusted p-value
  DIF.2report <- subset(DIF_dataset, DIF_dataset$your.adjusted.p <= crit_val)
  ## ** fix row names
  row.names(DIF.2report) <- NULL

  ## ** If nrow(DIF.2report) == 0 should be stated no DIF found druing screening
  if (nrow(DIF.2report) == 0){
    DIF.2report <- "No DIF detected druing initial screening"
  }
  ## ** Store results
  results <- list("full_DIF" = DIF_dataset,
                  "reported_DIF" = DIF.2report)
  ## ** Print only the reported DIF, but return the list containing both DIF dataset's
  print(results$reported_DIF)
  return(invisible(results))
}


#' Create SOURCE and DIF lists
#'
#' We take the output from screen_DIF and create two lists, SOURCE and DIF.
#'
#' @param screen_DIF_output The output from screen_DIF data frame frame from the output of
#' screen_DIF
#' @param items A charecter vector containing the item names in the data set
#' @param covariates A charecter vector containing the covaraite names in the data set
#'
#' @returns a list of lists
#' \describe{
#'  \item{SOURCE}{A list with the SOURCE varaibles causing DIF for each item}
#'  \item{DIF}{A list with all the DIF items and the exogenous varibales
#'  potentially causing it}
#' }
#' @export
#'
s.d_list <- function(screen_DIF_output, items, covariates){
  sig <- screen_DIF_output$reported_DIF

  ## **
  SOURCE <- stats::setNames(vector("list", length(items)), items)
  for (i in items){
    SOURCE[[i]] <- unique(sig$Var[sig$Item == i])
  }

  DIF <- stats::setNames(vector("list", length(covariates)), covariates)
  for (j in covariates){
    DIF[[j]] <- unique(sig$Item[sig$Var == j])
  }

  return(list(SOURCE = SOURCE,
              DIF = DIF))
}



#' Partial gamma coefficient
#'
#' @param dataset A data frame containng the data set
#' @param Yi The item in question for the hypothesis
#' @param Xj The vocaraite in question for the hypothesis
#' @param strata_vars the variables we want to condition on and hence stratify with
#' respect to
#'
#' @returns the partial gamma coefficient corrosponding to the test
#' @keywords internal
#'
compute_partial.gam <- function(dataset, Yi, Xj, strata_vars){

  stratas <- interaction(dataset[, strata_vars], drop = TRUE)
  splits <- split(dataset, strats)

  ## sum of concordant and disconcordant pairs
  sum.C <- 0
  sum.D <- 0

  ## ** loop over splits and update sum.C and sum.D
  for (s in splits){

    ## need at least 2 rows in a split
    if (nrow(s)<2) next

    ## ** create contingency table
    con.tab <- table(s[Yi], s[Xj])

    ## ** consitency check again: need at least 2x2 contigency table
    if (all(dim(con.tab))>1){
      loop.res <- DescTools::ConDisPairs(con.tab)

      loop.C <- loop.res$C
      loop.D <- loop.res$D

      ## ** update sums
      sum.C <- sum.C + loop.C
      sum.D <- sum.D + loop.D
    }
  }
  if (sum.C+sum.D == 0) return(NA_real_)
  return(
    (sum.C-sum.D)/(sum.C+sum.D)
  )
}

#' Monte Carlo test for partial gamma
#'
#' @param dataset a data frame dataset
#' @param Yi item name
#' @param Xj covariate name
#' @param strata_vars conditioning variables
#' @param B number of simulations
#'
#' @return list with partial gamma coefficient and corrospondoing p_value obtianed
#' via the Monte Carlo method.
#' @keywords internal
partial_gamma_mc_test <- function(dataset, Yi, Xj, strata_vars, B = 1000) {

  observed <- compute_partial_gamma(dataset, Yi, Xj, strata_vars)

  strata <- interaction(dataset[, strata_vars], drop = TRUE)

  sim_gamma <- numeric(B)

  for (b in seq_len(B)) {
    df_sim <- dataset

    for (s in levels(strata)) {
      idx <- which(strata == s)
      if (length(idx) > 1) {
        df_sim[idx, Xj] <- sample(dataset[idx, Xj])
      }
    }

    sim_gamma[b] <- compute_partial_gamma(df_sim, Yi, Xj, strata_vars)
  }

  p_value <- mean(abs(sim_gamma) >= abs(observed), na.rm = TRUE)

  list(gamma = observed, p_value = p_value)
}


#' Step 3(b): eliminate spurious DIF sources for one item
#'
#' @param dataset data.frame
#' @param Yi item name
#' @param source_set candidate covariates
#' @param items a charecter vector with the names of items in the dataset
#' @param B number of Monte Carlo samples. Default is set to \eqn{1000}.
#' @param crit_val significance level. Default is set to \eqn{0.05}.
#' @param iterative logical; repeat until stable
#'
#' @return list with remaining_sources and test results
#' @keywords internal
step3b_eliminate_sources <- function(dataset, Yi, source_set,
                                     items,
                                     B = 1000, crit_val = 0.05,
                                     iterative = TRUE) {
  dataset$Score <- compute_total_score(dataset[items])
  current_sources <- source_set
  results <- list()

  repeat {
    old_sources <- current_sources

    for (Xj in current_sources) {

      cond_vars <- c("Score", setdiff(current_sources, Xj))

      test <- partial_gamma_mc_test(
        dataset, Yi, Xj,
        strata_vars = cond_vars,
        B = B
      )

      results[[Xj]] <- test

      if (!is.na(test$p_value) && test$p_value > crit_val) {
        current_sources <- setdiff(current_sources, Xj)
      }
    }

    if (!iterative || identical(old_sources, current_sources)) break
  }

  list(
    remaining_sources = current_sources,
    tests = results
  )
}


#' Apply Step 3(b) to all items
#'
#' @param dataset data.frame
#' @param source_list named list of SOURCE(Y_i)
#' @param items a charecter vector with the names of items in the dataset
#' @param B number of Monte Carlo samples. Default is set to \eqn{1000}.
#' @param crit_val significance level. Default is set to \eqn{0.05}.
#'
#' @return list of results per item
#' @export
run_step3b <- function(dataset, source_list, items,
                       B = 1000, crit_val = 0.05) {
  ## compute total score variabel
  dataset$Score <- compute_total_score(dataset[items])

  out <- vector("list", length(source_list))
  names(out) <- names(source_list)

  for (Yi in names(source_list)) {
    out[[Yi]] <- step3b_eliminate_sources(
      dataset = dataset,
      Yi = Yi,
      source_set = source_list[[Yi]],
      score_var = "Score",
      B = B,
      crit_val = crit_val
    )
  }

  return(out)
}








