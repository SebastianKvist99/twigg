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
compute_partial_gamma <- function(dataset, Yi, Xj, strata_vars){

  stratas <- interaction(dataset[, strata_vars, drop = FALSE], drop = TRUE)
  splits <- split(dataset, stratas)

  sum.C <- 0
  sum.D <- 0

  for (s in splits){

    if (nrow(s) < 2) next

    con.tab <- table(s[[Yi]], s[[Xj]])

    if (all(dim(con.tab) > 1)) {

      loop.res <- DescTools::ConDisPairs(con.tab)

      sum.C <- sum.C + loop.res$C
      sum.D <- sum.D + loop.res$D
    }
  }

  if ((sum.C + sum.D) == 0) return(NA_real_)

  return((sum.C - sum.D) / (sum.C + sum.D))
}

###################### Compute p-values.  ######################
#' Monte Carlo permutation test for partial gamma
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
partial_gamma_mc_test <- function(dataset, Yi, Xj,
                                  strata_vars,
                                  B = 1000) {

  observed <- compute_partial_gamma(
    dataset,
    Yi,
    Xj,
    strata_vars
  )

  strata <- interaction(
    dataset[, strata_vars, drop = FALSE],
    drop = TRUE
  )

  sim_gamma <- numeric(B)

  for (b in seq_len(B)) {

    df_sim <- dataset

    for (s in levels(strata)) {

      idx <- which(strata == s)

      if (length(idx) > 1) {
        df_sim[idx, Xj] <- sample(dataset[idx, Xj])
      }
    }

    sim_gamma[b] <- compute_partial_gamma(
      df_sim,
      Yi,
      Xj,
      strata_vars
    )
  }

  p_value <- mean(
    abs(sim_gamma) >= abs(observed),
    na.rm = TRUE
  )

  return(list(
    gamma = observed,
    p_value = p_value
  ))
}

#' Conditional Monte Carlo test for DIF using coin
#'
#' @param dataset data.frame
#' @param Yi item name
#' @param Xj covariate name
#' @param strata_vars conditioning variables
#' @param B Monte Carlo iterations
#'
#' @return list with gamma and p_value
#' @keywords internal
partial_gamma_coin_test <- function(dataset,
                                    Yi,
                                    Xj,
                                    strata_vars,
                                    B = 10000) {

  gamma <- compute_partial_gamma(
    dataset,
    Yi,
    Xj,
    strata_vars
  )

  strat_formula <- paste(strata_vars, collapse = " + ")

  form <- stats::as.formula(
    paste(Yi, "~", Xj, "|", strat_formula)
  )

  test <- coin::independence_test(
    form,
    data = dataset,
    distribution = coin::approximate(B = B)
  )

  p_value <- coin::pvalue(test)

  return(list(
    gamma = gamma,
    p_value = p_value
  ))
}

######### ----------- ###########


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

      test <- partial_gamma_coin_test(
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

  return(list(
    remaining_sources = current_sources,
    tests = results
  ))
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
  # dataset$Score <- compute_total_score(dataset[items])

  out <- vector("list", length(source_list))
  names(out) <- names(source_list)

  for (Yi in names(source_list)) {
    ## ---- handle cases where "$SOURCE$item == character(0)"
    if (length(source_list[[Yi]]) == 0) {
      out[[Yi]] <- list(
        remaining_sources = character(0),
        tests = list()
      )
      next
    }
    out[[Yi]] <- step3b_eliminate_sources(
      dataset = dataset,
      Yi = Yi,
      source_set = source_list[[Yi]],
      items = items,
      B = B,
      crit_val = crit_val
    )
  }

  return(out)
}



#### ---- 3.c ---- ####
#' Step 3(c): eliminate spurious DIF items for one covariate
#'
#' @param dataset data.frame
#' @param Xj covariate name
#' @param dif_set candidate items showing DIF relative to Xj
#' @param items character vector of all item names
#' @param B number of Monte Carlo samples
#' @param crit_val significance level
#'
#' @return list with remaining_dif_items and test results
#' @keywords internal
step3c_eliminate_dif_items <- function(dataset, Xj, dif_set,
                                       items,
                                       B = 1000, crit_val = 0.05) {
  dataset$Score <- compute_total_score(dataset[items])
  current_dif <- dif_set
  results <- list()

  repeat {
    old_dif <- current_dif

    for (Yi in current_dif) {
      # Condition on S and other DIF items (not Yi)
      cond_vars <- c("Score", setdiff(current_dif, Yi))

      test <- partial_gamma_coin_test(
        dataset, Yi, Xj,
        strata_vars = cond_vars,
        B = B
      )

      results[[Yi]] <- test

      if (!is.na(test$p_value) && test$p_value > crit_val) {
        current_dif <- setdiff(current_dif, Yi)
      }
    }

    if (identical(old_dif, current_dif)) break
  }

  return(list(
    remaining_dif_items = current_dif,
    tests = results
  ))
}

#' Apply Step 3(c) to all covariates
#'
#' @param dataset data.frame
#' @param dif_list named list of DIF(X_j)
#' @param items character vector of all item names
#' @param B number of Monte Carlo samples
#' @param crit_val significance level
#'
#' @return list of results per covariate
#' @export
run_step3c <- function(dataset, dif_list, items,
                       B = 1000, crit_val = 0.05) {
  dataset$Score <- compute_total_score(dataset[items])

  out <- vector("list", length(dif_list))
  names(out) <- names(dif_list)

  for (Xj in names(dif_list)) {
    if (length(dif_list[[Xj]]) == 0) {
      out[[Xj]] <- list(
        remaining_dif_items = character(0),
        tests = list()
      )
      next
    }
    out[[Xj]] <- step3c_eliminate_dif_items(
      dataset = dataset,
      Xj = Xj,
      dif_set = dif_list[[Xj]],
      items = items,
      B = B,
      crit_val = crit_val
    )
  }

  return(out)
}


#### ---- Combine the two final functions into one ---- ####
#' Combine Step 3(b) and 3(c) results
#'
#' @param step3b_results output from run_step3b
#' @param step3c_results output from run_step3c
#' @param original_source_list original SOURCE list from s.d_list
#' @param original_dif_list original DIF list from s.d_list
#'
#' @return updated SOURCE and DIF lists
#' @export
combine_step3bc <- function(step3b_results, step3c_results,
                            original_source_list, original_dif_list) {

  # Get pairs surviving 3(b): for each item, which sources remain
  pairs_3b <- list()
  for (Yi in names(step3b_results)) {
    for (Xj in step3b_results[[Yi]]$remaining_sources) {
      pairs_3b[[paste(Yi, Xj, sep = ":")]] <- c(Yi, Xj)
    }
  }

  # Get pairs surviving 3(c): for each covariate, which items remain
  pairs_3c <- list()
  for (Xj in names(step3c_results)) {
    for (Yi in step3c_results[[Xj]]$remaining_dif_items) {
      pairs_3c[[paste(Yi, Xj, sep = ":")]] <- c(Yi, Xj)
    }
  }

  # Intersection: pairs that survive both
  surviving_pairs <- intersect(names(pairs_3b), names(pairs_3c))

  # Rebuild SOURCE and DIF lists
  new_source <- lapply(original_source_list, function(x) character(0))
  new_dif <- lapply(original_dif_list, function(x) character(0))

  for (pair_name in surviving_pairs) {
    parts <- strsplit(pair_name, ":")[[1]]
    Yi <- parts[1]
    Xj <- parts[2]
    new_source[[Yi]] <- c(new_source[[Yi]], Xj)
    new_dif[[Xj]] <- c(new_dif[[Xj]], Yi)
  }

  return(list(SOURCE = new_source, DIF = new_dif))
}







