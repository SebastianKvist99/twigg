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
screen_DIF <- function(dataset, items, covariates, crit_val = 0.05 #, p_adj = "BH"
                ){
  ## ** Input validation and non-redundancy in dataset
  are_items_numeric(dataset, items)
  are_items_in_df(dataset, items)
  are_covaraites_in_df(dataset, covariates)
  data <- complete_cases(dataset, 10)

  ## ** use our modified version of iarm::partgam_DIF to determine DIF
  DIF_dataset <- quiet_partgam_DIF(dat.items = data[items],
                              dat.exo = data[covariates],
                              p.adj = "BH")
  ## ** Extract the columns to report
  # DIF.2report <- subset(DIF_dataset, select = c(1,2,3,5,6,7))

  ## ** report only item covariate pairs with a critical adjusted p-value
  DIF.2report <- subset(DIF_dataset, DIF_dataset[, 6] <= crit_val)
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
#' @param reported_DIF The reported_DIF data frame frame from the output of
#' screen_DIF
#'
#' @returns a list of lists
#' \describe{
#'  \item{SOURCE}{A list with the SOURCE varaibles causing DIF for each item}
#'  \item{DIF}{A list with all the DIF items and the exogenous varibales
#'  potentially causing it}
#' }
#' @internal
#'
#' @examples
S_n_DIF.list <- function(reported_DIF){
  item.var.dataset <- reported_DIF[,1:2]
  ## ** create lists
  source.list <- split(item.var.dataset$Var, item.var.dataset$Item)
  dif.list <- split(item.var.dataset$Item, item.var.dataset$Var)

  ## ** remove duplicates
  source.list <- lapply(source.list, base::unique)
  dif.list <- lapply(dif.list, base::unique)
  return(
    list(
      "SOURCE" = source.list,
      "DIF" = dif.list
    )
  )
}



#' Title
#'
#' @param dataset
#' @param Yi
#' @param Xj
#' @param strata_vars
#'
#' @returns
#' @export
#'
#' @examples
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

#' Permutation function for determining p-values from observed partial gamma values.
#'
#' Using the Monte Carlo method (is it Mc tho? - how does MC work?)
#'
#'
#'
#'
#' @param dataset
#' @param Yi
#' @param Xj
#' @param strata_vars
#' @param B
#'
#' @returns
#' @export
#'
#' @examples
permutation.test <- function(dataset, Yi, Xj, strata_vars, B = 1000){
  ## ** compute and store observed partial gamma
  obs.gam <- compute_partial.gam(dataset, Yi, Xj, strata_vars)

  ## ** define stratification levels for the splits
  stratas <- interaction(dataset[, strata_vars], drop = TRUE)

  permuted <- numeric(B)

  for (b in seq_len(B)){
    dataset_perm <- dataset

    # permute within stratas
    for (s in levels(stratas)) {
      idx <- which(stratas == s)
      if (length(idx) > 1) {
        dataset_perm[idx, Xj] <- sample(dataset[idx, Xj])
      }
    }

    permuted[b] <- compute_partial.gam(dataset_perm, Yi, Xj, strata_vars)
  }

  p_value <- mean(abs(permuted) >= abs(observed), na.rm = TRUE)

  return(list(
    gamma = obs.gam,
    p_value = p_value
  ))
}


#' Title
#'
#' @param dataset
#' @param Yi
#' @param source.list
#' @param all_items
#' @param B
#' @param crit_val
#' @param iterative
#'
#' @returns
#' @export
#'
#' @examples
step3b_eliminate.sources <- function(dataset, Yi, source.list, all_items,
                                     B = 1000, crit_val = 0.05,
                                     iterative = TRUE) {
  ## ** compute score (this is the reason we need "all_items" argument)
  dataset$Score <- rowSums(dataset[all_items])

  current_sources.list <- source.list
  results <- list()

  repeat {
    old_sources <- current_sources.list

    for (Xj in current_sources.list) {

      cond_vars <- c("Score", setdiff(current_sources.list, Xj))

      test <- permutation.test(
        dataset = dataset,
        Yi = Yi,
        Xj = Xj,
        strata_vars = cond_vars,
        B = B
      )

      results[[Xj]] <- test

      if (!is.na(test$p_value) && test$p_value > cirt_val) {
        current_sources <- setdiff(current_sources, Xj)
      }
    }

    if (!iterative || identical(old_sources, current_sources)) break
  }

  list(
    remaining_sources = current_sources.list,
    tests = results
  )
}


#' Title
#'
#' @param dataset
#' @param source.list
#' @param all_items
#' @param B
#' @param crit_val
#' @param iterative
#'
#' @returns
#' @export
#'
#' @examples
step3b.run <- function(dataset, source.list, all_items,
                       B = 1000, crit_val = 0.05,
                       iterative = TRUE) {

  out <- vector("list", length(source.list))
  names(out) <- names(source.list)

  for (Yi in names(source.list)) {
    out[[Yi]] <- step3b_eliminate.sources(
      dataset = dataset,
      Yi = Yi,
      source.list = source.list[[Yi]],
      all_items = all_items,
      B = 1000,
      crit_val = 0.05,
      iterative = TRUE
    )
  }

  return(out)
}


###################################

#' Elimination of exogenous variables from \eqn{SOURCE(Y_i)}.
#'
#' This funciton implements step 3.b of the screening procedure. Here
#' we perform stepwise elimination of spurious sources of DIF for different items.
#' This is done by testing hypotheses of the type
#' \deqn{Y_i \bot X_j \mid S,SOURCE(Y_i)\setminus X_j}. If the hypothesis
#' is accepted for an exogenous variable \eqn{X_j} then \eqn{X_j\in SOURCE(Y_i)}
#' is removed.
#'
#' @param name description
#' @param name description
#'
#'
#' @return description
#'
#'
#' @details
#' Additional details...
#'
#' @export
#'
#' @examples
#'
# source_eliminiation <- function(screen_DIF_output, crit_val = 0.05,
#                                 p.adjust_method = "BH"){
#   dif.rep <-
#
# }








