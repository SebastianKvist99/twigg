#' Local Dependence (LD) Identifier.
#'
#' LD performs an initial screening for local dependecy between items using the
#' \code{iarm::partgam_LD} funciton. The function evaluates whether items are
#' indepndent of each other given their rest score. Note that for each pair of
#' items 2 such tests can be performed. Take the item pair \eqn{(Y_i,Y_j)} then we
#' can test both \eqn{Y_i \bot Y_j \mid S-Y_i} and \eqn{Y_j \bot Y_i \mid S-Y_j}.
#' Hence the screening results in 2 dataframes depending on which item is being
#' subtracted from the total score \eqn{S}.
#'
#' P-values are adjusted for multple testing useing the Benjamin-Hochberg procedure.
#' Item pairs with an adjusted p-value below a specified threshold are flagged as
#' exhibiting LD.
#'
#'
#' @param df A data frame containing item responses and covariates.
#' @param items A character vector of column names corresponding to the items.
#' @param crit_val Numeric. Significance threshold for the adjusted p-values.
#'   Default is set to \code{0.05}.
#'
#' @returns A list with two sub-lists, one containing both data frames produced by
#' \code{iarm::partgam_LD} and one containing the two reduced data frames with only
#' significant pairs of items. When called directly only the two reduced
#' data frames are printed.
#' \describe{
#'   \item{all_LD}{The complete output from \code{iarm::partgam_DIF}.}
#'   \item{reported_LD}{A list with two data frames containing only
#'   item–item pairs with adjusted p-values less than or equal to \code{crit_val}.}
#' }
#'
#' @details
#' This function is intended as a screening tool for detecting potential LD.
#' Flagged item pairs should be investigated further using substantive and
#' methodological considerations.
#'
#' @export
#'
#' @examples
#' data <- twigg::toy_sp_DIF_and_LD
#' data_items <- paste0("pain", 1:5)
#'
#' screen_LD(data, data_items)
screen_LD <- function(df, items, crit_val = 0.05){
  ## ** Check input
  are_items_numeric(df, items)
  are_items_in_df(df, items)
  data <- complete_cases(df, 10)


  ## ** Use our modified version of iarm::partgam_LD to compute partial gammas
  ## ** for the local independece tests
  LD.df <- quiet_partgam_LD(dat.items = data[items],
                            p.adj = "BH")
  ld.df1 <- data.frame(LD.df[[1]])
  ld.df2 <- data.frame(LD.df[[2]])

  ## ** Extract the columns we want to report
  ## Item1, Item2, gamma, raw p-value, adjusted p-value and sig
  # ld.df1 <- subset(ld.df1, select = c(1,2,3,5,6,7))
  # ld.df2 <- subset(ld.df2, select = c(1,2,3,5,6,7))

  ## ** extract the rows with adjusted p-value less than or equal to crit_val
  ld.df1.2report <- subset(ld.df1, ld.df1[ , 6]<= crit_val)
  ld.df2.2report <- subset(ld.df2, ld.df2[ , 6]<= crit_val)

  print(list(ld.df1.2report, ld.df2.2report))
  return(invisible(list(
    "all_LD" = list(ld.df1, ld.df2),
    "reported_LD" = list(ld.df1.2report, ld.df2.2report)
  )))
}


#' Genuine LD Identifier.
#'
#' This function performs step 3a. of the posed item screening.
#' We take the output from the screen_LD function, which flags all LD with a
#' significant partial gamma coefficient. On this LD data we compute the mean
#' partial gamma coefficient for each pair (i.e. the mean of the partial gamma
#' produced by checking the hypothesis \eqn{Y_i \bot Y_j \mid R_i} and
#' \eqn{Y_j \bot Y_i \mid R_j}). The highest absolute mean partial gamma coefficient
#' is then taken at face value and results obtained by conditioning on either
#' \eqn{Y_i} or \eqn{Y_j} are ignored, this procedure is performed literately the
#' data frame from screen_LD is empty.
#'
#' @param screen_LD_output the output from the screen_LD function.
#' @param crit_val The value at which we determine an adjusted pvalue is
#' critical
#'
#' @returns A list with two data frames both containing LD tests. When called
#' directly only the genuine_ld data frame is printed.
#' \describe{
#'  \item{genuine_ld} The reuslting LD after after deletion of spurious LD
#'  \item{all_ld_detected} All LD, including the spurious. This is the LD data
#'  frame that we use input to the algorithm and the genuine_ld data frame is its output.
#' }
#'
#'
#' @export
#'
#' @examples
#' data <- toy_sp_DIF_and_LD
#' items <- paste0("pain", 1:5)
#' screen_ld_output <- screen_LD(data, items)
#' genuine_ld_output <- genuine_LD(screen_ld_output)
#'
genuine_LD <- function(screen_LD_output, crit_val = 0.05){
  reported.ld <- screen_LD_output$reported_LD
  all.ld <- screen_LD_output$all_LD

  ld1 <- all.ld[[1]]
  ld2 <- all.ld[[2]]
  ## ** create pair id in both data frames
  ld1$pair_id <- paste(pmin(ld1$Item1, ld1$Item2),
                       pmax(ld1$Item1, ld1$Item2), sep = "_")
  ld2$pair_id <- paste(pmin(ld2$Item1, ld2$Item2),
                       pmax(ld2$Item1, ld2$Item2), sep = "_")
  ## ** filter significant rows in both df's
  ld1.sig <- subset(ld1, ld1[,6]<= crit_val)
  ld2.sig <- subset(ld2, ld2[,6]<= crit_val)

  ## ** extract pair_id for all pairs in the two significant df's
  all.pair.ids <- union(ld1.sig$pair_id, ld2.sig$pair_id)

  ## ** extract these significant pairs from both df's
  ld1.keep <- ld1[ld1$pair_id %in% all.pair.ids, ]
  ld2.keep <- ld2[ld2$pair_id %in% all.pair.ids, ]

  results <- merge(
    ld1.keep,
    ld2.keep,
    by = "pair_id",
    suffixes = c(".1", ".2"),
    all = TRUE
  )
  results <- subset(results, select = c("Item1.1", "Item2.1", "gamma.1",
                                            "gamma.2"))
  results$mean_gamma <- rowMeans(
    cbind(results$gamma.1, results$gamma.2),
    na.rm = TRUE
  )
  ## ** working LD pool
  working.ld.pool <- results
  ## ** empty df for genuine LD
  genuine.ld <- data.frame()
  ## ** run the described algorithm (section 7.3 in article)
  ## ** untill working.ld.pool is empty
  while (nrow(working.ld.pool)>0){
    ## ** identify the item pair with highest evidence for LD
    max.mean.gamma.pair.id <- which.max(abs(working.ld.pool$mean_gamma))
    max.mean.gamma.pair <- working.ld.pool[max.mean.gamma.pair.id,]
    ## ** store result
    genuine.ld <- rbind(genuine.ld, max.mean.gamma.pair)

    ## ** extract and remove items from other pairs
    item_i <- max.mean.gamma.pair$Item1.1
    item_j <- max.mean.gamma.pair$Item2.1
    ## ** remove any pairs containing one of the above items
    working.ld.pool <- working.ld.pool[
      !(working.ld.pool$Item1.1 %in% c(item_i, item_j) |
          working.ld.pool$Item2.1 %in% c(item_i, item_j)), ]
  }
  ## ** extract only columns we want to report
  genuine.ld <- subset(genuine.ld, select = c("Item1.1", "Item2.1", "mean_gamma"))
  ## ** rename columns appropriately
  names_old2new <- c("Item1.1" = "item1",
                     "Item2.1" = "item2",
                     "mean_gamma" = "mean_gamma")
  genuine.ld <- stats::setNames(genuine.ld, names_old2new)
  ## ** print and return genuine LD df
  results2report <- list("genuine_ld" = genuine.ld,
                         "all_ld_detected" = results)
  print(results2report$genuine_ld)
  return(invisible(results2report))
}


