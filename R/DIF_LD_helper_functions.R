#' Quiet version of iarm partgam LD function.
#'
#' This is the exact function that ships with the iarm package, it is called
#' "partgam_LD". The reason we redefine it is not due to any fancyness, but
#' simply because the partgam_LD function automatically prints its results and
#' we want a modified version which does not. Since "verbose = FALSE" is not an
#' option when calling the function we do it this way. Naturally we found the
#' function body by running the line "getAnywhere("partgam_LD")".
#'
#' @param dat.items A data frame or matrix of item responses
#' @param p.adj A string of charecters defining the methods to be
#' used for adjusting p-values
#'
#' @returns A list with two data frames containing the partial gamma coefficients, standard errors, p-values,
#' adjusted p-values, confidence limits for every item pair.
#'
#' @keywords internal
#'
#'
quiet_partgam_LD <- function(dat.items,
                             p.adj = c("BH","holm","hochberg","hommel",
                                       "bonferroni","BY","none")) {

  padj <- match.arg(p.adj)
  score <- apply(dat.items, 1, sum, na.rm = TRUE)
  ok <- stats::complete.cases(dat.items)
  k <- dim(dat.items)[2]

  result <- data.frame(Item1 = character(), Item2 = character(),
                       gamma = double(), se = double(), pvalue = double(),
                       pkorr = double(), sig = character(),
                       lower = double(), upper = double(),
                       stringsAsFactors = FALSE)

  result <- list(result, result)

  for (i in 1:k) {
    for (j in 1:k) {
      if (i != j) {
        rest <- score[ok] - dat.items[ok, j]
        mm <- iarm::partgam(dat.items[ok, i], dat.items[ok, j], rest)

        pvalue <- ifelse(mm[nrow(mm),1] > 0,
                         2 * (1 - stats::pnorm(mm[nrow(mm),1] / mm[nrow(mm),2])),
                         2 * stats::pnorm(mm[nrow(mm),1] / mm[nrow(mm),2]))

        pkorr <- stats::p.adjust(pvalue, method = padj, n = (k * (k - 1)))

        symp <- stats::symnum(pkorr,
                       cutpoints = c(0,0.001,0.01,0.05,0.1,1),
                       symbols = c(" ***"," **"," *"," ."," "))

        if (i < j) {
          result[[1]][nrow(result[[1]]) + 1, ] <-
            c(names(dat.items)[i], names(dat.items)[j],
              mm[nrow(mm),1:2], pvalue, pkorr, symp, mm[nrow(mm),3:4])
        } else {
          result[[2]][nrow(result[[2]]) + 1, ] <-
            c(names(dat.items)[i], names(dat.items)[j],
              mm[nrow(mm),1:2], pvalue, pkorr, symp, mm[nrow(mm),3:4])
        }
      }
    }
  }

  names(result[[1]])[6] <- names(result[[2]])[6] <- paste("padj", padj, sep = ".")

  invisible(result)   # ← keeps behavior but NO printing
}


#' Quiet version of iarm partgam DIF function.
#'
#' Again we needed a function which does exactly what iarm's partgam_DIF function does
#' but without pringitng the result. As "verbose = FALSE" is not an option we have
#' taken the function body and commented out the part that prints the results.
#'
#' @param dat.items A data frame or matrix with reposnses to items
#' @param dat.exo A data frame or matrix with one or more exogenous grouping
#' variables.
#' @param p.adj The method to be used for adjusting p-values, due to multiple testing
#'
#' @returns A data frame with the partial gamma coefficients, standard errors, p-values,
#' adjusted p-values, confidence limits for every pair of item and exogenous varaible.
#'
#' @keywords internal
#'
#'
quiet_partgam_DIF <- function (dat.items, dat.exo, p.adj = c("BH", "holm", "hochberg",
                                        "hommel", "bonferroni", "BY", "none")){
  if (!is.data.frame(dat.exo)) {
    gname <- deparse(substitute(dat.exo))
    dat.exo <- data.frame(dat.exo)
    names(dat.exo) <- gname
  }
  if (is.null(names(dat.items)))
    names(dat.items) <- paste("I", 1:dim(dat.items)[2], sep = "")
  padj <- match.arg(p.adj)
  score <- apply(dat.items, 1, sum, na.rm = T)
  ok <- stats::complete.cases(cbind(dat.items, dat.exo))
  k <- dim(dat.items)[2]
  l <- dim(dat.exo)[2]
  result <- data.frame(Item = character(), Var = character(),
                       gamma = double(), se = double(), pvalue = double(), pkorr = double(),
                       sig = character(), lower = double(), upper = double(),
                       stringsAsFactors = FALSE)
  z <- 1
  for (i in 1:k) {
    for (j in 1:l) {
      mm <- iarm::partgam(dat.items[ok, i], dat.exo[ok, j], score[ok])
      pvalue <- ifelse(mm[dim(mm)[1], 1] > 0, 2 * (1 -
                                                     stats::pnorm(mm[dim(mm)[1], 1]/mm[dim(mm)[1], 2])),
                       2 * (stats::pnorm(mm[dim(mm)[1], 1]/mm[dim(mm)[1], 2])))
      pkorr <- stats::p.adjust(pvalue, method = padj, n = l *
                          k)
      symp <- stats::symnum(pkorr, cutpoints = c(0, 0.001, 0.01,
                                          0.05, 0.1, 1), symbols = c(" ***", " **", " *",
                                                                     " .", " "))
      result[z, ] <- c(names(dat.items)[i], names(dat.exo)[j],
                       mm[dim(mm)[1], 1:2], pvalue, pkorr, symp, mm[dim(mm)[1],
                                                                    3:4])
      z <- z + 1
    }
  }
  names(result)[6] <- paste("padj", padj, sep = ".")
  # if (padj != "none")
  #   print(cbind(result[, 1:2], round(result[, 3:6], digits = 4),
  #               sig = result[, 7], round(result[, 8:9], digits = 4)))
  # else print(cbind(result[, 1:2], round(result[, 3:5], digits = 4),
  #                  sig = result[, 7], round(result[, 8:9], digits = 4)))
  invisible(result)
}
