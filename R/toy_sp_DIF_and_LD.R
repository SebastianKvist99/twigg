#' Toy SPADI pain data with both LD and DIF (!)
#'
#' A simulated toy data set for a pain questionnaire to test our functions on.
#'
#'
#' @format ## toy SPADI pain data
#' A data frame with 250 rows and 7 columns:
#' \describe{
#'   \item{sex}{Gender of participants. Varaible type: factor with 2 levels "M" male and "F" female.}
#'   \item{age}{Age of participants. Numerical variable ranging from 18 to 85.}
#'   \item{pain1}{ordinal varaible taking values from 0 to 10}
#'   \item{pain2}{ordinal varaible taking values from 0 to 10}
#'   \item{pain3}{ordinal varaible taking values from 0 to 10}
#'   \item{pain4}{ordinal varaible taking values from 0 to 10}
#'   \item{pain5}{ordinal varaible taking values from 0 to 10}
#' }
#'
#' @source simulated
#' @usage data(toy_sp_DIF_and_LD)
#' @examples
#' data(toy_sp_DIF_and_LD)
#' summary(toy_sp_DIF_and_LD)
"toy_sp_DIF_and_LD"
