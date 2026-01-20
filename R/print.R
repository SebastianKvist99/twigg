#' @export
#' @method print item_screening
print.item_screening <- function(x, ...) {
  cat("Item screening result\n")
  cat("----------------------\n")
  cat("Passed: ", x$passed, "\n", sep = "")
  cat("Failed step: ",
      if (is.null(x$failed_step)) "None" else x$failed_step,
      "\n",
      sep = "")

  invisible(x)
}
