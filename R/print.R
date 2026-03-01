#' print item screening, simply controls what we is printed when we run the
#' "screen_items" function.
#'
#'
#' @keywords internal
#'
print.item_screening <- function(x, ...) {
  cat("Item screening result\n")
  cat("----------------------\n")
  cat("Passed: ", x$passed, "\n", sep = "")
  failed <- if (is.null(x$failed_step) || length(x$failed_step) == 0) {
    "None"
  } else {
    paste(x$failed_step, collapse = ", ")
  }

  cat("Failed step: ", failed, "\n", sep = "")

  invisible(x)
}
