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
  failed_steps <- x$failed_steps
  if (is.null(failed_steps)) failed_steps <- x$failed_step

  failed <- if (is.null(failed_steps) || length(failed_steps) == 0) {
    "None"
  } else {
    paste(failed_steps, collapse = ", ")
  }

  cat("Failed steps: ", failed, "\n", sep = "")

  invisible(x)
}
