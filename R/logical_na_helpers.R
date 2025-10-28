#' na_false
#'
#' Treat NA values in a logical vector as FALSE.
#'
#' @param x A logical vector.
#'
#' @return A logical vector where any NA is coerced to FALSE.
#' @export
#' @examples
#' na_false(c(NA, FALSE, TRUE))
na_false <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.logical(x)) stop("Input 'x' must be logical.")
  # Replace NA with FALSE
  x[is.na(x)] <- FALSE
  x
}

#' na_true
#'
#' Treat NA values in a logical vector as TRUE.
#'
#' @param x A logical vector.
#'
#' @return A logical vector where any NA is coerced to TRUE.
#' @export
#' @examples
#' na_true(c(NA, FALSE, TRUE))
na_true <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.logical(x)) stop("Input 'x' must be logical.")
  # Replace NA with TRUE
  x[is.na(x)] <- TRUE
  x
}
