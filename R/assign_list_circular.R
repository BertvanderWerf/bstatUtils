#' Assign Values to List with Circular Recycling
#'
#' Creates a named list of length \code{n} from the values provided.
#' If \code{values} is shorter than \code{n}, elements are assigned in a circular/recycled fashion.
#'
#' @param values A vector (or list) of values to assign.
#' @param n Integer; length of the output list.
#' @param names Optional character vector of names for the list elements.
#'
#' @return A list of length \code{n}, optionally named, with input values assigned (circular if shorter).
#' @examples
#' assign_list_circular(c(1, 2), n = 5)
#' assign_list_circular(c("a", "b", "c"), n = 4, names = c("A", "B", "C", "D"))
assign_list_circular <- function(values, n, names = NULL) {
  if (!is.numeric(n) || length(n) != 1 || n < 0 || n != as.integer(n)) {
    stop("Parameter 'n' must be a single non-negative integer.")
  }
  res <- vector("list", n)
  if (!is.null(names)) {
    if (length(names) != n) stop("'names' must be NULL or a character vector of length n.")
    names(res) <- names
  }
  if (is.null(values)) return(res)
  value_list <- as.list(values)
  nval <- length(value_list)
  if (nval == 0 && n > 0) stop("'values' must have at least one element if n > 0.")
  idx <- 1
  for (i in seq_len(n)) {
    res[[i]] <- value_list[[idx]]
    idx <- idx + 1
    if (idx > nval) idx <- 1
  }
  res
}
