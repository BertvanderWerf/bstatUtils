#' Replace Values in a Vector or Factor
#'
#' Replaces one or more values in a vector or factor, including levels of factors.
#'
#' @param x A vector or factor whose values will be replaced.
#' @param old_values A vector of values to be replaced. Must not contain duplicates.
#' @param new_values A vector of replacement values. Must be the same length as `old_values` or of length 1.
#' @param warn Logical, if TRUE, warns about unmatched old values. Default: TRUE.
#' @param drop_levels Logical, if TRUE and `x` is a factor, other unused levels are dropped after replacement. Default: TRUE.
#'
#' @returns
#' Returns `x` with specified values replaced by new values. If `x` is a factor, factor levels are updated accordingly.
#'
#' @examples
#' replace_values(c(1, 2, 2, 3), c(2, 3), c(99, 42))
#' replace_values(factor(c("a", "b", "b", "c")), c("b", "c"), c("x", "y"))
#' replace_values(letters, "a", "z")
#'
#' @export
replace_values <- function(x, old_values, new_values, warn = TRUE, drop_levels = TRUE) {

  bstatErr::check_logical(warn)
  bstatErr::check_logical(drop_levels)

  # Ensure old_values has unique entries
  stopifnot(length(old_values) == length(unique(old_values)))

  # Ensure both old_values and new_values are non-empty
  stopifnot(length(old_values) > 0 & length(new_values) > 0)

  # If values are factors, convert to character for safe replacement
  if (is.factor(old_values)) old_values <- as.character(old_values)
  if (is.factor(new_values)) new_values <- as.character(new_values)

  n_old <- length(old_values)
  n_new <- length(new_values)

  # Adjust lengths if needed
  if (n_old != n_new) {
    if (n_old == 1) {
      stop('`old_values` length cannot be less than `new_values` length')
    } else if (n_new == 1) {
      new_values <- rep(new_values, n_old)
      n_new <- n_old
    } else {
      stop("`new_values` should be length 1 or the same length as `old_values`")
    }
  }

  # Double-check uniqueness of old_values
  if (length(unique(old_values)) != n_old) {
    counts <- table(old_values, useNA = "ifany")
    dupes <- names(counts)[counts > 1]
    if (length(dupes) == 1) {
      stop("The value ", dupes, " is found multiple times in `old_values`. `old_values` must be unique.")
    } else {
      stop("The values (", paste(dupes, collapse = ", "), ") are found multiple times in `old_values`. `old_values` must be unique.")
    }
  }

  # Handle factor x
  if (is.factor(x)) {
    # Recursively replace levels
    new_levels <- replace_values(levels(x), old_values, new_values, warn = warn)
    levels(x) <- new_levels
    idx <- match(x, old_values, nomatch = 0)
    to_replace <- idx > 0
    idx <- idx[to_replace]

    # Add new levels to the factor if needed
    missing_levels <- !(new_values %in% new_levels)
    if (any(missing_levels) && length(idx) > 0) {
      levels(x) <- c(levels(x), new_values[missing_levels])
    }
    x[to_replace] <- new_values[idx]
    if (drop_levels) {
      x <- droplevels(x)
    }
  } else {
    if (warn) {
      not_found <- old_values[!(old_values %in% x)]
      if (length(not_found) > 0) {
        if (length(not_found) == 1) {
          warning("The value ", not_found, " is not found in `x`.")
        } else {
          warning("The values (", paste(not_found, collapse = ", "), ") are not found in `x`.")
        }
      }
    }
    idx <- match(x, old_values, nomatch = 0)
    to_replace <- idx > 0
    idx <- idx[to_replace]
    x[to_replace] <- new_values[idx]
  }
  x
}
