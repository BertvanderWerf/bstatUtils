#' Compute Quantiles via Linear Interpolation
#'
#' Calculates one or more quantiles from a numeric vector using linear interpolation.
#' This approach provides smoother, more stable quantile estimates than the standard sample quantile,
#' particularly useful for discrete data, sparse distributions, or when you need quantiles
#' at specific probability levels.
#'
#' @param x Numeric vector of observed values.
#'   Missing values (\code{NA}) are handled according to \code{na_rm}.
#' @param probs Numeric vector of probabilities (0 to 1) at which to compute quantiles.
#'   Default: \code{c(0, 0.25, 0.5, 0.75, 1)}.
#' @param na_rm Logical; if \code{TRUE}, remove missing values before calculation. Default: \code{TRUE}.
#'
#' @returns
#' Numeric vector of quantile values corresponding to \code{probs}.
#' If \code{probs} is length 1, returns a scalar; otherwise returns a vector with names
#' corresponding to probability labels (e.g., \code{"25%"}, \code{"50%"}).
#'
#' @details
#' The function first removes \code{NA} values (if \code{na_rm = TRUE}), then constructs
#' an empirical cumulative distribution function (ECDF) by sorting the data and computing
#' cumulative probabilities. Linear interpolation via \code{\link[stats]{approx}} is used
#' to find the quantile values at the requested probability levels.
#'
#' This is equivalent to using \code{stats::quantile(..., type = 4)}, which uses linear interpolation.
#'
#' @examples
#' # Compute standard quartiles
#' approx_quantile(rnorm(1000), probs = c(0.25, 0.5, 0.75))
#'
#' # Compute a single quantile (median)
#' approx_quantile(rnorm(500), probs = 0.5)
#'
#' # Percentiles
#' approx_quantile(rnorm(100), probs = c(0.1, 0.5, 0.9))
#'
#' @export
approx_quantile <- function(x, probs = c(0, 0.25, 0.5, 0.75, 1), na_rm = TRUE) {
  # Validate input vector
  x <- bstatErr::check_numeric_vector(x, allow_na=TRUE)
  probs <- bstatErr::check_numeric_vector(probs)

  # Validate probability vector on values
  if (!is.numeric(probs) || any(probs < 0 | probs > 1)) {
    stop("`probs` must be a numeric vector with values in [0, 1].")
  }

  # Remove NA if requested
  if (isTRUE(na_rm)) {
    x <- x[!base::is.na(x)]
  }

  # Stop if all values are NA and na_rm = FALSE
  if (length(x) == 0) {
    stop("No valid (non-NA) values in `x` after filtering.")
  }

  if (any(is.na(x))) {
    quantiles <- rep(NA, length(probs))
  } else {

    # Build empirical cumulative distribution (ECDF) via linear interpolation
    x_unique <- table(x)
    x_vals <- as.numeric(names(x_unique))
    ecdf_vals <- cumsum(x_unique) / sum(x_unique)

    if (length(x_unique)==1) {
      # all quantiles have the same value
      quantiles <- rep(x_vals, length(probs))
    } else {
      # Use linear interpolation to find quantiles
      quantiles <- stats::approx(
        ecdf_vals, x_vals,
        xout = probs,
        yleft = min(x_vals),
        yright = max(x_vals)
      )$y
    }
  }

  # Name the output according to probability labels
  if (length(probs) > 1) {
    names(quantiles) <- paste0(format(probs * 100, trim=TRUE), "%")
  }

  quantiles
}


#' Compute Median via Linear Interpolation
#'
#' Calculates the median of a distribution using linear interpolation over quantiles,
#' which is smoother and more stable than the standard sample median, particularly
#' for discrete or sparse data.
#'
#' @param x Numeric vecto.
#' @param na_rm Logical; if \code{TRUE}, propagate any NA handling from parent context. Default: \code{TRUE}.
#'
#' @returns
#' Numeric scalar representing the interpolated median (0.5 quantile).
#'
#' @examples
#' # Example: discrete distribution
#' x_vals <- c(1, 2, 3, 4, 5)
#' ecdf_vals <- c(0.1, 0.3, 0.5, 0.8, 1.0)
#' approx_median(x_vals, ecdf_vals)
#'
#' @export
approx_median <- function(x, na_rm = TRUE) {
  approx_quantile(x, 0.5, na_rm=na_rm)
}
