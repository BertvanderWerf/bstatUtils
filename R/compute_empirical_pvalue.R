#' Compute Empirical P-Value from a Numerical Vector
#'
#' Calculates an empirical (simulated) p-value based on a distribution of observed values.
#' Uses linear interpolation to compute quantiles and median, then compares against a hypothesized value.
#' Supports one-sided and two-sided hypothesis tests.
#'
#' @param x Numeric vector of observed values (typically simulation results or sample data).
#'   Missing values (\code{NA}) are handled according to \code{na_rm}.
#' @param hypothesis Numeric scalar; the hypothesized value to test against. Default: \code{1}.
#' @param alternative Character; test direction: \code{"two.sided"} (default),
#'   \code{"less"} (test if x <= hypothesis), or \code{"greater"} (test if x >= hypothesis).
#' @param na_rm Logical; if \code{TRUE}, remove missing values before calculation. Default: \code{TRUE}.
#'
#' @returns
#' An object of class \code{htest}, which is a list with elements:
#' \itemize{
#'   \item \code{statistic} — not applicable, omitted
#'   \item \code{p.value} — computed empirical p-value (0 to 1)
#'   \item \code{method} — description of the test performed
#'   \item \code{data.name} — original variable name from input
#'   \item \code{parameter} — named numeric vector with median, hypothesis value, and test type
#' }
#'
#' @details
#' The function constructs an empirical cumulative distribution function (ECDF) from the input data,
#' then uses linear interpolation (\code{\link[stats]{approx}}) to evaluate quantiles.
#' For two-sided tests, the p-value is calculated using the observed median and the absolute deviance.
#'
#' @examples
#' # One-sided test: P(X >= 1.5)
#' compute_empirical_pvalue(rnorm(1000, mean = 1.2), hypothesis = 1.5, alternative = "greater")
#'
#' # Two-sided test
#' compute_empirical_pvalue(rnorm(500), hypothesis = 0, alternative = "two.sided")
#'
#' # One-sided test: P(X <= 0.5)
#' compute_empirical_pvalue(rnorm(500, sd = 2), hypothesis = 0.5, alternative = "less")
#'
#' @export
compute_empirical_pvalue <- function(
    x,
    hypothesis = 1,
    alternative = c("two.sided", "less", "greater"),
    na_rm = TRUE
) {
  # Capture input name for reporting
  x_name <- as.character(deparse(substitute(x)))

  # Validate arguments
  x <- bstatErr::check_numeric_vector(x, allow_inf = TRUE, allow_na = TRUE)
  hypothesis <- bstatErr::check_numeric(hypothesis, allow_inf = TRUE)
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  na_rm <- bstatErr::check_logical(na_rm)

  # Remove NA if requested
  if (isTRUE(na_rm)) {
    x <- x[!base::is.na(x)]
  }
  # Stop if all values are NA and na_rm = FALSE
  if (length(x) == 0) {
    stop("No valid (non-NA) values in `x` after filtering.")
  }

  if (any(is.na(x))) {
    # Construct output as htest object
    result <- list(
      statistic = NULL,
      p.value = NA,
      method = "No method exist for NA's",
      data.name = x_name,
      parameter = c(
        median = NA,
        hypothesis = hypothesis,
        test_type = alternative
      )
    )

    class(result) <- c("htest", "list")
    return(result)
  }

  # Compute median using linear interpolation
  median_interp <- approx_median(x, na_rm = na_rm)

  # Build empirical cumulative distribution (ECDF) via linear interpolation
  x_unique <- table(x)
  x_vals <- as.numeric(names(x_unique))
  ecdf_vals <- cumsum(x_unique) / sum(x_unique)

  # Calculate p-value based on hypothesis test direction
  if (alternative == "greater") {
    # P(X >= hypothesis)
    p_value <- 1 - stats::approx(
      x_vals, ecdf_vals,
      xout = hypothesis, yleft = 0, yright = 1
    )$y
    method_desc <- sprintf(
      "Empirical p-value for %s >= %s",
      x_name, hypothesis
    )
  } else if (alternative == "less") {
    # P(X <= hypothesis)
    p_value <- stats::approx(
      x_vals, ecdf_vals,
      xout = hypothesis, yleft = 0, yright = 1
    )$y
    method_desc <- sprintf(
      "Empirical p-value for %s <= %s",
      x_name, hypothesis
    )
  } else {
    # Two-sided: P(|X - median| >= |hypothesis - median|)
    deviance <- abs(hypothesis - median_interp)
    upper_bound <- median_interp + deviance
    lower_bound <- median_interp - deviance

    p_left <- stats::approx(
      x_vals, ecdf_vals,
      xout = lower_bound, yleft = 0, yright = 1
    )$y
    p_right <- 1 - stats::approx(
      x_vals, ecdf_vals,
      xout = upper_bound, yleft = 0, yright = 1
    )$y
    p_value <- pmax(0, pmin(1, p_left + p_right))

    method_desc <- sprintf(
      "Empirical two-sided p-value: P(|deviation from median(%s)| >= %.4f)",
      x_name, deviance
    )
  }

  # Ensure p-value is in [0, 1]
  p_value <- pmax(0, pmin(1, p_value))

  # Construct output as htest object
  result <- list(
    statistic = NULL,
    p.value = p_value,
    method = method_desc,
    data.name = x_name,
    parameter = c(
      median = signif(median_interp, 4),
      hypothesis = hypothesis,
      test_type = alternative
    )
  )

  class(result) <- c("htest", "list")
  result
}


