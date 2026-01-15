#' Permutation-Based Randomization Test
#'
#' Performs a randomization (permutation) test to assess the statistical significance of
#' a test statistic by repeatedly shuffling group labels and recomputing the statistic.
#' Supports any test function that returns an \code{htest} object.
#'
#' @param y Numeric vector (or factor) of response values. Missing values are handled
#'   according to \code{na_rm}.
#' @param group Factor or grouping variable indicating group membership. Must have the same
#'   length as \code{y}.
#' @param test_func Function that computes a test statistic and returns an \code{htest} object.
#'   Must accept arguments \code{(y, group)} and return an object with class \code{htest}.
#'   Examples: \code{\link{one_way_anova_test}}, \code{\link[stats]{bartlett.test}},
#'   \code{\link[stats]{chisq.test}}.
#' @param n_perm Integer; number of permutations to perform. Default: \code{9999}.
#' @param alternative Character; test direction: \code{"two.sided"} (default) or
#'   \code{"one.sided"}. For one-sided, direction is determined by comparing observed
#'   statistic to the median permutation statistic.
#' @param na_rm Logical; if \code{TRUE}, remove missing values before calculation. Default: \code{FALSE}.
#'
#' @returns
#' A list (invisibly) with three elements:
#' \itemize{
#'   \item \code{observed} — \code{htest} object from the original (non-permuted) data
#'   \item \code{randomization_test} — \code{htest} object with permutation test results
#'   \item \code{statistic_perm} — Numeric vector of test statistics from all permutations
#' }
#' The randomization test results are also printed to the console.
#'
#' @details
#' The function:
#' 1. Computes the observed test statistic from the original data.
#' 2. Repeats the test \code{n_perm} times with shuffled group labels.
#' 3. Computes an empirical p-value based on how often the permuted statistic
#'    exceeds the observed value.
#' 4. Returns both the observed and permutation-based test results.
#'
#' Missing or invalid permutation results are handled gracefully, with invalid statistics
#' replaced by \code{NA} and excluded from p-value calculation.
#'
#' @examples
#' # One-way ANOVA permutation test
#' set.seed(123)
#' response <- rnorm(30, mean = 5, sd = 1)
#' groups <- factor(rep(c("A", "B", "C"), 10))
#' result <- randomization_test(response, groups, test_func = one_way_anova_test, n_perm = 999)
#' result$observed
#' result$randomization_test
#'
#' @export
randomization_test <- function(
    y,
    group,
    test_func,
    n_perm = 9999,
    alternative = c("two.sided", "one.sided"),
    na_rm = FALSE
) {
  # Capture input names for reporting
  y_name <- as.character(deparse(substitute(y)))
  group_name <- as.character(deparse(substitute(group)))

  # Validate arguments
  alternative <- match.arg(alternative, c("two.sided", "one.sided"))

  # Validate response vector (numeric or factor)
  if (is.numeric(y)) {
    y <- bstatErr::check_numeric_vector(y, allow_inf = TRUE, allow_na = TRUE)
  } else if (!is.factor(y)) {
    stop("`y` must be numeric or a factor.")
    y <- bstatErr::check_factor(y)
  }

  # Validate group
  group <- bstatErr::check_factor(group, allow_null = TRUE)

  if (length(y) != length(group)) {
    stop("`y` and `group` must have the same length.")
  }

  # Validate test function
  if (!is.function(test_func)) {
    stop("`test_func` must be a function.")
  }

  # Compute observed test statistic
  htest_obs <- test_func(y, group)

  # Verify output is htest class
  if (!inherits(htest_obs, "htest")) {
    stop(
      "`test_func` must return an object of class 'htest'. ",
      "Got class: ", paste(class(htest_obs), collapse = ", ")
    )
  }

  stat_obs <- htest_obs$statistic[1]  # Extract first statistic
  if (!is.numeric(stat_obs) || is.na(stat_obs)) {
    stop("Observed test statistic is non-numeric or NA.")
  }

  # Permutation loop: shuffle group labels and recompute statistic
  stat_perm <- sapply(
    seq_len(n_perm),
    function(i) {
      # Suppress warnings/messages/errors; return NA on failure
      result <- bstatErr::catch_conditions(
        test_func(y, sample(group))$statistic[1],
        default = NA_real_
      )
      result$value
    }
  )

  # Remove NA values from permutation statistics
  stat_perm_valid <- stat_perm[!is.na(stat_perm)]
  if (length(stat_perm_valid) == 0) {
    stop("All permutation test statistics were NA or invalid.")
  }

  # Compute median and determine test direction if one-sided
  stat_perm_median <- approx_quantile(stat_perm_valid, probs = 0.5)

  if (alternative == "one.sided") {
    # Auto-detect direction based on observed vs. permutation median
    alternative_direction <- if (stat_obs >= stat_perm_median) "greater" else "less"
  } else {
    alternative_direction <- "two.sided"
  }

  # Compute empirical p-value
  p_value_obj <- compute_empirical_pvalue(
    stat_perm_valid,
    hypothesis = stat_obs,
    alternative = alternative_direction,
    na_rm = na_rm
  )

  # Construct randomization test result (modify observed result)
  htest_perm <- htest_obs
  htest_perm$statistic <- c("median(permutation)" = stat_perm_median)
  htest_perm$p.value <- p_value_obj$p.value
  htest_perm$method <- paste0(
    "Randomization test (",
    n_perm, " permutations) - ",
    htest_obs$method
  )
  htest_perm$data.name <- paste(y_name, "and sample(", group_name, ")")
  htest_perm$parameter <- c(
    htest_perm$parameter,
    c(
      "n_permutations" = n_perm,
      "n_valid" = length(stat_perm_valid)
    )
  )

  # Update observed data.name
  htest_obs$data.name <- paste(y_name, "and", group_name)

  # Construct output list
  out <- list(
    observed = htest_obs,
    randomization_test = htest_perm,
    statistic_perm = stat_perm
  )

  # Print randomization test result
  print(htest_perm)

  invisible(out)
}
