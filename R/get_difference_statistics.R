#' Create a difference_statistic S3 object
#'
#' The \code{difference_statistic} class represents the result of all pairwise
#' comparisons between a set of parameter estimates, including pairwise differences,
#' their variances, and multiple-testing adjusted p-values.
#'
#' This class enables easy downstream manipulation, printing, and summarizing of pairwise
#' difference statistics, as well as extension via additional S3 methods (e.g., plotting).
#'
#' @param difference A square numeric matrix of all pairwise differences between parameter estimates.
#' @param vcov       A square numeric matrix of the variances of the differences.
#' @param p_values   A square numeric matrix of p-values for the null hypothesis that each difference is zero.
#' @param method     The multiple testing correction method used (character scalar).
#'
#' @return An object of class \code{"difference_statistic"}.
#'
#' @examples
#' # Create a difference_statistic object from simulated estimates and variances
#' estimates <- c(1.1, 2.3, 3.7)
#' vmat <- matrix(c(0.12, 0.04, 0.03,
#'                  0.04, 0.11, 0.05,
#'                  0.03, 0.05, 0.13), nrow = 3)
#' dstat <- difference_statistic(
#'   difference = outer(estimates, estimates, "-"),
#'   vcov = vmat,
#'   p_values = matrix(runif(9), 3, 3),
#'   method = "none"
#' )
#'
#' # Display
#' print(dstat)
#' summary(dstat)
#'
#' @seealso \code{\link{get_difference_statistics}} which computes pairwise difference statistics.
#'
#' @export
difference_statistic <- function(difference, vcov, p_values, method = "none") {
  structure(
    list(difference = difference,
         vcov = vcov,
         P_values = p_values,
         method = method),
    class = "difference_statistic"
  )
}

#' Compute Pairwise Difference Statistics
#'
#' Calculates pairwise differences between estimates, their variances,
#' test statistics, and p-values. Useful for post-hoc comparisons in
#' statistical models.
#'
#' @details
#' This function performs pairwise comparisons between all estimates by:
#' \enumerate{
#'   \item Computing all pairwise differences: \code{estimate[i] - estimate[j]}
#'   \item Computing variance of each difference using the variance-covariance matrix
#'   \item Computing test statistics (z-scores or t-statistics)
#'   \item Computing two-sided p-values
#'   \item Optionally applying multiple testing correction
#' }
#'
#' **Variance of Differences:**
#' The variance of a difference between two estimates is computed as:
#' \deqn{Var(X_i - X_j) = Var(X_i) + Var(X_j) - 2 \times Cov(X_i, X_j)}
#'
#' **Test Statistic:**
#' For each pairwise difference:
#' \deqn{z = \frac{difference}{\sqrt{Var(difference)}}}
#'
#' If \code{df} is provided, t-statistics are used instead of z-statistics.
#'
#' **Multiple Testing Correction:**
#' When \code{method != 'none'}, p-values are adjusted using the specified
#' method (see \code{\link[stats]{p.adjust}}). Only the lower triangle
#' (unique comparisons) is adjusted, then mirrored to upper triangle.
#'
#' @param estimate numeric vector of parameter estimates. estimates must be a
#'   named vector if vcov has no row or column names. Names are
#'   used for labeling the output matrices.
#' @param vcov Variance-covariance matrix of the estimates. Must be symmetric
#'   with dimensions matching the length of \code{estimate}.
#' @param df Degrees of freedom for t-distribution. If NULL (default),
#'   normal distribution (z-test) is used.
#' @param method Multiple testing correction method. Must be one of:
#'   "none" (default), "holm", "hochberg", "hommel", "bonferroni",
#'   "BH", "BY", "fdr". See \code{\link[stats]{p.adjust}} for details.
#'
#' @return A list with four components:
#' \describe{
#'   \item{difference}{Matrix of pairwise differences (estimate\[i] - estimate\[j])}
#'   \item{vcov}{Matrix of variances for each pairwise difference}
#'   \item{P_values}{Matrix of two-sided p-values (adjusted if method != "none")}
#'   \item{method}{Character string indicating the correction method used}
#' }
#' All matrices have row and column names matching the names of \code{estimate}.
#' Diagonal elements represent comparisons of estimates with themselves.
#'
#' @note
#' **Important considerations:**
#' \itemize{
#'   \item Diagonal elements of difference matrix are always 0
#'   \item Diagonal elements of P_values matrix are always 1
#'   \item The difference matrix is antisymmetric: \code{diff[i,j] = -diff[j,i]}
#'   \item Near-zero values are set to exact zero using \code{set_zero()}
#' }
#'
#' **Multiple Testing:**
#' When comparing k estimates, there are k*(k-1)/2 unique pairwise comparisons.
#' Multiple testing correction accounts for this to control family-wise error
#' rate or false discovery rate.
#'
#' @examples
#' # Example with 3 estimates
#' est <- c(A = 10, B = 12, C = 15)
#' V <- matrix(c(
#'   1.0, 0.2, 0.1,
#'   0.2, 1.5, 0.3,
#'   0.1, 0.3, 2.0
#' ), nrow = 3, byrow = TRUE)
#'
#' # Without multiple testing correction
#' result <- get_difference_statistics(est, V)
#' result$difference  # Pairwise differences
#' result$P_values    # Unadjusted p-values
#'
#' # With Bonferroni correction
#' result_bonf <- get_difference_statistics(est, V, method = "bonferroni")
#' result_bonf$P_values  # Bonferroni-adjusted p-values
#'
#' # With t-distribution (df = 20)
#' result_t <- get_difference_statistics(est, V, df = 20)
#'
#' @seealso
#' \code{\link[stats]{p.adjust}} for multiple testing correction methods
#' \code{\link{set_zero}} for handling near-zero values
#'
#' @export
get_difference_statistics <- function(estimate, vcov, df = NULL, method = 'none') {

  # ============================================================================
  # VALIDATE INPUT ARGUMENTS
  # ============================================================================

  # Check dimensions match
  if (length(estimate) != nrow(vcov) || length(estimate)!=ncol(vcov)) {
    stop(
      "The length of 'estimate' must equal the number of rows and columns of 'vcov'.",
      call. = FALSE
    )
  }

  # Validate variance-covariance matrix
  vcov <- bstatErr::check_numeric_matrix(vcov, must_be_symmetric = TRUE)

  if (is.null(row.names(vcov))) {
    if (is.null(colnames(vcov))) {
      row.names(vcov) <- names(estimate)
      colnames(vcov) <- names(estimate)
    } else {
      row.names(vcov) <- colnames(vcov)
    }
  } else {
    if (is.null(colnames(vcov))) {
      colnames(vcov) <- row.names(vcov)
    }
  }
  if (is.null(names(estimate))) {
    names(estimate) <- row.names(vcov)
  }

  # Validate estimate vector
  estimate <- bstatErr::check_numeric_vector(estimate, must_have_names = TRUE)

  row.names(vcov) <- colnames(vcov) <- names(estimate)

  # Validate degrees of freedom (optional)
  df <- bstatErr::check_numeric(df, allow_null = TRUE)

  # Validate variance-covariance matrix
  vcov <- bstatErr::check_numeric_matrix(vcov, must_be_symmetric = TRUE)

  # Validate method string
  method <- bstatErr::check_string(method)



  # ============================================================================
  # COMPUTE PAIRWISE DIFFERENCES
  # ============================================================================

  # Extract estimate values
  val <- estimate

  # Compute all pairwise differences: val[i] - val[j]
  # Using outer() to create matrix of differences
  diff <- matrix(
    outer(val, val, '-'),
    nrow = length(val),
    ncol = length(val),
    dimnames = list(names(estimate), names(estimate))
  )

  # ============================================================================
  # COMPUTE VARIANCE OF DIFFERENCES
  # ============================================================================

  # # Initialize variance matrix with same dimensions as vcov
  # var_diff <- vcov
  #
  # # Compute variance for each pairwise difference
  # # Var(X_i - X_j) = Var(X_i) + Var(X_j) - Cov(X_i, X_j) - Cov(X_j, X_i)
  # # Since vcov is symmetric: Cov(X_i, X_j) = Cov(X_j, X_i)
  # for (i in 1:nrow(vcov)) {
  #   for (j in 1:ncol(vcov)) {
  #     var_diff[i, j] <- vcov[i, i] + vcov[j, j] - vcov[i, j] - vcov[j, i]
  #   }
  # }

  diag_v <- diag(vcov)
  var_diff <- outer(diag_v, diag_v, "+") - vcov - t(vcov)


  # ============================================================================
  # COMPUTE TEST STATISTICS
  # ============================================================================

  # Compute z-scores or t-statistics: difference / sqrt(variance)
  z <- diff / sqrt(var_diff)

  # Set z to 0 where difference is (numerically) zero
  # This prevents NaN from 0/0 on diagonal
  cond <- set_zero(diff) == 0
  if (any(cond)) {
    z[cond] <- 0
  }

  # Clean up near-zero z-values
  z <- set_zero(z)

  # ============================================================================
  # COMPUTE P-VALUES
  # ============================================================================

  # Compute two-sided p-values
  if (is.null(df)) {
    # Use normal distribution (z-test)
    P <- (1 - pnorm(abs(z))) * 2
  } else {
    # Use t-distribution with specified df
    P <- (1 - pt(abs(z), df)) * 2
  }

  # Diagonal should be 1 (comparing estimate with itself)
  diag(P) <- 1

  # ============================================================================
  # APPLY MULTIPLE TESTING CORRECTION
  # ============================================================================

  if (method != 'none') {
    # Create matrix to hold adjusted p-values
    mat <- P * 0

    # Adjust only lower triangle (unique comparisons)
    # Number of comparisons: n*(n-1)/2
    mat[lower.tri(mat)] <- stats::p.adjust(
      P[lower.tri(P)],
      method = method,
      n = nrow(mat) * (nrow(mat) - 1) / 2
    )

    # Mirror to upper triangle (matrix is symmetric)
    P <- mat + t(mat)

    # Diagonal should remain 1
    diag(P) <- 1
  }

  # Clean up near-zero p-values
  P <- set_zero(P)

  # ============================================================================
  # RETURN RESULTS
  # ============================================================================

  difference_statistic(
    difference = diff,
    vcov = var_diff,
    p_values = P,
    method = method
  )
}

#' Print method for difference_statistic objects
#' @param x A difference_statistic object
#' @param ... ignored
#' @export
print.difference_statistic <- function(x, ...) {
  cat("Pairwise Difference Statistics\n")
  cat("Multiple-test correction:", x$method, "\n\n")
  cat("Differences:\n")
  print(round(x$difference, 4))
  cat("\nP-values:\n")
  print(round(x$P_values, 4))
  invisible(x)
}

#' Summary method for difference_statistic objects
#' @param object A difference_statistic object
#' @param sig.level Significance level for counting significant results
#' @param ... ignored
#' @export
summary.difference_statistic <- function(object, sig.level = 0.05, ...) {
  idx <- lower.tri(object$P_values)
  sig <- sum(object$P_values[idx] < sig.level)
  cat("Summary of Pairwise Differences\n")
  cat("Correction method:", object$method, "\n")
  cat("Significant comparisons (p <", sig.level, "):", sig, "of", sum(idx), "\n")
  rng <- range(object$P_values[idx], na.rm = TRUE)
  cat("P-value range (off-diagonal):", round(rng[1], 4), "to", round(rng[2], 4), "\n")
  invisible(object)
}




