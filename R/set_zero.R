#' Set Values Near Zero to Exact Zero
#'
#' Replaces values that are very close to zero (within machine precision
#' tolerance) with exact zero. This prevents issues with floating-point
#' arithmetic where values should be zero but are represented as extremely
#' small non-zero numbers.
#'
#' @details
#' Floating-point arithmetic can produce values that are mathematically zero
#' but are represented as very small numbers (e.g., 1e-16) due to rounding
#' errors. This function identifies such values and replaces them with exact zero.
#'
#' **Threshold Choice:**
#' The threshold \code{2^-43} (approximately 1.136e-13) is chosen because:
#' \itemize{
#'   \item It's well above typical double-precision floating-point errors
#'   \item It's small enough to not affect meaningful small values
#'   \item It corresponds to roughly 13 decimal places of precision
#' }
#'
#' **Use Cases:**
#' \itemize{
#'   \item Cleaning up results from matrix operations
#'   \item Preparing values for conditional tests (e.g., \code{if (x == 0)})
#'   \item Improving numerical stability in iterative algorithms
#'   \item Making output more readable by removing "noise" near zero
#' }
#'
#' @param x Numeric vector or matrix. Values with absolute value ≤ 2^-43
#'   will be set to exactly 0.
#'
#' @return Object of the same type and dimensions as \code{x}, with near-zero
#'   values replaced by exact zero.
#'
#' @note
#' This is a utility function typically used internally in statistical
#' computations. It does not validate inputs for performance reasons, assuming
#' it will be called with numeric data.
#'
#' **Performance:** This function is vectorized and very fast, with negligible
#' overhead even for large matrices.
#'
#' @examples
#' # Simple vector example
#' x <- c(1, 1e-50, 2, -1e-45, 3)
#' set_zero(x)
#' # Returns: c(1, 0, 2, 0, 3)
#'
#' # Matrix from computation that should be zero
#' A <- matrix(c(1, 0, 0, 1), 2, 2)
#' B <- matrix(c(1, 0, 0, 1), 2, 2)
#' diff <- A - B  # Should be zero matrix but may have tiny values
#' set_zero(diff)
#'
#' # Cleaning up covariance matrix
#' cov_matrix <- matrix(c(1.0, 1e-50, 1e-50, 1.0), 2, 2)
#' set_zero(cov_matrix)
#'
#' # Values larger than threshold are preserved
#' x <- c(1e-10, 1e-12, 1e-14)
#' set_zero(x)
#' # Returns: c(1e-10, 1e-12, 0)
#'
#' @export
set_zero <- function(x) {
  # ============================================================================
  # SET NEAR-ZERO VALUES TO EXACT ZERO
  # ============================================================================

  # Define threshold: 2^-43 ≈ 1.136e-13
  # Values with |x| ≤ threshold are considered "numerical zero"
  threshold <- 2^-43

  # Replace all values where absolute value ≤ threshold with exact 0
  # This is vectorized and works for vectors, matrices, and arrays
  x[abs(x) <= threshold] <- 0

  # ============================================================================
  # RETURN CLEANED OBJECT
  # ============================================================================

  # Return object with same structure as input
  x
}
