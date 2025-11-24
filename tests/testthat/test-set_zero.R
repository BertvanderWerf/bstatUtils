# Unit Tests for set_zero Function
# File: tests/testthat/test-set_zero.R

# library(testthat)

describe("set_zero", {

  # ==========================================================================
  # Test 1: Basic Vector Operations
  # ==========================================================================

  it("sets very small values to zero", {
    x <- c(1, 1e-50, 2, -1e-45, 3)
    result <- set_zero(x)
    expect_equal(result, c(1, 0, 2, 0, 3))
  })

  it("preserves larger values", {
    x <- c(1, 0.5, 0.001, 1e-10)
    result <- set_zero(x)
    expect_equal(result[1:3], c(1, 0.5, 0.001))
    expect_true(result[4] > 0)  # 1e-10 > threshold
  })

  it("handles negative values correctly", {
    x <- c(-1, -1e-50, -0.001, 1e-50)
    result <- set_zero(x)
    expect_equal(result, c(-1, 0, -0.001, 0))
  })

  # ==========================================================================
  # Test 2: Threshold Behavior
  # ==========================================================================

  it("threshold is 2^-43", {
    threshold <- 2^-43
    expect_true(threshold < 1e-12)
    expect_true(threshold > 1e-14)
  })

  it("values below threshold become zero", {
    threshold <- 2^-43
    x <- c(threshold / 2, threshold * 0.9)
    result <- set_zero(x)
    expect_equal(result, c(0, 0))
  })

  it("values above threshold are preserved", {
    threshold <- 2^-43
    x <- c(threshold * 1.1, threshold * 2)
    result <- set_zero(x)
    expect_true(all(result > 0))
    expect_equal(result, x)
  })

  it("value exactly at threshold becomes zero", {
    threshold <- 2^-43
    x <- threshold
    result <- set_zero(x)
    expect_equal(result, 0)
  })

  # ==========================================================================
  # Test 3: Matrix Operations
  # ==========================================================================

  it("works with matrices", {
    mat <- matrix(c(1, 1e-50, 2, -1e-45), 2, 2)
    result <- set_zero(mat)
    expected <- matrix(c(1, 0, 2, 0), 2, 2)
    expect_equal(result, expected)
  })

  it("preserves matrix dimensions", {
    mat <- matrix(rnorm(12), 3, 4)
    mat[2, 3] <- 1e-50
    result <- set_zero(mat)
    expect_equal(dim(result), c(3, 4))
    expect_equal(result[2, 3], 0)
  })

  it("works with large matrices", {
    mat <- matrix(rnorm(100), 10, 10)
    mat[5, 5] <- 1e-50
    result <- set_zero(mat)
    expect_equal(dim(result), c(10, 10))
    expect_equal(result[5, 5], 0)
  })

  # ==========================================================================
  # Test 4: Edge Cases
  # ==========================================================================

  it("handles exact zero", {
    x <- c(0, 0, 0)
    result <- set_zero(x)
    expect_equal(result, c(0, 0, 0))
  })

  it("handles single value", {
    expect_equal(set_zero(1e-50), 0)
    expect_equal(set_zero(1), 1)
  })

  it("handles empty vector", {
    x <- numeric(0)
    result <- set_zero(x)
    expect_equal(result, numeric(0))
  })

  it("handles NA values", {
    x <- c(1, NA, 1e-50, 2)
    result <- set_zero(x)
    expect_true(is.na(result[2]))
    expect_equal(result[c(1, 3, 4)], c(1, 0, 2))
  })

  it("handles Inf values", {
    x <- c(Inf, 1e-50, -Inf)
    result <- set_zero(x)
    expect_equal(result, c(Inf, 0, -Inf))
  })

  # ==========================================================================
  # Test 5: Practical Use Cases
  # ==========================================================================

  it("cleans up matrix subtraction results", {
    A <- matrix(c(1, 0, 0, 1), 2, 2)
    B <- matrix(c(1, 0, 0, 1), 2, 2)
    diff <- A - B
    result <- set_zero(diff)
    expect_equal(result, matrix(0, 2, 2))
  })

  it("cleans covariance matrix off-diagonals", {
    # Simulate nearly-zero covariances
    cov_mat <- matrix(c(1.0, 1e-50, 1e-50, 1.0), 2, 2)
    result <- set_zero(cov_mat)
    expect_equal(result, matrix(c(1, 0, 0, 1), 2, 2))
  })

  it("preserves meaningful small values", {
    # 1e-10 is above threshold, should be kept
    x <- c(1, 1e-10, 1e-50, 1e-12)
    result <- set_zero(x)
    expect_equal(result[1:2], c(1, 1e-10))
    expect_equal(result[3], 0)
    expect_equal(result[4], 1e-12)
  })

  # ==========================================================================
  # Test 6: Return Type
  # ==========================================================================

  it("returns numeric vector for vector input", {
    x <- c(1, 2, 3)
    result <- set_zero(x)
    expect_true(is.numeric(result))
    expect_true(is.vector(result))
  })

  it("returns matrix for matrix input", {
    mat <- matrix(1:4, 2, 2)
    result <- set_zero(mat)
    expect_true(is.matrix(result))
  })

  # ==========================================================================
  # Test 7: Vectorization
  # ==========================================================================

  it("handles large vectors efficiently", {
    # Test that function works with large vectors
    x <- rep(1e-50, 10000)
    result <- set_zero(x)
    expect_equal(length(result), 10000)
    expect_true(all(result == 0))
  })

  it("mixed values in large vector", {
    x <- c(rep(1, 5000), rep(1e-50, 5000))
    result <- set_zero(x)
    expect_equal(sum(result == 0), 5000)
    expect_equal(sum(result == 1), 5000)
  })

  # ==========================================================================
  # Test 8: Sign Preservation
  # ==========================================================================

  it("handles positive and negative near-zero correctly", {
    x <- c(1e-50, -1e-50, 1e-44, -1e-44)
    result <- set_zero(x)
    # All should become exactly 0 (not negative zero)
    expect_equal(result, c(0, 0, 0, 0))
  })
})
