# Unit Tests for get_difference_statistics Function
# File: tests/testthat/test-get_difference_statistics.R

# library(testthat)

describe("get_difference_statistics", {

  # ==========================================================================
  # Test 1: Basic Functionality
  # ==========================================================================

  it("works with simple 2x2 case", {
    est <- c(A = 10, B = 12)
    V <- matrix(c(1, 0.2, 0.2, 1.5), 2, 2)

    result <- get_difference_statistics(est, V)

    expect_true(is.list(result))
    expect_true(all(c("difference", "vcov", "P_values", "method") %in% names(result)))
  })

  it("returns correctly structured output", {
    est <- c(A = 10, B = 12, C = 15)
    V <- matrix(c(
      1.0, 0.2, 0.1,
      0.2, 1.5, 0.3,
      0.1, 0.3, 2.0
    ), 3, 3, byrow = TRUE)

    result <- get_difference_statistics(est, V)

    # Check all components present
    expect_equal(length(result), 4)
    expect_true(is.matrix(result$difference))
    expect_true(is.matrix(result$vcov))
    expect_true(is.matrix(result$P_values))
    expect_true(is.character(result$method))
  })

  # ==========================================================================
  # Test 2: Input Validation
  # ==========================================================================

  it("rejects non-numeric estimate", {
    est <- c("A", "B")
    V <- matrix(c(1, 0, 0, 1), 2, 2)
    expect_error(
      get_difference_statistics(est, V),
      "must be a numeric vector"
    )
  })

  it("rejects unnamed estimate", {
    est <- c(10, 12)  # No names
    V <- matrix(c(1, 0, 0, 1), 2, 2)
    expect_error(
      get_difference_statistics(est, V),
      "must be a named vector with unique names"
    )
  })

  it("rejects non-symmetric vcov", {
    est <- c(A = 10, B = 12)
    V <- matrix(c(1, 0.5, 0.2, 1), 2, 2)  # Not symmetric
    expect_error(
      get_difference_statistics(est, V),
      "must be a symmetric matrix"
    )
  })

  it("rejects mismatched dimensions", {
    est <- c(A = 10, B = 12, C = 15)
    V <- matrix(c(1, 0, 0, 1), 2, 2)  # 3 estimates but 2x2 matrix
    expect_error(
      get_difference_statistics(est, V),
      "length of 'estimate' must equal"
    )
  })

  # ==========================================================================
  # Test 3: Pairwise Differences
  # ==========================================================================

  it("computes pairwise differences correctly", {
    est <- c(A = 10, B = 12, C = 15)
    V <- diag(3)

    result <- get_difference_statistics(est, V)

    # Check specific differences
    expect_equal(result$difference["A", "B"], -2)
    expect_equal(result$difference["B", "A"], 2)
    expect_equal(result$difference["A", "C"], -5)
    expect_equal(result$difference["C", "A"], 5)
  })

  it("diagonal differences are zero", {
    est <- c(A = 10, B = 12, C = 15)
    V <- diag(3)

    result <- get_difference_statistics(est, V)

    expect_equal(diag(result$difference), c(A=0, B=0, C=0))
  })

  it("difference matrix is antisymmetric", {
    est <- c(A = 10, B = 12, C = 15)
    V <- diag(3)

    result <- get_difference_statistics(est, V)

    # Check diff[i,j] = -diff[j,i]
    expect_equal(result$difference["A", "B"], -result$difference["B", "A"])
    expect_equal(result$difference["A", "C"], -result$difference["C", "A"])
  })

  # ==========================================================================
  # Test 4: Variance Calculations
  # ==========================================================================

  it("computes variance of differences correctly", {
    est <- c(A = 10, B = 12)
    # Independent estimates: Var(A-B) = Var(A) + Var(B)
    V <- matrix(c(1, 0, 0, 1.5), 2, 2)

    result <- get_difference_statistics(est, V)

    # Var(A - B) = 1 + 1.5 - 0 - 0 = 2.5
    expect_equal(result$vcov["A", "B"], 2.5)
    expect_equal(result$vcov["B", "A"], 2.5)
  })

  it("accounts for covariance in variance calculation", {
    est <- c(A = 10, B = 12)
    # With covariance: Var(A-B) = Var(A) + Var(B) - 2*Cov(A,B)
    V <- matrix(c(1, 0.5, 0.5, 1), 2, 2)

    result <- get_difference_statistics(est, V)

    # Var(A - B) = 1 + 1 - 0.5 - 0.5 = 1.0
    expect_equal(result$vcov["A", "B"], 1.0)
  })

  # ==========================================================================
  # Test 5: P-values
  # ==========================================================================

  it("diagonal p-values are 1", {
    est <- c(A = 10, B = 12, C = 15)
    V <- diag(3)

    result <- get_difference_statistics(est, V)

    expect_equal(diag(result$P_values), c(A=1, B=1, C=1))
  })

  it("p-values are symmetric", {
    est <- c(A = 10, B = 12, C = 15)
    V <- diag(3)

    result <- get_difference_statistics(est, V)

    # P[i,j] should equal P[j,i]
    expect_equal(result$P_values["A", "B"], result$P_values["B", "A"])
    expect_equal(result$P_values["A", "C"], result$P_values["C", "A"])
  })

  it("uses normal distribution by default", {
    est <- c(A = 10, B = 10)  # No difference
    V <- diag(2)

    result <- get_difference_statistics(est, V)

    # No difference should give p-value = 1
    expect_equal(result$P_values["A", "B"], 1)
  })

  it("uses t-distribution when df provided", {
    est <- c(A = 10, B = 12)
    V <- diag(2)

    result_z <- get_difference_statistics(est, V, df = NULL)
    result_t <- get_difference_statistics(est, V, df = 5)

    # T-distribution has heavier tails, so p-values should be larger
    expect_true(result_t$P_values["A", "B"] >= result_z$P_values["A", "B"])
  })

  # ==========================================================================
  # Test 6: Multiple Testing Correction
  # ==========================================================================

  it("applies no correction by default", {
    est <- c(A = 10, B = 12, C = 15)
    V <- diag(3)

    result <- get_difference_statistics(est, V)

    expect_equal(result$method, "none")
  })

  it("applies Bonferroni correction", {
    est <- c(A = 10, B = 12, C = 15)
    V <- diag(3) * 0.01  # Small variance for significant differences

    result_none <- get_difference_statistics(est, V, method = "none")
    result_bonf <- get_difference_statistics(est, V, method = "bonferroni")

    # Bonferroni should increase p-values
    expect_true(result_bonf$P_values["A", "B"] >= result_none$P_values["A", "B"])
    expect_equal(result_bonf$method, "bonferroni")
  })

  it("accepts valid p.adjust methods", {
    est <- c(A = 10, B = 12, C = 15)
    V <- diag(3)

    methods <- c("holm", "hochberg", "BH", "BY", "fdr")

    for (m in methods) {
      result <- get_difference_statistics(est, V, method = m)
      expect_equal(result$method, m)
    }
  })

  # ==========================================================================
  # Test 7: Matrix Dimensions and Names
  # ==========================================================================

  it("preserves estimate names in output matrices", {
    est <- c(First = 10, Second = 12, Third = 15)
    V <- diag(3)

    result <- get_difference_statistics(est, V)

    expect_equal(rownames(result$difference), c("First", "Second", "Third"))
    expect_equal(colnames(result$difference), c("First", "Second", "Third"))
    expect_equal(rownames(result$P_values), c("First", "Second", "Third"))
  })

  it("all output matrices have same dimensions", {
    est <- c(A = 10, B = 12, C = 15, D = 18)
    V <- diag(4)

    result <- get_difference_statistics(est, V)

    expect_equal(dim(result$difference), c(4, 4))
    expect_equal(dim(result$vcov), c(4, 4))
    expect_equal(dim(result$P_values), c(4, 4))
  })

  # ==========================================================================
  # Test 8: Near-Zero Handling
  # ==========================================================================

  it("handles near-zero differences correctly", {
    est <- c(A = 10, B = 10 + 1e-50)  # Nearly identical
    V <- diag(2)

    result <- get_difference_statistics(est, V)

    # Near-zero difference should be set to exact zero
    expect_equal(result$difference["A", "B"], 0)
  })

  # ==========================================================================
  # Test 9: Edge Cases
  # ==========================================================================

  it("works with 2 estimates", {
    est <- c(A = 10, B = 12)
    V <- diag(2)

    result <- get_difference_statistics(est, V)

    expect_equal(dim(result$difference), c(2, 2))
  })

  it("works with many estimates", {
    n <- 10
    est <- setNames(1:n, LETTERS[1:n])
    V <- diag(n)

    result <- get_difference_statistics(est, V)

    expect_equal(dim(result$difference), c(n, n))
  })

  # ==========================================================================
  # Test 10: Practical Examples
  # ==========================================================================

  it("identifies significant differences correctly", {
    # Create scenario with one clearly different estimate
    est <- c(A = 10, B = 10.1, C = 15)  # C is very different
    V <- diag(3) * 0.01  # Small variances

    result <- get_difference_statistics(est, V)

    # C vs A and C vs B should have small p-values
    expect_true(result$P_values["A", "C"] < 0.05)
    expect_true(result$P_values["B", "C"] < 0.05)

    # A vs B should have large p-value (not different)
    expect_true(result$P_values["A", "B"] > 0.05)
  })
})

test_that("difference_statistic class and methods work", {
  est <- c(1, 2, 3)
  vmat <- diag(3) * 0.1
  colnames(vmat) <- row.names(vmat) <- c("A","B", "C")
  ds <- get_difference_statistics(est, vmat)
  expect_s3_class(ds, "difference_statistic")
  expect_true(all(dim(ds$difference) == c(3, 3)))
  expect_true(all(dim(ds$P_values) == c(3, 3)))
  expect_output(print(ds), "Pairwise Difference Statistics")
  expect_output(summary(ds), "Summary of Pairwise Differences")
})
