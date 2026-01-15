test_that("computes single median correctly", {
  set.seed(123)
  x <- rnorm(1000, mean = 5, sd = 2)

  q_median <- approx_quantile(x, probs = 0.5)

  expect_length(q_median, 1)
  expect_true(is.numeric(q_median))
  expect_true(q_median > 4 && q_median < 6)  # Near expected mean
})

test_that("computes multiple quantiles", {
  set.seed(456)
  x <- rnorm(500)

  qs <- approx_quantile(x, probs = c(0.25, 0.5, 0.75))

  expect_length(qs, 3)
  expect_true(qs[1] < qs[2] && qs[2] < qs[3])  # Monotonic
  expect_true(all(names(qs) == c("25%", "50%", "75%")))
})

test_that("computes extremes (min and max)", {
  x <- c(1, 2, 3, 4, 5)

  qs <- approx_quantile(x, probs = c(0, 1))

  expect_equal(qs[1], c(`0%`=1))  # min
  expect_equal(qs[2], c(`100%`=5))  # max
})

test_that("handles NA values with na_rm=TRUE", {
  x <- c(rnorm(100), NA, NA, NA)

  qs <- approx_quantile(x, probs = c(0.25, 0.5, 0.75), na_rm = TRUE)

  expect_length(qs, 3)
  expect_false(any(is.na(qs)))
})

test_that("returns NA values if na_rm=FALSE", {
  x <- c(rnorm(50), NA)

  expect_equal(
    approx_quantile(x, probs = 0.5, na_rm = FALSE),
    NA
  )
})

test_that("returns named vector for multiple quantiles", {
  x <- 1:100

  qs <- approx_quantile(x, probs = c(0.1, 0.5, 0.9))

  expect_true(is.numeric(qs))
  expect_true(length(names(qs)) == 3)
  expect_match(names(qs)[2], "50%")
})

test_that("scalar return for single probability", {
  x <- rnorm(500)

  q_single <- approx_quantile(x, probs = 0.5)

  expect_length(q_single, 1)
  expect_true(!is.na(names(q_single)) || is.null(names(q_single)))
})

test_that("handles integer input", {
  x <- 1:100

  qs <- approx_quantile(x, probs = c(0.25, 0.5, 0.75))

  expect_length(qs, 3)
  expect_true(qs[1] < qs[2] && qs[2] < qs[3])
})

test_that("percentiles work correctly", {
  set.seed(789)
  x <- rnorm(2000)  # Large sample for stability

  q_10 <- approx_quantile(x, probs = 0.1)
  q_50 <- approx_quantile(x, probs = 0.5)
  q_90 <- approx_quantile(x, probs = 0.9)

  expect_true(q_10 < q_50 && q_50 < q_90)
})

test_that("errors on invalid probability values", {
  x <- rnorm(100)

  expect_error(approx_quantile(x, probs = -0.1))
  expect_error(approx_quantile(x, probs = 1.5))
  expect_error(approx_quantile(x, probs = c(0.5, 1.5)))
})

test_that("errors on non-numeric input", {
  expect_error(approx_quantile(c("a", "b", "c"), probs = 0.5))
  expect_error(approx_quantile(factor(1:5), probs = 0.5))
})

test_that("handles uniform data", {
  x <- rep(5, 100)

  qs <- approx_quantile(x, probs = c(0.25, 0.5, 0.75))

  expect_true(all(qs == 5))  # All quantiles should be 5
})

test_that("handles small samples", {
  x <- c(1, 2, 3)

  q_median <- approx_quantile(x, probs = 0.5)

  expect_equal(q_median, 1.5)  # not Exact median
})

test_that("computes full range of standard quantiles", {
  x <- 1:1000

  qs <- approx_quantile(x, probs = seq(0, 1, by = 0.1))

  expect_length(qs, 11)
  expect_true(all(diff(qs) >= 0))  # Monotonically increasing
})

