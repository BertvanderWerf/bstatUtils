test_that("basic empirical p-value calculation works", {
  # Simulate data around hypothesis = 1
  set.seed(123)
  x <- rnorm(1000, mean = 1.0, sd = 1.0)

  result <- compute_empirical_pvalue(x, hypothesis = 1.0, alternative = "two.sided")

  expect_s3_class(result, "htest")
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_true(is.character(result$method))
  expect_true(is.character(result$data.name))
})

test_that("one-sided greater test", {
  set.seed(456)
  x <- rnorm(500, mean = 0.5, sd = 1.0)

  result <- compute_empirical_pvalue(x, hypothesis = 0.0, alternative = "greater")

  expect_s3_class(result, "htest")
  expect_match(result$parameter["test_type"], "greater")
  expect_true(result$p.value > 0)
})

test_that("one-sided less test", {
  set.seed(789)
  x <- rnorm(500, mean = -1.0, sd = 1.0)

  result <- compute_empirical_pvalue(x, hypothesis = 0.0, alternative = "less")

  expect_s3_class(result, "htest")
  expect_match(result$parameter["test_type"], "less")
  expect_true(result$p.value > 0)
})

test_that("handles NA values correctly with na_rm=TRUE", {
  x <- c(rnorm(100), NA, NA)

  result <- compute_empirical_pvalue(x, hypothesis = 0, na_rm = TRUE)

  expect_s3_class(result, "htest")
  expect_false(is.na(result$p.value))
})

test_that("stops on NA values if na_rm=FALSE", {
  x <- c(rnorm(50), NA)

  expect_equal(
    compute_empirical_pvalue(x, hypothesis = 0, na_rm = FALSE)$p.value,
    NA
  )
})

test_that("p-value is bounded [0, 1]", {
  set.seed(111)
  x <- rnorm(200)

  result_greater <- compute_empirical_pvalue(x, hypothesis = 10, alternative = "greater")
  result_less <- compute_empirical_pvalue(x, hypothesis = -10, alternative = "less")

  expect_true(result_greater$p.value >= 0 && result_greater$p.value <= 1)
  expect_true(result_less$p.value >= 0 && result_less$p.value <= 1)
})

test_that("approx_median interpolates correctly", {
  x_vals <- c(1, 2, 3, 4, 5)
  ecdf_vals <- c(0.2, 0.4, 0.6, 0.8, 1.0)

  median_val <- approx_median(x_vals, ecdf_vals)

  expect_true(is.numeric(median_val))
  expect_true(median_val >= min(x_vals) && median_val <= max(x_vals))
})

test_that("parameter names match expected structure", {
  set.seed(222)
  x <- rnorm(100)

  result <- compute_empirical_pvalue(x, hypothesis = 0.5, alternative = "two.sided")

  expect_true("median" %in% names(result$parameter))
  expect_true("hypothesis" %in% names(result$parameter))
  expect_true("test_type" %in% names(result$parameter))
})
