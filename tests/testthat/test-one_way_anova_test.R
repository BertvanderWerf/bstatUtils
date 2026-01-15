test_that("basic ANOVA test works", {
  set.seed(123)
  y <- rnorm(30, mean = 5, sd = 1)
  group <- factor(rep(c("A", "B", "C"), 10))

  result <- one_way_anova_test(y, group)

  expect_s3_class(result, "htest")
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
})

test_that("returns correct parameter structure", {
  y <- c(1, 2, 3, 4, 5, 6)
  group <- factor(c("A", "A", "B", "B", "C", "C"))

  result <- one_way_anova_test(y, group)

  expect_true("df1" %in% names(result$parameter))
  expect_true("df2" %in% names(result$parameter))
  expect_equal(result$parameter["df1"], c(df1=2))  # 3 groups - 1
  expect_equal(result$parameter["df2"], c(df2=3))  # 6 observations - 3 groups
})

test_that("detects significant differences", {
  # Large differences between groups
  y <- c(1, 1, 1, 10, 10, 10)
  group <- factor(c("A", "A", "A", "B", "B", "B"))

  result <- one_way_anova_test(y, group)

  expect_true(result$p.value < 0.05)  # Should be significant
})

test_that("error group must be factor", {
  y <- rnorm(20)
  group <- c(rep(1, 10), rep(2, 10))  # Numeric, not factor

  expect_error(
    result <- one_way_anova_test(y, group),
    "must be a factor"
  )
})

test_that("errors on length mismatch", {
  y <- rnorm(10)
  group <- factor(rep(c("A", "B"), 3))

  expect_error(one_way_anova_test(y, group), "same length")
})

test_that("errors on non-numeric y", {
  y <- c("a", "b", "c")
  group <- factor(c("A", "A", "B"))

  expect_error(one_way_anova_test(y, group), "numeric")
})

test_that("returns htest class with print method", {
  y <- rnorm(20)
  group <- factor(rep(c("A", "B"), 10))

  result <- one_way_anova_test(y, group)

  expect_output(print(result), "Univariate one-way analysis of variance")
})

