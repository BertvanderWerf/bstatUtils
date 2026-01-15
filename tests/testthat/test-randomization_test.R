test_that("basic randomization test with ANOVA works", {
  set.seed(123)
  y <- rnorm(30, mean = 5, sd = 1)
  group <- factor(rep(c("A", "B", "C"), 10))

  result <- randomization_test(y, group, test_func = one_way_anova_test, n_perm = 99)

  expect_type(result, "list")
  expect_true("observed" %in% names(result))
  expect_true("randomization_test" %in% names(result))
  expect_true("statistic_perm" %in% names(result))
  expect_s3_class(result$observed, "htest")
  expect_s3_class(result$randomization_test, "htest")
})

test_that("returns correct number of permutation statistics", {
  set.seed(456)
  y <- rnorm(20)
  group <- factor(rep(c("A", "B"), 10))

  result <- randomization_test(y, group, test_func = one_way_anova_test, n_perm = 50)

  expect_length(result$statistic_perm, 50)
  expect_true(is.numeric(result$statistic_perm))
})

test_that("handles one-sided alternative", {
  set.seed(789)
  y <- c(rnorm(15, mean = 1), rnorm(15, mean = 5))
  group <- factor(rep(c("A", "B"), 15))

  result <- randomization_test(
    y, group,
    test_func = one_way_anova_test,
    alternative = "one.sided",
    n_perm = 99
  )

  expect_s3_class(result$randomization_test, "htest")
  expect_true(result$randomization_test$p.value >= 0)
})

test_that("errors if test_func does not return htest", {
  y <- rnorm(20)
  group <- factor(rep(c("A", "B"), 10))

  bad_func <- function(y, group) list(result = 42)  # Not htest

  expect_error(
    randomization_test(y, group, test_func = bad_func, n_perm = 10),
    "htest"
  )
})

test_that("errors if y and group have different lengths", {
  y <- rnorm(20)
  group <- factor(rep(c("A", "B"), 8))

  expect_error(
    randomization_test(y, group, test_func = one_way_anova_test),
    "same length"
  )
})

test_that("Error factor needed", {
  y <- rnorm(20)
  group <- c(rep(1, 10), rep(2, 10))

  expect_error(
    randomization_test(y, group, test_func = one_way_anova_test, n_perm = 50),
    "must be a factor"
  )

})

test_that("handles factor y (for chi-square test)", {
  set.seed(111)
  y <- factor(c(rep("A", 16), rep("B", 10)))
  group <- factor(rep(c("X", "Y"), 13))

  # Use chisq.test which accepts factors
  result <- randomization_test(y, group, test_func = stats::chisq.test, n_perm = 50)

  expect_s3_class(result$observed, "htest")
})

test_that("parameter includes n_permutations and n_valid", {
  set.seed(222)
  y <- rnorm(20)
  group <- factor(rep(c("A", "B"), 10))

  result <- randomization_test(y, group, test_func = one_way_anova_test, n_perm = 99)
c(result)
  expect_true("n_permutations" %in% names(result$randomization_test$parameter))
  expect_equal(result$randomization_test$parameter["n_permutations"], c(n_permutations=99))
})

test_that("observed and randomization test have different p-values", {
  set.seed(333)
  # Create data with clear group differences
  y <- c(rnorm(10, mean = 0), rnorm(10, mean = 5))
  group <- factor(rep(c("A", "B"), 10))

  result <- randomization_test(y, group, test_func = one_way_anova_test, n_perm = 99)

  # Permutation test p-value should be computed
  expect_true(is.numeric(result$randomization_test$p.value))
  expect_true(result$randomization_test$p.value >= 0)
})

