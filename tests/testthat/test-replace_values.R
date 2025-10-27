test_that("basic replacement works", {
  expect_equal(replace_values(c("a", "b", "b"), "b", "x"), c("a", "x", "x"))
  expect_equal(replace_values(1:3, c(2, 3), c(10, 20)), c(1, 10, 20))
})

test_that("factor replacement and levels update", {
  res <- replace_values(factor(c("a", "b", "c")), c("b", "c"), c("x", "y"))
  expect_equal(as.character(res), c("a", "x", "y"))
  expect_equal(levels(res), c("a", "x", "y"))
})

test_that("warning for missing values", {
  expect_warning(replace_values(c("a", "b"), c("b", "c"), c("x", "y")), "not found")
})

test_that("replace missing values", {
  expect_equal(replace_values(c(NaN, NA, "z"), c(NaN, NA), c("x", "y")), c("x", "y", "z"))
})

test_that("new_values recycling works", {
  expect_equal(replace_values(c("foo", "bar", "baz"), c("bar", "baz"), "zap"), c("foo", "zap", "zap"))
})

test_that("multiple replacements and uniqueness check", {
  expect_error(replace_values(letters, c("a", "a"), c("x", "y")), "unique")
})

test_that("factor with drop_levels = FALSE", {
  res <- replace_values(factor(c("a", "b"), c("a", "b", "d")), "b", "c", drop_levels = FALSE)
  expect_true("d" %in% levels(res))
})

test_that("correct handling of length mismatches", {
  expect_error(replace_values(letters, c("a", "b"), c("x", "y", "z")), "should be length 1 or the same length")
  expect_error(replace_values(letters, "a", c("x", "y")), "cannot be less than")
})
