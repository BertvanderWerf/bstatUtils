test_that("basic splitting and indicators work", {
  x <- c("A;B", "B", NA, "")
  df <- text_to_columns(x)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("A", "B") %in% names(df)))
  expect_equal(nrow(df), length(x))
  expect_equal(unique(unlist(lapply(df[ , c("A", "B")], unique))), c(1, 0))
})

test_that("works with specified items and Other column", {
  x <- c("A;B", "B;C;Z", "B")
  df <- text_to_columns(x, items = c("A", "B", "C"))
  expect_true("Other" %in% names(df))
  expect_equal(df$Other[2], "Z")
})

test_that("creates NA column when requested", {
  x <- c("A;B", NA)
  df <- text_to_columns(x, na_col = "Missing")
  expect_true("Missing" %in% names(df))
  expect_equal(df$Missing, c(0, 1))
})

test_that("handles case insensitivity", {
  x <- c("Apple;Pear", "pear")
  df <- text_to_columns(x, items = c("apple", "pear"), ignore_case = TRUE)
  expect_true(all(c("apple", "pear") %in% names(df)))
  expect_equal(df$apple, c(1, 0))
})

test_that("returns numeric columns for indicators", {
  x <- c("A;B", "B")
  df <- text_to_columns(x)
  expect_type(df$A, "double")
})

test_that("multiple Other entries combine correctly", {
  x <- c("A;X;Y")
  df <- text_to_columns(x, items = "A", other_sep = "|")
  expect_equal(df$Other, "X|Y")
})
