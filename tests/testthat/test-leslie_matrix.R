test_that("numeric Leslie matrix is built correctly from p", {
  p <- c(0.8, 0.6, 0.4)
  M <- leslie_matrix(p = p)

  expect_true(is.matrix(M))
  expect_equal(dim(M), c(length(p) + 1, length(p) + 1))
  expect_equal(M[1, 1:length(p)], p)
  expect_equal(M[2:(length(p) + 1), 1:length(p)], diag(1 - p), ignore_attr = TRUE)
})

test_that("symbolic Leslie matrix has correct structure from n", {
  M <- leslie_matrix(n = 3, use_subscripts = FALSE, add_lambda = FALSE)

  expect_true(is.matrix(M))
  expect_equal(dim(M), c(3, 3))
  expect_equal(M[1, 3], "1")
  expect_match(M[2, 1], "^1-p_0$")
  expect_match(M[3, 2], "^1-p_1$")
})

test_that("subscripts are applied correctly", {
  M <- leslie_matrix(n = 3, use_subscripts = TRUE, add_lambda = FALSE)
  first_row <- M[1, ]
  expect_true(any(grepl("\u2080", first_row)))
})

test_that("lambda terms are added when requested", {
  M <- leslie_matrix(n = 3, use_subscripts = FALSE, add_lambda = TRUE)
  expect_true(any(grepl("-L", M)))
})

test_that("returns error when both n and p are NULL", {
  expect_error(leslie_matrix(), "Both 'n' and 'p' are NULL")
})

test_that("matrix is symbolic when built with n only", {
  M <- leslie_matrix(n = 3)
  expect_type(M, "character")
})

test_that("dimension consistency check", {
  p <- c(0.5, 0.3)
  M <- leslie_matrix(p = p)
  expect_equal(nrow(M), length(p) + 1)
  expect_equal(ncol(M), length(p) + 1)
})
