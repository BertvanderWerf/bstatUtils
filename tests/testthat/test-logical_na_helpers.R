test_that("na_false replaces NA with FALSE", {
  expect_equal(na_false(c(TRUE, NA, FALSE)), c(TRUE, FALSE, FALSE))
})

test_that("na_true replaces NA with TRUE", {
  expect_equal(na_true(c(TRUE, NA, FALSE)), c(TRUE, TRUE, FALSE))
})

test_that("works for all NA, all TRUE, all FALSE", {
  expect_equal(na_false(rep(NA, 5)), rep(FALSE, 5))
  expect_equal(na_true(rep(NA, 5)), rep(TRUE, 5))
  expect_equal(na_false(rep(TRUE, 3)), rep(TRUE, 3))
  expect_equal(na_true(rep(FALSE, 2)), rep(FALSE, 2))
})

test_that("errors on non-logical input", {
  expect_error(na_false(1:3))
  expect_error(na_true("a"))
})

test_that("returns NULL for NULL input", {
  expect_null(na_false(NULL))
  expect_null(na_true(NULL))
})
