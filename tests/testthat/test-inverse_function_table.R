test_that("reciprocal and sqr work", {
  expect_equal(reciprocal(2), 0.5)
  expect_equal(sqr(2), 4)
  expect_error(reciprocal("a"))
  expect_error(sqr(c(TRUE, FALSE)))
})

test_that("get_inverse_function works in both directions", {
  tbl <- inverse_function_table
  expect_equal(get_inverse_function("log",  table = tbl), "exp")
  expect_equal(get_inverse_function("exp",  table = tbl), "log")
  expect_equal(get_inverse_function("reciprocal",  table = tbl), "reciprocal")
  expect_null(get_inverse_function("nonexistent",  table = tbl))
})

test_that("add_inverse_function properly adds", {
  tbl <- inverse_function_table
  tbl <- add_inverse_function("tan", "atan", table = tbl)
  expect_equal(get_inverse_function("tan", tbl), "atan")
})

test_that("add_inverse_function fails for wrong function", {
  expect_error(add_inverse_function("notafunc", "exp", env = globalenv()))
  expect_error(add_inverse_function("log", "exp", env = globalenv()))
})
