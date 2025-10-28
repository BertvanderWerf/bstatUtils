test_that("reciprocal and sqr work", {
  expect_equal(reciprocal(2), 0.5)
  expect_equal(sqr(2), 4)
  expect_error(reciprocal("a"))
  expect_error(sqr(c(TRUE, FALSE)))
})

test_that("get_inverse_function works in both directions", {
  expect_equal(get_inverse_function("log"), "exp")
  expect_equal(get_inverse_function("exp"), "log")
  expect_equal(get_inverse_function("reciprocal"), "reciprocal")
  expect_null(get_inverse_function("nonexistent"))
})

# test_that("add_inverse_function properly adds", {
#   old <- inverse_function_table
#   add_inverse_function("tan", "atan", env = globalenv())
#   expect_equal(get_inverse_function("tan", inverse_function_table), "atan")
#   # restore for further tests
#   assign("inverse_function_table", old, envir = globalenv())
# })

test_that("add_inverse_function fails for wrong function", {
  expect_error(add_inverse_function("notafunc", "exp", env = globalenv()))
  expect_error(add_inverse_function("log", "exp", env = globalenv()))
})
