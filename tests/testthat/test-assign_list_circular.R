test_that("handles circular assignment and naming", {
  expect_equal(assign_list_circular(c(1,2), 5), list(1,2,1,2,1))
  expect_equal(names(assign_list_circular(1:3, 3, names = c("x","y","z"))), c("x","y","z"))
})

test_that("handles null input and zero length", {
  expect_equal(assign_list_circular(NULL, 3), list(NULL, NULL, NULL))
  expect_equal(assign_list_circular(1, 0), list())
})

test_that("throws error for invalid arguments", {
  expect_error(assign_list_circular(1:2, "a"))
  expect_error(assign_list_circular(1:2, 3, names = c("A")))
  expect_error(assign_list_circular(numeric(0), 3))
})
