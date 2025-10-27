test_that("updates and creates by name properly", {
  expect_equal(update_by_name(list(a=1, b=2), list(b=3), c('a','b')), list(a=1, b=3))
  expect_equal(update_by_name(NULL, c(a=9, b=2), c('a','b')), list(a=9, b=2))
})

test_that("recycles values for unnamed new_list", {
  res <- update_by_name(list(a=1, b=2), c(7,8), c('a','b'))
  expect_equal(res, list(a=7, b=8))
})

test_that("errors on bad names", {
  expect_error(update_by_name(list(a=1), list(x=5), c('a')))
  expect_error(update_by_name(NULL, c(x=9), c('a','b')))
})

test_that("functions with NULL new_list", {
  old <- list(a=5, b=8)
  expect_equal(update_by_name(old, NULL, c('a','b')), old)
})
