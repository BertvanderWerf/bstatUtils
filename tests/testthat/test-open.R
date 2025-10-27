test_that("fails gracefully on wrong input", {
  expect_error(open(NULL))
  expect_error(open(integer(1)))
  expect_error(open(NA_character_))
  expect_error(open(c("file1.txt", "file2.txt")))
})

#test_that("returns FALSE and raises warning on unsupported OS", {
  # Simulate fake OS: safest to only run if not on standard platform (for true test, would mock Sys.info)
  # skip_on_cran()
  # skip_on_ci()
  # (In real tests, use withr::with_envvar to fake platform)
#})

#test_that("returns TRUE or FALSE on platform", {
  # Should work (side effect: attempts to open URL)
  # expect_true(open("https://www.r-project.org")) # existence not checked
  # The above is usually safe; on headless CI it may fail, so always ignore result value.
  # Manual check: open(tempfile())
  # (Future: use mockery or withr for system() call testing)
#})
