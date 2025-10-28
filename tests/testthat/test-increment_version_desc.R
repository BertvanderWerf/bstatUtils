test_that("increments major, minor, patch, dev/build", {
  # Create a temporary DESCRIPTION file for testing
  temp_dir <- tempdir()
  desc_path <- file.path(temp_dir, "DESCRIPTION")

  # Minimal DESCRIPTION template
  writeLines(c(
    "Package: testpkg",
    "Version: 1.2.3",
    "Title: Test Package",
    "Description: For testing increment_version_desc."
  ), desc_path)

  # Each increment returns correct result and updates DESCRIPTION
  expect_equal(
    increment_version_desc(temp_dir, "patch"),
    c(old_version = "1.2.3", new_version = "1.2.4")
  )

  expect_equal(
    increment_version_desc(temp_dir, "minor"),
    c(old_version = "1.2.4", new_version = "1.3.0")
  )

  expect_equal(
    increment_version_desc(temp_dir, "major"),
    c(old_version = "1.3.0", new_version = "2.0.0")
  )

  expect_equal(
    increment_version_desc(temp_dir, "build"),
    c(old_version = "2.0.0", new_version = "2.0.0.9000")
  )

  # Test further dev/build increment
  expect_equal(
    increment_version_desc(temp_dir, "build"),
    c(old_version = "2.0.0.9000", new_version = "2.0.0.9001")
  )
})

test_that("errors on wrong input", {
  temp_dir <- tempdir()
  writeLines(c(
    "Package: testpkg",
    "Version: 1.2.3",
    "Title: Test Package"
  ), file.path(temp_dir, "DESCRIPTION"))

  expect_error(
    increment_version_desc(temp_dir, "notvalid")
  )
})

# Optionally clean up temporary files
# file.remove(file.path(temp_dir, "DESCRIPTION"))
