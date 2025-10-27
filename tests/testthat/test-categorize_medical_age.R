test_that("categorize_medical_age correctly classifies ages into bins", {
  ages <- c(0.5, 4, 10, 16, 25, 40, 55, 70, 90)
  age_classes <- categorize_medical_age(ages)

  expect_type(age_classes, "integer")  # factor under the hood
  expect_s3_class(age_classes, "factor")
  expect_equal(length(age_classes), length(ages))
  expect_equal(levels(age_classes)[1], "Infancy")
  expect_equal(as.character(age_classes[1]), "Infancy")
  expect_equal(as.character(age_classes[9]), "Advanced age")
})

test_that("categorize_medical_age adds metadata attribute", {
  ages <- c(5, 20)
  result <- categorize_medical_age(ages)
  expect_true(!is.null(attr(result, "age_class_info")))
  expect_true("ColumnName" %in% colnames(attr(result, "age_class_info")))
})

test_that("categorize_medical_age handles boundary values correctly", {
  boundaries <- c(0, 2, 3, 5, 6, 13, 14, 18, 19, 33, 34, 48, 49, 64, 65, 78, 79, 100)
  results <- categorize_medical_age(boundaries)
  expect_length(results, length(boundaries))
  expect_equal(as.character(results[1]), "Infancy")
  expect_equal(as.character(results[length(boundaries)]), "Advanced age")
})

test_that("categorize_medical_age throws error for invalid input", {
  expect_error(categorize_medical_age("ten"))
  expect_error(categorize_medical_age(NULL))
})

