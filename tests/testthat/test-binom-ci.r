test_that("binom_ci_all returns all methods in the documented order", {
  all_methods <- c(
    "wald", "wilson", "clopper-pearson", "agresti-coull",
    "jeffreys", "logit", "likelihood"
  )

  res_all <- binom_ci_all(19, 20)

  expect_equal(nrow(res_all), length(all_methods))
  expect_identical(as.character(res_all$method), all_methods)
  expect_true(all(abs(res_all$estimate - 19 / 20) < 1e-12))
  expect_true(all(res_all$lower >= 0 & res_all$upper <= 1))
  expect_true(all(res_all$lower <= res_all$upper))
})

test_that("Wilson is the default method", {
  res_default <- binom_ci(19, 20)
  res_wilson <- binom_ci(19, 20, method = "wilson")

  expect_equal(res_default, res_wilson)
})

test_that("Clopper-Pearson respects the parameter boundaries", {
  res_cp0 <- binom_ci(0, 10, method = "clopper-pearson")
  res_cpN <- binom_ci(10, 10, method = "clopper-pearson")

  expect_identical(res_cp0$lower, 0)
  expect_identical(res_cpN$upper, 1)
})

test_that("Logit intervals stay finite at the boundaries", {
  res_logit0 <- binom_ci(0, 10, method = "logit")
  res_logitN <- binom_ci(10, 10, method = "logit")

  expect_true(is.finite(res_logit0$lower))
  expect_true(is.finite(res_logit0$upper))
  expect_true(is.finite(res_logitN$lower))
  expect_true(is.finite(res_logitN$upper))
})

test_that("Likelihood intervals contain the MLE for interior counts", {
  res_like_mid <- binom_ci(5, 10, method = "likelihood")

  expect_lt(res_like_mid$lower, 0.5)
  expect_gt(res_like_mid$upper, 0.5)
})

test_that("Invalid inputs raise informative errors", {
  expect_error(
    binom_ci(11, 10, method = "wilson"),
    "must not exceed"
  )

  expect_error(
    binom_ci(1.5, 10, method = "wilson"),
    "integer-like"
  )

  expect_error(
    binom_ci(1, 0, method = "wilson"),
    "positive integer-like"
  )

  expect_error(
    binom_ci(1, 10, conf.level = 1),
    "conf.level"
  )

  expect_error(
    binom_ci(0, 10, method = "logit", cc = -0.1),
    "`cc`"
  )
})
