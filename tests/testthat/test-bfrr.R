test_that("defaults", {
  x <- bfrr()
  x2 <- bfrr(0, 0.1, 99, "normal", mean = 0, sd = 1, tail = 2, 3, c(0,2), 2)
  x3 <- bfrr(sample_mean = 0, sample_se = 0.1, sample_df = 99,
             model = "normal", mean = 0, sd = 1, tail = 2,
             criterion = 3, rr_interval = c(0, 2), precision = 2)
  x4 <- bfrr(sample_se = 0.1, criterion = 3, sample_mean = 0, tail = 2,
             model = "normal", sd = 1, mean = 0, sample_df = 99,
             rr_interval = c(0, 2), precision = 2)
  expect_equal(class(x), c("bfrr", "list"))
  expect_equal(x$theory, 0.03969222, tolerance = 1e-5)
  expect_equal(x$null, 0.3979361, tolerance = 1e-5)
  expect_equal(x$BF, 0.0997452, tolerance = 1e-5)
  expect_equal(x$RR, c(0.29, 2.00))
  expect_equal(x$criterion, 3)
  expect_equal(x$conclusion, "H0")
  expect_equal(x$H0_model, "T(99)")
  expect_equal(x$H1_model, "N(0, 1)")

  expect_equal(x, x2)
  expect_equal(x, x3)
  expect_equal(x, x4)
})
