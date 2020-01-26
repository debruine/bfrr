test_that("defaults", {
  x <- bfrr()
  x2 <- bfrr(0, 0.1, 99, "normal", mean = 0, sd = 1, tail = 2)
  x3 <- bfrr(sample_mean = 0, sample_se = 0.1, sample_df = 99,
             model = "normal", mean = 0, sd = 1, tail = 2,
             criterion = 3, rr_interval = NA, precision = 0.05)
  x4 <- bfrr(sample_se = 0.1, criterion = 3, sample_mean = 0, tail = 2,
             model = "normal", sd = 1, mean = 0, sample_df = 99,
             rr_interval = NA, precision = 0.05)
  expect_equal(class(x), c("bfrr", "list"))
  expect_equal(x$theory, 0.03969222, tolerance = 1e-5)
  expect_equal(x$null, 0.3979361, tolerance = 1e-5)
  expect_equal(x$BF, 0.0997452, tolerance = 1e-5)
  expect_equal(x$RR, list(mean = c(0, 0), sd = c(0.3, 2.0)))
  expect_equal(x$criterion, 3)
  expect_equal(x$conclusion, "H0")
  expect_equal(x$model, "N")
  expect_equal(x$params, list(mean = 0, sd = 1, tail = 2))
  expect_equal(x$H0_model, "T(99)")
  expect_equal(x$H1_model, "N(0, 1)")

  expect_equal(x, x2)
  expect_equal(x, x3)

  # get order equal
  x4$params <- x4$params[names(x$params)]
  expect_equal(x, x4)
})

test_that("errors", {
  expect_error(bfrr(model = "nope"), "The model type 'nope' is not supported")
})

test_that("normal", {
  dat <- expand.grid(
    mean = seq(-0.5,0.5, 0.5),
    sd = seq(0.5, 1.5, 0.5),
    n = c(30, 40),
    tmean = seq(0, 1, 0.5),
    tsd = seq(0.5, 1.5, 0.5),
    tail = 1:2
  )
  dat$se <- dat$sd/sqrt(dat$n)

  tmp <- purrr::pmap(dat, function(...) {
    p <- list(...)

    x <- bfrr(p$mean, p$se, p$n-1, "normal", mean = p$tmean, sd = p$tsd, tail = p$tail,
              # skips RR calculations
              rr_interval = list(mean = c(p$tmean, p$tmean), sd = c(p$tsd, p$tsd)))
    x2 <- Bf(p$se, p$mean, p$n-1, 0, 0, 1, p$tmean, p$tsd, p$tail)

    expect_equal(x$theory, x2$LikelihoodTheory)
    expect_equal(x$null, x2$Likelihoodnull)
    expect_equal(x$BF, x2$BayesFactor)
  })
})

# test_that("uniform", {
#   dat <- expand.grid(
#     mean = seq(-1,1, 0.5),
#     sd = seq(0.5, 1.5, 0.5),
#     n = c(30, 40),
#     lower = c(-0.5, 0),
#     upper = c(1, 1.5)
#   )
#   dat$se <- dat$sd/sqrt(dat$n)
#
#   tmp <- purrr::pmap(dat, function(...) {
#     p <- list(...)
#
#     x <- bfrr(p$mean, p$se, p$n-1, "normal", lower = p$lower, upper = p$upper,
#               # skips RR calculations
#               rr_interval = list(lower = c(p$lower, p$lower), upper = c(p$upper, p$upper)))
#     x2 <- Bf(p$se, p$mean, p$n-1, 1, p$lower, p$upper)
#
#     expect_equal(x$theory, x2$LikelihoodTheory, tolerance = 1e-3, scale = 1)
#     expect_equal(x$null, x2$Likelihoodnull)
#     expect_equal(x$BF, x2$BayesFactor, tolerance = 1e-3, scale = 1)
#   })
# })

