test_that("default", {
  x <- likelihood()
  x2 <- bfnorm()
  x3 <- likelihood(0, .1, 99, "H0", 2000)
  x4 <- likelihood(sample_mean = 0, sample_se = .1, sample_df = 99,
                   model = "H0", steps = 2000)
  x5 <- likelihood(sample_se = .1, steps = 2000, sample_mean = 0,
                   sample_df = 99, model = "H0")

  expect_equal(x, stats::dt(0, df = 99))
  expect_equal(x, x2$null)
  expect_equal(x, x3)
  expect_equal(x, x4)
  expect_equal(x, x5)
})

test_that("steps", {
  x <- likelihood(model = "N")
  x100 <- likelihood(model = "N", steps = 100)
  expect_equal(x, x100, tolerance = 1e-6, scale = 1)
  expect_true(x != x100)
})

test_that("normal", {
  x <- likelihood(model = "normal")
  x2 <- bfnorm()

  expect_equal(x, x2$theory)

  x <- likelihood(model = "normal", mean = 0.2, sd = 0.5, tail = 1)
  x2 <- bfnorm(theory_mean = 0.2, theory_sd = 0.5, tail = 1)

  expect_equal(x, x2$theory)
})

test_that("uniform", {
  x <- likelihood(model = "uniform")
  x2 <- bfunif()

  expect_equal(x, x2$theory)

  x <- likelihood(model = "uniform", lower = 0.2, upper = 0.8)
  x2 <- bfunif(lower = 0.2, upper = 0.8)

  expect_equal(x, x2$theory)
})

test_that("aliases", {
  x <- likelihood(model = "normal")
  x2 <- likelihood(model = "norm")
  x3 <- likelihood(model = "N")
  x4 <- likelihood(model = "n")
  x5 <- likelihood(model = " NoRmAl ")

  expect_equal(x, x2)
  expect_equal(x, x3)
  expect_equal(x, x4)
  expect_equal(x, x5)

  x <- likelihood(model = "uniform")
  x2 <- likelihood(model = "unif")
  x3 <- likelihood(model = "U")
  x4 <- likelihood(model = "u")
  x5 <- likelihood(model = " UniForM ")

  expect_equal(x, x2)
  expect_equal(x, x3)
  expect_equal(x, x4)
  expect_equal(x, x5)
})
