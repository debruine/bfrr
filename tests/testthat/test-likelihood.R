test_that("default", {
  x <- likelihood()
  x3 <- likelihood(0, .1, 99, "H0", 2000)
  x4 <- likelihood(sample_mean = 0, sample_se = .1, sample_df = 99,
                   model = "H0", steps = 2000)
  x5 <- likelihood(sample_se = .1, steps = 2000, sample_mean = 0,
                   sample_df = 99, model = "H0")

  expect_equal(x, stats::dt(0, df = 99))
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
