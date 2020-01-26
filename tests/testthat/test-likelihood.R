test_that("default", {
  x <- likelihood()
  x2 <- Bf()
  x3 <- likelihood(0, .1, 99, "H0", 2000)
  x4 <- likelihood(sample_mean = 0, sample_se = .1, sample_df = 99,
                   model = "H0", steps = 2000)
  x5 <- likelihood(sample_se = .1, steps = 2000, sample_mean = 0,
                   sample_df = 99, model = "H0")

  expect_equal(x, stats::dt(0, df = 99))
  expect_equal(x, x2$Likelihoodnull)
  expect_equal(x, x3)
  expect_equal(x, x4)
  expect_equal(x, x5)
})

test_that("normal", {
  x <- likelihood(model = "normal")
  x2 <- Bf()
  expect_equal(x, x2$LikelihoodTheory)

  x2 <- likelihood(sample_mean = 0.5, model = "normal")
  x1 <- likelihood(sample_mean = 0.5, model = "normal", tail = 1)
  expect_equal(x1, x2*2, tolerance = 1e-4, scale = 1)

  replicate(100, {
    sm <- runif(1)
    se <- runif(1, 0.05, 0.2)
    df <- sample(10:100, 1)
    mt <- runif(1, 0, 0.5)
    sdt <- runif(1, 0.05, 0.2)
    tail <- sample(1:2, 1)
    x <- likelihood(sample_mean = sm, sample_se = se, sample_df = df,
                    model = "normal", mean = mt, sd = sdt, tail = tail)
    x2 <- Bf(sd = se, obtained = sm, dfdata = df, uniform = 0,
             meanoftheory = mt, sdtheory = sdt, tail = tail)
    expect_equal(x, x2$LikelihoodTheory)
  })
})

test_that("uniform", {
  x <- likelihood(model = "uniform")
  x2 <- Bf(uniform = 1)
  expect_equal(x, x2$LikelihoodTheory)

  replicate(100, {
    sm <- runif(1)
    se <- runif(1, 0.05, 0.2)
    df <- sample(10:100, 1)
    lower <- runif(1, 0, 0.5)
    upper <- lower + runif(1, 0, 0.5)
    x <- likelihood(sample_mean = sm, sample_se = se, sample_df = df,
                    model = "uniform", lower = lower, upper = upper)
    x2 <- Bf(sd = se, obtained = sm, dfdata = df, uniform = 1,
             lower = lower, upper = upper)
    expect_equal(x, x2$LikelihoodTheory)
  })
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
