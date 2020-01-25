test_that("simple values", {
  a <- 1
  b <- 3
  expect_equal(default(a, 2), a)
  remove(a)
  expect_equal(default(a, 2), 2)
  expect_equal(default(a, b), 3)
})

test_that("named lists", {
  a <- list("first" = 1, "second" = 2)
  expect_equal(default(a$first, 3), a$first)
  expect_equal(default(a$third, 3), 3)

  expect_equal(default(a["first"], 3), a["first"])
  expect_equal(default(a["third"], 3), 3)

  expect_equal(default(a[["first"]], 3), a[["first"]])
  expect_equal(default(a[["third"]], 3), 3)
})

test_that("unnamed lists", {
  a <- list(1, "two", NA)
  expect_equal(default(a[1], "no"), a[1])
  expect_equal(default(a[2], "no"), a[2])
  expect_equal(default(a[3], "no"), list("no"))
  expect_equal(default(a[[1]], "no"), a[[1]])
  expect_equal(default(a[[2]], "no"), a[[2]])
  expect_equal(default(a[[3]], "no"), "no")

  expect_equal(default(a, "no"), list(1, "two", "no"))
  expect_equal(default(a, "no", FALSE), a)
})

test_that("vectors", {
  a <- 1:5
  expect_equal(default(a, 11:15), a)
  remove(a)
  expect_equal(default(a, 11:15), 11:15)

  a <- c(1, 2, 3, NA)
  expect_equal(default(a, 4), 1:4)
  expect_equal(default(a, 4, FALSE), a)
  expect_equal(default(a, 1:4), 1:4)

  # if the default is a different data type from the vector, coersion will occur
  expect_equal(default(a, "no"), c("1", "2", "3", "no"))

  expect_equal(default(a[1], "no"), a[1])
  expect_equal(default(a[4], "no"), "no")
})
