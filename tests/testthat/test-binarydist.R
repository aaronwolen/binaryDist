context("Compatibility")

set.seed(123)
x <- matrix(rbinom(1e6, 1, 0.1), ncol = 100)

time.sd <- system.time(out.sd <- dist(t(x), method = "binary"))
time.bd <- system.time(out.bd <- binaryDist(t(x)))

test_that("matches stats::binary", {
  expect_equivalent(out.bd, out.sd)
})


test_that("faster than stats::binary", {
  expect_true((time.sd > time.bd)["elapsed"])
})


