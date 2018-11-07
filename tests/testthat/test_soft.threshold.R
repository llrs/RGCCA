context("test_soft.threshold")

test_that("soft.threshold works", {
  x <- rnorm(10)
  out <- soft.threshold(x, 0.5)
  expect_equal(out, c(0, 0, 0, 0, 0, 0, 0, 1.34653177497057e-10, 0, 0))
})
