context("test-binarysearch")


set.seed(45)
x <- rnorm(15702)
d <- rnorm(15702)
test_that("binarysearch works", {
  out <- BinarySearch(x, 1)
  expect_equal(out, 4.112401781225)
})
