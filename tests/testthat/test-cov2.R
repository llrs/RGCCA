test_that("cov2", {
  out <- cov2(1:10)
  expect_equal(out[1, 1], 8.25)
})
