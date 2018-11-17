context("tau.estimate")

test_that("tau.estimate", {
  data("Russett")
  expect_equal(tau.estimate(Russett), 0.114279478757313)
})
