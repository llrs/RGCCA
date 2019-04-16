context("tau.estimate")

test_that("tau.estimate", {
  data("Russett")
  expect_equal(tau.estimate(Russett), 0.114279478757313)

})
test_that("implementations are coherent", {
  data(Russett)
  set.seed(45791)
  X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
  s <- scale2_(X_agric, bias = TRUE)
  t1 <- tau.estimate(s)
  expect_true(t1 != 1)
})
