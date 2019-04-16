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


test_that("test scale_col", {
  set.seed(45792)
  n.obs <- 150
  n.vars <- 2
  x <- matrix(rnorm(n.obs * n.vars), n.obs, n.vars)
  out <- scale_col(x)
  y <- scale(x, center = TRUE, scale = TRUE)
  expect_equivalent(out, y)
})
