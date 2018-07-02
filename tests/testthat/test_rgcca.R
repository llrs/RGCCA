context("RGCCA")

test_that("Example 1:factorial", {
  data(Russett)
  X_agric =as.matrix(Russett[,c("gini","farm","rent")])
  X_ind = as.matrix(Russett[,c("gnpr","labo")])
  X_polit = as.matrix(Russett[ , c("demostab", "dictator")])
  A = list(X_agric, X_ind, X_polit)
  #Define the design matrix (output = C)
  C = matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  result.rgcca = rgcca(A, C, tau = c(1, 1, 1), scheme = "factorial",
                       scale = TRUE, verbose = FALSE)

  expect_equal(result.rgcca$C, C)
  expect_equal(result.rgcca$tau, c(1L, 1L, 1L))
  expect_equal(result.rgcca$ncomp, c(1L, 1L, 1L))
  expect_equal(result.rgcca$crit, c(1.01185900653456, 1.01187178801355, 1.01187178931001))
  expect_equal(result.rgcca$scheme, "factorial")
  expect_equal(result.rgcca$AVE$AVE_inner, 0.38448863486574)
  expect_equal(result.rgcca$AVE$AVE_outer, 0.772561329080763)
  expect_length(result.rgcca$AVE, 3)

  expect_length(result.rgcca$Y, 3)
  expect_length(result.rgcca$a, 3)

  expect_true(is(result.rgcca$a[[1]], "matrix"))
  expect_true(is(result.rgcca$a[[2]], "matrix"))
  expect_true(is(result.rgcca$a[[3]], "matrix"))

  expect_true(is(result.rgcca$Y[[1]], "matrix"))
  expect_true(is(result.rgcca$Y[[2]], "matrix"))
  expect_true(is(result.rgcca$Y[[3]], "matrix"))

  expect_equal(dim(result.rgcca$a[[1]]), c(3L, 1L))
})


test_that("Example 2:function", {
  data(Russett)
  X_agric =as.matrix(Russett[,c("gini","farm","rent")])
  X_ind = as.matrix(Russett[,c("gnpr","labo")])
  X_polit = as.matrix(Russett[ , c("inst", "ecks", "death",
                                   "demostab", "dictator")])
  A = list(X_agric, X_ind, X_polit, cbind(X_agric, X_ind, X_polit))

  #Define the design matrix (output = C)
  C = matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0), 4, 4)
  result.rgcca = rgcca(A, C, tau = c(1, 1, 1, 0), ncomp = rep(2, 4),
                       scheme = function(x) x^4, scale = TRUE, verbose = FALSE)
  # HPCA


  expect_equal(result.rgcca$C, C)
  expect_equal(result.rgcca$tau, c(1L, 1L, 1L, 0L))
  expect_equal(result.rgcca$ncomp, c(2L, 2L, 2L, 2L))
  expect_length(result.rgcca$crit, 2L)
  expect_equal(result.rgcca$crit[[1]],
               c(1.80671549082325, 1.89149449550107, 1.90180193655475, 1.90263165645116,
                 1.90268926267435, 1.90269315425883, 1.90269341858531, 1.90269343664851,
                 1.90269343788708))
  expect_true(is(result.rgcca$scheme, "function"))
  expect_equal(result.rgcca$AVE$AVE_inner, c(0.615689384285592, 0.48123151788268))
  expect_equal(result.rgcca$AVE$AVE_outer_model, c(0.528033239739941, 0.132205670993204))
  expect_length(result.rgcca$AVE, 3)

  expect_length(result.rgcca$Y, 4)
  expect_length(result.rgcca$a, 4)

  expect_true(is(result.rgcca$a[[1]], "matrix"))
  expect_true(is(result.rgcca$a[[2]], "matrix"))
  expect_true(is(result.rgcca$a[[3]], "matrix"))

  expect_true(is(result.rgcca$Y[[1]], "matrix"))
  expect_true(is(result.rgcca$Y[[2]], "matrix"))
  expect_true(is(result.rgcca$Y[[3]], "matrix"))

  expect_equal(dim(result.rgcca$a[[1]]), c(3L, 2L))

})


test_that("Example 3: factorial", {
  Ytest = matrix(0, 47, 3)
  X_agric =as.matrix(Russett[,c("gini","farm","rent")])
  X_ind = as.matrix(Russett[,c("gnpr","labo")])
  X_polit = as.matrix(Russett[ , c("demostab", "dictator")])
  A = list(X_agric, X_ind, X_polit)
  #Define the design matrix (output = C)
  C = matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  result.rgcca = rgcca(A, C, tau = rep(1, 3), ncomp = rep(1, 3),
                       scheme = "factorial", verbose = FALSE)

  expect_equal(result.rgcca$C, C)
  expect_equal(result.rgcca$tau, c(1L, 1L, 1L))
  expect_equal(result.rgcca$ncomp, c(1L, 1L, 1L))
  expect_length(result.rgcca$crit, 3L)
  expect_equal(result.rgcca$crit,
               c(1.01185900653456, 1.01187178801355, 1.01187178931001))
  expect_equal(result.rgcca$scheme, "factorial")
  expect_equal(result.rgcca$AVE$AVE_inner, 0.38448863486574)
  expect_equal(result.rgcca$AVE$AVE_outer, 0.772561329080763)
  expect_length(result.rgcca$AVE, 3)

  expect_length(result.rgcca$Y, 3)
  expect_length(result.rgcca$a, 3)

  expect_true(is(result.rgcca$a[[1]], "matrix"))
  expect_true(is(result.rgcca$a[[2]], "matrix"))
  expect_true(is(result.rgcca$a[[3]], "matrix"))

  expect_true(is(result.rgcca$Y[[1]], "matrix"))
  expect_true(is(result.rgcca$Y[[2]], "matrix"))
  expect_true(is(result.rgcca$Y[[3]], "matrix"))

  expect_equal(dim(result.rgcca$a[[1]]), c(3L, 1L))

})
