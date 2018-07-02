
context("Testing sgcca")





test_that("sgcca output does what it is expected", {
  library("gliomaData")
  data("ge_cgh_locIGR")
  A <- ge_cgh_locIGR$multiblocks
  Loc <- factor(ge_cgh_locIGR$y) ; levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
  C <-  matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  tau <- c(1, 1, 0)

  result.sgcca <- sgcca(A, C, c1 = c(.071,.2, 1), ncomp = c(2, 2, 1),
                        scheme = "centroid", verbose = FALSE)

  expect_equal(result.sgcca$C, C)
  expect_equal(result.sgcca$ncomp, c(2L, 2L, 1L))
  expect_length(result.sgcca$crit, 2L)
  expect_equal(result.sgcca$crit[[2]],
               c(0.0705922441625706, 0.0723431879010189, 0.0912348678837695,
                 0.119025897140844, 0.12238825814507, 0.122504903372083, 0.122508520626011,
                 0.122508633245961, 0.122508636752987, 0.122508636870408, 0.122508636873339,
                 0.122508636867339, 0.122508636867962, 0.122508636870334, 0.122508636865809,
                 0.122508636862402))
  expect_equal(result.sgcca$scheme, "centroid")
  expect_equal(result.sgcca$AVE$AVE_inner, c(0.609213197536486, 0.45712854179023))
  expect_equal(result.sgcca$AVE$AVE_outer, c(0.0691671029653102, 0.0425623110059463))
  expect_length(result.sgcca$AVE, 3)

  expect_length(result.sgcca$Y, 3)
  expect_length(result.sgcca$a, 3)

  expect_true(is(result.sgcca$a[[1]], "matrix"))
  expect_true(is(result.sgcca$a[[2]], "matrix"))
  expect_true(is(result.sgcca$a[[3]], "matrix"))

  expect_true(is(result.sgcca$Y[[1]], "matrix"))
  expect_true(is(result.sgcca$Y[[2]], "matrix"))
  expect_true(is(result.sgcca$Y[[3]], "matrix"))

  expect_equal(dim(result.sgcca$a[[1]]), c(15702L, 2L))

})
