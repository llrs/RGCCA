library("RGCCA")
library("gliomaData")
data("ge_cgh_locIGR")

context("Testing sgcca")


A <- ge_cgh_locIGR$multiblocks
Loc <- factor(ge_cgh_locIGR$y) ; levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
C <-  matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
tau <- c(1, 1, 0)



test_that("sgcca output does what it is expected", {
  result.sgcca <- sgcca(A, C, c1 = c(.071,.2, 1), ncomp = c(2, 2, 1),
                       scheme = "centroid", verbose = FALSE)

  expect_equal(result.sgcca$Y[[1]], A[[1]] %*% result.sgcca$astar[[1]])

})
