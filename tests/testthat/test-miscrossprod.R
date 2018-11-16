context("test-miscrossprod")

set.seed(45791)
data("ge_cgh_locIGR", package = "gliomaData")
A <- ge_cgh_locIGR$multiblocks
Loc <- factor(ge_cgh_locIGR$y)
levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
tau <- c(1, 1, 0)

test_that("miscrossprod works", {
  out <- miscrossprod(A[[1]], A[[1]])
  expect_equal(out, 1694006.77320806)
  result <- sum(A[[1]] * A[[1]])
  expect_equal(out, result)

})

test_that("miscrossprod equivalent", {
  a <- svd(A[[1]], nu = 0, nv = 1)$v
  out2 <- apply(A[[1]], 1, miscrossprod, a)
  result2 <- A[[1]] %*% a
  expect_equal(out2, drop(result2))
})
