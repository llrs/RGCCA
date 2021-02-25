test_that("correct works", {
  C <- matrix(0, ncol = 5, nrow = 5)
  C[1, 3] <- C[3, 1] <- 1
  expect_false(correct(C))
  C <- matrix(0, ncol = 5, nrow = 5)
  C[1, 2] <- C[2, 1] <- 1
  C[2, 3] <- C[3, 2] <- 1
  C[4, 3] <- C[3, 4] <- 1
  C[4, 5] <- C[5, 4] <- 1
  expect_true(correct(C))
  C <- matrix(0, ncol = 5, nrow = 5)
  C[1, 4] <- C[4, 1] <- 1
  C[2, 3] <- C[3, 2] <- 1
  C[2, 5] <- C[5, 2] <- 1
  C[3, 5] <- C[5, 3] <- 1
  expect_false(correct(C))
  C <- matrix(0, ncol = 5, nrow = 5)
  C[4, 1] <- 1
  expect_false(correct(C))
})
