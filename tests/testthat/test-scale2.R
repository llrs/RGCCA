test_that("Works without changing", {
  out <- scale2(1:5)
  expect_equal(out[, 1], c(
    -1.41421356237309, -0.707106781186547, 0, 0.707106781186547,
    1.41421356237309
  ))
  expect_error(scale2(1:5, center = FALSE), "positive length")
  out <- scale2(matrix(1:6, 2, 3), center = FALSE)
  expect_equal(out[, 1], c(4L, 8L))

  out <- scale2(matrix(1:6, 2, 3), scale = FALSE)
  expect_equal(out[, 1], c(-0.5, .5))

  out <- scale2(matrix(1:6, 2, 3), scale = FALSE, center = FALSE)
  expect_true(is.null(out))

  out <- scale2(matrix(1:6, 2, 3), bias = FALSE)
  expect_equal(out[, 1], c(-0.707106781186547, 0.707106781186547))

  out <- scale2(matrix(1:6, 2, 3), bias = FALSE, scale = FALSE)
  expect_equal(out[, 1], c(-0.5, .5))

  out <- scale2(matrix(1:6, 2, 3), bias = FALSE, center = FALSE)
  expect_equal(out[, 1], c(2L, 4L))
})
