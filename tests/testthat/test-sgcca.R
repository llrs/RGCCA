test_that("sgcca centroid", {
  skip_on_travis()
  set.seed(45791)
  data("ge_cgh_locIGR", package = "gliomaData")
  A <- ge_cgh_locIGR$multiblocks
  Loc <- factor(ge_cgh_locIGR$ylabel)
  levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  tau <- c(1, 1, 0)

  result.sgcca <- sgcca(A, C,
    c1 = c(.071, .2, 1), ncomp = c(2, 2, 1),
    scheme = "centroid", verbose = FALSE
  )

  expect_equal(result.sgcca$C, C)
  expect_equal(result.sgcca$ncomp, c(2L, 2L, 1L))
  expect_length(result.sgcca$crit, 2L)
  expect_equal(sum(result.sgcca$a[[1]] != 0), 296L)
  expect_equal(result.sgcca$a[[1]][261, ], c(-0.309282475578372, 0))
  expect_equal(sum(result.sgcca$a[[2]] != 0), 161L)
  expect_equal(sum(result.sgcca$a[[3]] != 0), 3L)

  ha <- scale2_(A[[1]], TRUE) %*% result.sgcca$astar[[1]]
  colnames(ha) <- c("comp1", "comp2")
  expect_equal(ha, result.sgcca$Y[[1]])
  expect_length(result.sgcca$crit[[2]], 16L)
  expect_equal(result.sgcca$scheme, "centroid")
  expect_equal(result.sgcca$AVE$AVE_inner, c(0.609213197536486, 0.45712854179023))
  expect_equal(result.sgcca$AVE$AVE_outer, c(0.0691671029653102, 0.0425623110059463))
  expect_length(result.sgcca$AVE, 3)

  expect_length(result.sgcca$AVE$AVE_X[[1]], 2)
  expect_length(result.sgcca$AVE$AVE_X[[2]], 2)
  expect_length(result.sgcca$AVE$AVE_X[[3]], 1)

  expect_true(all(!is.na(result.sgcca$AVE$AVE_X[[1]])))
  expect_true(all(!is.na(result.sgcca$AVE$AVE_X[[2]])))
  expect_true(all(!is.na(result.sgcca$AVE$AVE_X[[3]])))

  expect_length(result.sgcca$Y, 3)
  expect_length(result.sgcca$a, 3)

  expect_true(is(result.sgcca$a[[1]], "matrix"))
  expect_true(is(result.sgcca$a[[2]], "matrix"))
  expect_true(is(result.sgcca$a[[3]], "matrix"))

  expect_true(is(result.sgcca$Y[[1]], "matrix"))
  expect_true(is(result.sgcca$Y[[2]], "matrix"))
  expect_true(is(result.sgcca$Y[[3]], "matrix"))

  expect_true(all(!is.na(result.sgcca$Y[[1]][1, ])))
  expect_true(all(!is.na(result.sgcca$Y[[2]][1, ])))
  expect_true(all(!is.na(result.sgcca$Y[[3]][1, ])))

  expect_true(all(!is.na(result.sgcca$a[[1]][1, ])))
  expect_true(all(!is.na(result.sgcca$a[[2]][1, ])))
  expect_true(all(!is.na(result.sgcca$a[[3]][1, ])))

  expect_equal(dim(result.sgcca$a[[1]]), c(15702L, 2L))
})



test_that("sgcca horst", {
  skip_on_travis()
  data("ge_cgh_locIGR", package = "gliomaData")
  A <- ge_cgh_locIGR$multiblocks
  Loc <- factor(ge_cgh_locIGR$ylabel)
  levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  tau <- c(1, 1, 0)

  result.sgcca <- sgcca(A, C,
                        c1 = c(.071, .2, 1), ncomp = c(2, 2, 1),
                        scheme = "horst", verbose = FALSE
  )

  expect_equal(sum(result.sgcca$a[[1]] != 0), 296L)
  expect_equal(sum(result.sgcca$a[[2]] != 0), 161L)
  expect_equal(sum(result.sgcca$a[[3]] != 0), 3L)

  ha <- scale2_(A[[1]], TRUE) %*% result.sgcca$astar[[1]]
  colnames(ha) <- c("comp1", "comp2")
  expect_equal(ha, result.sgcca$Y[[1]])
  expect_equal(result.sgcca$C, C)
  expect_equal(result.sgcca$ncomp, c(2L, 2L, 1L))
  expect_length(result.sgcca$crit, 2L)
  expect_length(result.sgcca$crit[[2]], 16L)
  expect_equal(result.sgcca$scheme, "horst")
  expect_equal(result.sgcca$AVE$AVE_inner, c(0.609213197536486, 0.45712854179023))
  expect_equal(result.sgcca$AVE$AVE_outer, c(0.0691671029653102, 0.0425623110059463))
  expect_length(result.sgcca$AVE, 3)

  expect_length(result.sgcca$AVE$AVE_X[[1]], 2)
  expect_length(result.sgcca$AVE$AVE_X[[2]], 2)
  expect_length(result.sgcca$AVE$AVE_X[[3]], 1)

  expect_true(all(!is.na(result.sgcca$AVE$AVE_X[[1]])))
  expect_true(all(!is.na(result.sgcca$AVE$AVE_X[[2]])))
  expect_true(all(!is.na(result.sgcca$AVE$AVE_X[[3]])))

  expect_length(result.sgcca$Y, 3)
  expect_length(result.sgcca$a, 3)

  expect_true(is(result.sgcca$a[[1]], "matrix"))
  expect_true(is(result.sgcca$a[[2]], "matrix"))
  expect_true(is(result.sgcca$a[[3]], "matrix"))

  expect_true(is(result.sgcca$Y[[1]], "matrix"))
  expect_true(is(result.sgcca$Y[[2]], "matrix"))
  expect_true(is(result.sgcca$Y[[3]], "matrix"))

  expect_true(all(!is.na(result.sgcca$Y[[1]][1, ])))
  expect_true(all(!is.na(result.sgcca$Y[[2]][1, ])))
  expect_true(all(!is.na(result.sgcca$Y[[3]][1, ])))

  expect_true(all(!is.na(result.sgcca$a[[1]][1, ])))
  expect_true(all(!is.na(result.sgcca$a[[2]][1, ])))
  expect_true(all(!is.na(result.sgcca$a[[3]][1, ])))

  expect_equal(dim(result.sgcca$a[[1]]), c(15702L, 2L))
})


test_that("sgcca factorial", {
  skip_on_travis()
  data("ge_cgh_locIGR", package = "gliomaData")
  A <- ge_cgh_locIGR$multiblocks
  Loc <- factor(ge_cgh_locIGR$ylabel)
  levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  tau <- c(1, 1, 0)

  result.sgcca <- sgcca(A, C,
                        c1 = c(.071, .2, 1), ncomp = c(2, 2, 1),
                        scheme = "factorial", verbose = FALSE
  )

  ha <- scale2_(A[[1]], TRUE) %*% result.sgcca$astar[[1]]
  colnames(ha) <- c("comp1", "comp2")
  expect_equal(ha, result.sgcca$Y[[1]])
  expect_equal(result.sgcca$C, C)
  expect_equal(result.sgcca$ncomp, c(2L, 2L, 1L))
  expect_length(result.sgcca$crit, 2L)

  expect_equal(sum(result.sgcca$a[[1]] != 0), 293L)
  expect_equal(sum(result.sgcca$a[[2]] != 0), 156L)
  expect_equal(sum(result.sgcca$a[[3]] != 0), 3L)

  expect_length(result.sgcca$crit[[2]], 17L)
  expect_equal(result.sgcca$scheme, "factorial")
  expect_equal(result.sgcca$AVE$AVE_inner, c(0.597889877456015, 0.437202629644858))
  expect_equal(result.sgcca$AVE$AVE_outer, c(0.0691408017211274, 0.0433081948939905))
  expect_length(result.sgcca$AVE, 3)

  expect_length(result.sgcca$AVE$AVE_X[[1]], 2)
  expect_length(result.sgcca$AVE$AVE_X[[2]], 2)
  expect_length(result.sgcca$AVE$AVE_X[[3]], 1)

  expect_true(all(!is.na(result.sgcca$AVE$AVE_X[[1]])))
  expect_true(all(!is.na(result.sgcca$AVE$AVE_X[[2]])))
  expect_true(all(!is.na(result.sgcca$AVE$AVE_X[[3]])))

  expect_length(result.sgcca$Y, 3)
  expect_length(result.sgcca$a, 3)

  expect_true(is(result.sgcca$a[[1]], "matrix"))
  expect_true(is(result.sgcca$a[[2]], "matrix"))
  expect_true(is(result.sgcca$a[[3]], "matrix"))

  expect_true(is(result.sgcca$Y[[1]], "matrix"))
  expect_true(is(result.sgcca$Y[[2]], "matrix"))
  expect_true(is(result.sgcca$Y[[3]], "matrix"))

  expect_true(all(!is.na(result.sgcca$Y[[1]][1, ])))
  expect_true(all(!is.na(result.sgcca$Y[[2]][1, ])))
  expect_true(all(!is.na(result.sgcca$Y[[3]][1, ])))

  expect_true(all(!is.na(result.sgcca$a[[1]][1, ])))
  expect_true(all(!is.na(result.sgcca$a[[2]][1, ])))
  expect_true(all(!is.na(result.sgcca$a[[3]][1, ])))

  expect_equal(dim(result.sgcca$a[[1]]), c(15702L, 2L))
})


test_that("errors solved", {
  skip_on_travis()
  data("ge_cgh_locIGR", package = "gliomaData")
  A <- ge_cgh_locIGR$multiblocks
  Loc <- factor(ge_cgh_locIGR$y)
  levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  tau <- c(1, 1, 0)
  shrinkage <- c(.071, .2, 1)
  ncomp <- c(2, 2)
  expect_error(sgcca(A, C = C, c1 = shrinkage, ncomp = ncomp),
               "The ncomp parameter should be ")
  C[1, 3] <- 0
  expect_error(sgcca(A, C = C, c1 = shrinkage, ncomp = ncomp),
               "symmetric and connected")
})



test_that("different number of samples", {
  skip_on_travis()
  set.seed(45791)
  data("ge_cgh_locIGR", package = "gliomaData")
  A <- ge_cgh_locIGR$multiblocks
  A[[1]] <- A[[1]][sample(1:53, size = 50), ]
  Loc <- factor(ge_cgh_locIGR$y)
  levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  tau <- c(1, 1, 0)

  expect_error(sgcca(A, C,
                        c1 = c(.071, .2, 1), ncomp = c(2, 2, 1),
                        scheme = "centroid", verbose = FALSE
  ), "number of samples")

})


test_that("#32 input are not matrices", {
  path <- test_path("issue_data.RDS")
  if (file.exists(path)) {
    B <- readRDS(path)
  } else {
    testthat::skip("No data to test")
  }
  shrinkage <- c(RNAseq = 0.486223918802408, micro = 0.938776330169865, 0.5,
                 y = 1)
  m <- structure(c(0, 0, 0.9, 0.5, 0, 0, 0, 0.1, 0.9, 0, 0, 0, 0.5,
                   0.1, 0, 0), .Dim = c(4L, 4L), .Dimnames = list(NULL, NULL))
  expect_error(sgcca(B, C = m, scale = FALSE, c1 = shrinkage, ncomp = rep(1, 4)))
  B2 <- lapply(B, as.matrix)
  r <- sgcca(B2, C = m, scale = FALSE, c1 = shrinkage, ncomp = rep(1, 4))
})
