context("Testing sgcca")

test_that("centroid", {
  skip_on_travis()
  set.seed(45791)
  data("ge_cgh_locIGR", package = "gliomaData")
  A <- ge_cgh_locIGR$multiblocks
  Loc <- factor(ge_cgh_locIGR$y)
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
  expect_equal(sum(result.sgcca$a[[2]] != 0), 161L)
  expect_equal(sum(result.sgcca$a[[3]] != 0), 3L)

  expect_equal(
    result.sgcca$crit[[2]],
    c(
      0.0705922441625706, 0.0723431879010189, 0.0912348678837695,
      0.119025897140844, 0.12238825814507, 0.122504903372083, 0.122508520626011,
      0.122508633245961, 0.122508636752987, 0.122508636870408, 0.122508636873339,
      0.122508636867339, 0.122508636867962, 0.122508636870334, 0.122508636865809,
      0.122508636862402
    )
  )
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



test_that("horst", {
  skip_on_travis()
  data("ge_cgh_locIGR", package = "gliomaData")
  A <- ge_cgh_locIGR$multiblocks
  Loc <- factor(ge_cgh_locIGR$y)
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

  expect_equal(result.sgcca$C, C)
  expect_equal(result.sgcca$ncomp, c(2L, 2L, 1L))
  expect_length(result.sgcca$crit, 2L)
  expect_equal(
    result.sgcca$crit[[2]],
    c(0.0705922441625706, 0.0723431879010189, 0.0912348678837695,
      0.119025897140844, 0.12238825814507, 0.122504903372083, 0.122508520626011,
      0.122508633245961, 0.122508636752987, 0.122508636870408, 0.122508636873339,
      0.122508636867339, 0.122508636867962, 0.122508636870334, 0.122508636865809,
      0.122508636862402)
  )
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


test_that("factorial", {
  skip_on_travis()
  data("ge_cgh_locIGR", package = "gliomaData")
  A <- ge_cgh_locIGR$multiblocks
  Loc <- factor(ge_cgh_locIGR$y)
  levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  tau <- c(1, 1, 0)

  result.sgcca <- sgcca(A, C,
                        c1 = c(.071, .2, 1), ncomp = c(2, 2, 1),
                        scheme = "factorial", verbose = FALSE
  )

  expect_equal(result.sgcca$C, C)
  expect_equal(result.sgcca$ncomp, c(2L, 2L, 1L))
  expect_length(result.sgcca$crit, 2L)

  expect_equal(sum(result.sgcca$a[[1]] != 0), 293L)
  expect_equal(sum(result.sgcca$a[[2]] != 0), 156L)
  expect_equal(sum(result.sgcca$a[[3]] != 0), 3L)

  expect_equal(
    result.sgcca$crit[[2]],
    c(0.00188849566851127, 0.002530558152751, 0.00385094951153514,
      0.00418723310327225, 0.00421178622074041, 0.00421317198840081,
      0.00421324829755606, 0.00421325230938456, 0.00421325251633106,
      0.00421325252701272, 0.00421325252767922, 0.0042132525277674,
      0.00421325252771664, 0.0042132525279121, 0.00421325252778345,
      0.00421325252797521, 0.00421325252769357)
  )
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
