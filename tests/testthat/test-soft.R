set.seed(45)
x <- rnorm(10)
d <- rnorm(10)

test_that("soft works", {
  out <- soft(x, d)
  expect_equal(out, c(1.47121790923518, -0.487351414498001, 0, 0,
                      -0.496556703149629, -0.607778142376554,
                      -0.537530495736905, -0.324846931160383, 0,
                      -1.88260095097369))
})

test_that("equivalent (but slower on big numbers)",  {
  s <- soft(x, d)
  out <- sign(x) * ifelse(abs(x) - d > 0, abs(x) - d, 0)
  expect_equal(out, s)
  out <- sign(x) * do.call(pmax, list(0, abs(x) - d))
  expect_equal(out, s)
})
