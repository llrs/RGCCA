deflation <- function(X, y) {
  # Computation of the residual matrix R
  # Computation of the vector p.
  # p <- t(X)%*%y/as.vector(crossprod(y))
  p <- drop(crossprod(X, y)) / as.vector(crossprod(y))
  R <- X - y %*% t(p)
  return(list(p = p, R = R))
}
