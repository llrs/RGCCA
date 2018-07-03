initsvd <- function(X) {
  n <- NROW(X)
  p <- NCOL(X)
  if(n >= p){
    svd(X, nu = 0, nv = 1)$v
  } else {
    svd(X, nu = 1, nv = 0)$u
  }
}
