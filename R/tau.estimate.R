#' Estimation of the optimal shrinkage parameters as described in [1,2] and implemented
#' in a more general version within the SHIP package [2].
#' @param x  Data set on which the covariance matrix is estimated.
#' @return \item{tau}{Optimal shrinkage intensity parameter}
#' @title Optimal shrinkage intensity parameters.
#' @references [1] Schaefer J. and Strimmer K., 2005. A shrinkage approach to large-scale covariance matrix estimation and implications for functional genomics. Statist. Appl. Genet. Mol. Biol. 4:32.
#' @references [2] Jelizarow M., Guillemot V., Tenenhaus A., Strimmer K., Boulesteix A.-L., 2010. Over-optimism in bioinformatics: an illustration. Bioinformatics 26:1990-1998.
#' @export tau.estimate
#' @importFrom WGCNA cor
#' @importFrom rfunctions crossprodcpp
#' @examples
#' n.obs <- 1e5
#' n.vars <- 150
#' x <- matrix(rnorm(n.obs * n.vars), n.obs, n.vars)
#' tau <- tau.estimate(x)
#' tau
tau.estimate <- function(x) {
  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE) {
    stop("The data matrix must be numeric!")
  }
  n <- NROW(x)
  corm <- WGCNA::cor(x, use = 'pairwise.complete.obs')
  xs <- scale(x, center = TRUE, scale = TRUE)
  v <- (n / ((n - 1)^3)) * (rfunctions::crossprodcpp(xs^2) - 1 / n * (
    rfunctions::crossprodcpp(xs))^2)
  diag(v) <- 0
  I <- diag(NCOL(x))
  d <- (corm - I)^2
  tau <- (sum(v)) / sum(d)
  max(min(tau, 1), 0)
}


scale_col <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  nc <- ncol(x)
  n <- nrow(x) - 1
  for (i in seq_len(nc)) {
    centered <- x[, i] - mean(x[, i])
    x[, i] <- centered/sd_helper(centered, n)
  }
  x
}

# Sd but taking advantage of :
# That it doesn't have NAs
# That we know the number of samples
sd_helper <- function(v, n) {
  sqrt(sum(v^2)/n)
}
