#' @title Variable Selection For Generalized Canonical Correlation Analysis 2 (SGCCA2)
#' SGCCA2 extends SGCCA to address the issue following the same design in all the dimensions.
#' @param A  A list that contains the \eqn{J} blocks of variables \eqn{X_1, X_2, ..., X_J}.
#' @param C  A list that contains the design matrix that describes the relationships between blocks (default: complete design).
#' @param c1 Either a \eqn{1*J} vector or a \eqn{max(ncomp) * J} matrix encoding the L1 constraints applied to the outer weight vectors.
#' Elements of c1 vary between \eqn{1/sqrt(p_j)} and 1 (larger values of c1 correspond to less penalization).
#' If c1 is a vector, L1-penalties are the same for all the weights corresponding to the same block but different components:
#' \deqn{for all h, |a_{j,h}|_{L_1} \le c_1[j] \sqrt{p_j},}
#' with \eqn{p_j} the number of variables of \eqn{X_j}.
#' If c1 is a matrix, each row \eqn{h} defines the constraints applied to the weights corresponding to components \eqn{h}:
#' \deqn{for all h, |a_{j,h}|_{L_1} \le c_1[h,j] \sqrt{p_j}.}
#' @param ncomp  A \eqn{1*J} vector that contains the numbers of components for each block (default: rep(1, length(A)), which means one component per block).
#' @param scheme Either  "horst", "factorial" or "centroid" (Default: "centroid").
#' @param scale  If scale = TRUE, each block is standardized to zero means and unit variances and then divided by the square root of its number of variables (default: TRUE).
#' @param init Mode of initialization use in the SGCCA algorithm, either by Singular Value Decompostion ("svd") or random ("random") (default : "svd").
#' @param bias A logical value for biaised or unbiaised estimator of the var/cov.
#' @param verbose  Will report progress while computing if verbose = TRUE (default: FALSE).
#' @param tol Stopping value for convergence.
#' @return A list of class sgcca with the following elements:
#' @return \item{Y}{A list of \eqn{J} elements. Each element of Y is a matrix that contains the SGCCA components for each block.}
#' @return \item{a}{A list of \eqn{J} elements. Each element of a is a matrix that contains the outer weight vectors for each block.}
#' @return \item{astar}{A list of \eqn{J} elements. Each element of astar is a matrix defined as Y[[j]][, h] = A[[j]]\%*\%astar[[j]][, h]}
#' @return \item{C}{A design matrix that describes the relationships between blocks (user specified).}
#' @return \item{scheme}{The scheme chosen by the user (user specified).}
#' @return \item{c1}{A vector or matrix that contains the value of c1 applied to each block \eqn{\mathbf{X}_j}, \eqn{ j=1, \ldots, J} and each dimension (user specified).}
#' @return \item{ncomp}{A \eqn{1 \times J} vector that contains the number of components for each block (user specified).}
#' @return \item{crit}{A vector that contains the values of the objective function at each iterations.}
#' @return \item{AVE}{Indicators of model quality based on the Average Variance Explained (AVE): AVE(for one block), AVE(outer model), AVE(inner model).}
#' @references Tenenhaus, A., Philippe, C., Guillemot, V., Le Cao, K. A., Grill, J., and Frouin, V. , "Variable selection for generalized canonical correlation analysis.," Biostatistics, vol. 15, no. 3, pp. 569-583, 2014.
#' @examples
#'
#' #############
#' # Example 1 #
#' #############
#' \dontrun{
#' # Download the dataset's package at http://biodev.cea.fr/sgcca/.
#' # --> gliomaData_0.4.tar.gz
#'
#' require(gliomaData)
#' data(ge_cgh_locIGR)
#'
#' A <- ge_cgh_locIGR$multiblocks
#' Loc <- factor(ge_cgh_locIGR$y) ; levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
#' C1 <-  matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' C2 <-  matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' C <- list(C1, C2)
#' tau = c(1, 1, 0)
#'
#' # rgcca algorithm using the dual formulation for X1 and X2
#' # and the dual formulation for X3
#' A[[3]] = A[[3]][, -3]
#' # sgcca algorithm
#' result.sgcca = sgcca2(A, C, c1 = c(.071,.2, 1), ncomp = c(2, 2, 1),
#'                      scheme = "centroid", verbose = TRUE)
#'
#' ############################
#' # plot(y1, y2) for (SGCCA) #
#' ############################
#' layout(t(1:2))
#' plot(result.sgcca$Y[[1]][, 1], result.sgcca$Y[[2]][, 1], col = "white", xlab = "Y1 (GE)",
#'      ylab = "Y2 (CGH)", main = "Factorial plan of SGCCA")
#' text(result.sgcca$Y[[1]][, 1], result.sgcca$Y[[2]][, 1], Loc, col = as.numeric(Loc), cex = .6)
#'
#' plot(result.sgcca$Y[[1]][, 1], result.sgcca$Y[[1]][, 2], col = "white", xlab = "Y1 (GE)",
#'      ylab = "Y2 (GE) Dim 2.", main = "Factorial plan of SGCCA")
#' text(result.sgcca$Y[[1]][, 1], result.sgcca$Y[[1]][, 2], Loc, col = as.numeric(Loc), cex = .6)
#'
#' # sgcca algorithm with multiple components and different L1 penalties for each components
#' # (-> c1 is a matrix)
#' init = "random"
#' result.sgcca = sgcca2(A, C, c1 = matrix(c(.071,.2, 1, 0.06, 0.15, 1), nrow = 2, byrow = TRUE),
#'                      ncomp = c(2, 2, 1), scheme = "factorial", scale = TRUE, bias = TRUE,
#'                      init = init, verbose = TRUE)
#' # number of non zero elements per dimension
#' apply(result.sgcca$a[[1]], 2, function(x) sum(x!=0))
#'      #(-> 145 non zero elements for a11 and 107 non zero elements for a12)
#' apply(result.sgcca$a[[2]], 2, function(x) sum(x!=0))
#'      #(-> 85 non zero elements for a21 and 52 non zero elements for a22)
#' init = "svd"
#' result.sgcca = sgcca2(A, C, c1 = matrix(c(.071,.2, 1, 0.06, 0.15, 1), nrow = 2, byrow = TRUE),
#'                      ncomp = c(2, 2, 1), scheme = "factorial", scale = TRUE, bias = TRUE,
#'                      init = init, verbose = TRUE)}
#' @export
sgcca2 <- function(A, C = rep(1 - diag(length(A)), max(ncomp)),
                              c1 = rep(1, length(A)),
                              ncomp = rep(2, length(A)), scheme = "centroid",
                              scale = TRUE, init = "svd", bias = TRUE,
                              tol = .Machine$double.eps, verbose = FALSE) {

  # ndefl number of deflation per block
  ndefl <- ncomp - 1
  N <- max(ndefl)
  J <- length(A)

  if (!is(C, "list")) {
    stop("You must provide a list of designs. You might be interested in sgcca.")
  }
  C_dims <- sapply(C, dim)
  if (length(unique(C_dims[1, ])) != 1 | length(unique(C_dims[2, ])) != 1) {
    stop("Different sizes on the design matrices")
  }
  C_def <- Reduce(`+`, C)
  if (!correct(C_def)) {
    stop("Design matrix should be symmetric and connected")
  }
  if (ncol(C_def) != J) {
    stop("Design matrix should match the number of blocks provided")
  }
  if (length(C) < max(ncomp)) {
    stop("There must be one design for each component desired")
  }
  if (J < 2) {
    stop("Provide a list of several sets of variables")
  }

  if (length(c1) != length(A)) {
    stop("The shrinkage parameters should be of the same length as the input data")
  }

  if (length(ncomp) != length(A) && all(ncomp >= 1)) {
    stop("The ncomp parameter should be of the same length as the input data")
  }
  pjs <- vapply(A, NCOL, numeric(1L))
  nb_ind <- NROW(A[[1]])
  AVE_X <- list()
  AVE_outer <- rep(NA, max(ncomp))

  if (any(ncomp < 1)) {
    stop("One must compute at least one component per block!")
  }
  if (any(ncomp - pjs > 0)) {
    stop("For each block, choose a number of components smaller than the number of variables!")
  }
  if (is.vector(c1)) {
    if (any(c1 < 1 / sqrt(pjs) | c1 > 1)) {
      stop("L1 constraints (c1) must vary between 1/sqrt(p_j) and 1.")
    }
  } else if (is.matrix(c1)) {
    if (any(apply(c1, 1, function(x) any(x < 1 / sqrt(pjs))))) {
      stop("L1 constraints (c1) must vary between 1/sqrt(p_j) and 1.")
    }
  }

  init <- match.arg(init, c("svd", "random"))

  ###################################################

  if (mode(scheme) != "function") {
    if ((scheme != "horst") & (scheme != "factorial") & (scheme != "centroid")) {
      stop("Choose one of the three following schemes: horst, centroid, factorial or design the g function")
    }
    if (verbose) {
      message("Computation of the SGCCA block components based on the ", scheme, " scheme")
    }
  }
  if (mode(scheme) == "function" & verbose) {
    message("Computation of the SGCCA block components based on the g scheme")
  }


  #-------------------------------------------------------

  if (scale == TRUE) {
    A <- lapply(A, scale2_, bias = bias)
  }
  ####################################
  # sgcca with 1 component per block #
  ####################################

  if (N == 0) {
    stop("Must use sgcca not sgcca2")
  }

  ##################
  # Initialization #
  ##################

  Y <- vector("list", length = J)
  R <- A
  P <- a <- astar <- NULL
  crit <- list()
  AVE_inner <- rep(NA, max(ncomp))

  for (b in seq_len(J)) {
    P[[b]] <- a[[b]] <- astar[[b]] <- matrix(NA, pjs[[b]], N + 1)
    Y[[b]] <- matrix(NA, nb_ind, N + 1)
  }

  ##############################################
  #               If any ncomp > 1             #
  #      Determination of SGCCA components     #
  ##############################################

  for (n in seq_len(N)) {
    if (verbose) {
      message("Computation of the SGCCA block components #", n, " is under progress...")
    }
    if (is.vector(c1)) {
      sgcca.result <- sgccak(R, C[[n]], c1 = c1, scheme = scheme, init = init,
                             bias = bias, tol = tol, verbose = verbose)
    } else {
      sgcca.result <- sgccak(R, C[[n]], c1 = c1[n, ], scheme = scheme, init = init,
                             bias = bias, tol = tol, verbose = verbose)
    }
    AVE_inner[n] <- sgcca.result$AVE_inner
    crit[[n]] <- sgcca.result$crit

    defla.result <- defl.select(sgcca.result$Y, R, ndefl, n, nbloc = J)
    R <- defla.result$resdefl

    for (b in seq_len(J)) {
      Y[[b]][, n] <- sgcca.result$Y[, b]
      P[[b]][, n] <- defla.result$pdefl[[b]]
      a[[b]][, n] <- sgcca.result$a[[b]]
    }
    for (q in which(n < ndefl)) {
      if (sum(sgcca.result$a[[q]] != 0) <= 1) {
        warning("Deflation failed because only one variable was selected for block #", q, "!")
      }
    }

    if (n == 1) {
      for (b in seq_len(J)) {
        astar[[b]][, n] <- sgcca.result$a[[b]]
      }
    } else {
      for (b in seq_len(J)) {
        astar[[b]][, n] <- sgcca.result$a[[b]] - astar[[b]][, (seq_len(n) - 1), drop = FALSE] %*% drop(crossprod(a[[b]][, n], P[[b]][, seq_len(n - 1), drop = FALSE]))
      }
    }
  }

  if (verbose) {
    message("Computation of the SGCCA block components #", N + 1, " is under progress...")
  }
  if (is.vector(c1)) {
    sgcca.result <- sgccak(R, C[[N + 1]], c1 = c1, scheme = scheme, init = init,
                           bias = bias, tol = tol, verbose = verbose)
  } else {
    sgcca.result <- sgccak(R, C[[N + 1]], c1 = c1[N + 1, ], scheme = scheme,
                           init = init, bias = bias, tol = tol,
                           verbose = verbose)
  }
  AVE_inner[max(ncomp)] <- sgcca.result$AVE_inner

  crit[[N + 1]] <- sgcca.result$crit
  for (b in seq_len(J)) {
    Y[[b]][, N + 1] <- sgcca.result$Y[, b]
    a[[b]][, N + 1] <- sgcca.result$a[[b]]
    astar[[b]][, N + 1] <- sgcca.result$a[[b]] - astar[[b]][, (seq_len(N)), drop = FALSE] %*% drop(crossprod(a[[b]][, (N + 1)], P[[b]][, seq_len(N), drop = FALSE]))
    rownames(a[[b]]) <- rownames(astar[[b]]) <- colnames(A[[b]])
    rownames(Y[[b]]) <- rownames(A[[b]])
    colnames(Y[[b]]) <- paste0("comp", seq_len(max(ncomp)))

    # Average Variance Explained (AVE) per block
    AVE_X[[b]] <- apply(cor(A[[b]], Y[[b]])^2, 2, mean)
  }

  # AVE outer
  outer <- matrix(unlist(AVE_X), nrow = max(ncomp))
  AVE_outer <- as.numeric((outer %*% pjs)/sum(pjs))

  Y <- shave(Y, ncomp)
  AVE_X <- shave(AVE_X, ncomp)

  AVE <- list(
    AVE_X = AVE_X,
    AVE_outer = AVE_outer,
    AVE_inner = AVE_inner
  )

  out <- list(
    Y = shave(Y, ncomp),
    a = shave(a, ncomp),
    astar = shave(astar, ncomp),
    C = C, c1 = c1, scheme = scheme,
    ncomp = ncomp, crit = crit,
    AVE = AVE
  )
  class(out) <- "sgcca"
  return(out)
}
