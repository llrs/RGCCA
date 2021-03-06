# print.rgcca <- function(x,verbose=FALSE,...) {
#   nbloc <- length(x$Y)
#   cat("Outputs for RGCCA: \n")
# }

#' Regularized Generalized Canonical Correlation Analysis (RGCCA)
#'
#' Regularized Generalized Canonical Correlation Analysis (RGCCA) is a generalization
#' of regularized canonical correlation analysis to three or more sets of variables.
#'
#' Given \eqn{J} matrices \eqn{X_1, X_2, ..., X_J} that represent
#' \eqn{J} sets of variables observed on the same set of \eqn{n} individuals. The matrices
#' \eqn{X_1, X_2, ..., X_J} must have the same number of rows,
#' but may (and usually will) have different numbers of columns. The aim of RGCCA is to study
#' the relationships between these \eqn{J} blocks of variables. It constitutes a general
#' framework for many multi-block data analysis methods. It combines the power of
#' multi-block data analysis methods (maximization of well identified criteria)
#' and the flexibility of PLS path modeling (the researcher decides which blocks
#' are connected and which are not). Hence, the use of RGCCA requires the construction
#' (user specified) of a design matrix \eqn{C}, that characterize
#' the connections between blocks. Elements of the symmetric design matrix \eqn{C = (c_{jk})}
#' is equal to 1 if block \eqn{j} and block \eqn{k} are connected, and 0 otherwise.
#' The function rgcca() implements a monotonically convergent algorithm (i.e. the bounded
#' criteria to be maximized increases at each step of the iterative procedure) that is very
#' similar to the PLS algorithm proposed by Herman Wold and finds at convergence a stationnary point
#' of the RGCCA optimization problem. . Moreover, depending on the
#' dimensionality of each block \eqn{X_j}, \eqn{j = 1, ..., J}, the primal (when \eqn{n > p_j}) algorithm or
#' the dual (when \eqn{n < p_j}) algorithm is used (see Tenenhaus et al. 2015).
#' Moreover, by deflation strategy, rgcca() allow to compute several RGCCA block
#' components (specified by ncomp) for each block. Within each block, block components are guaranteed to
#' be orthogonal using the deflation procedure. The so-called symmetric deflation is considered in
#' this implementation, i.e. each block is deflated with respect to its own component(s).
#' It should be noted that the numbers of components per block can differ from one block to another.
#' @param A  A list that contains the \eqn{J} blocks of variables \eqn{X_1, X_2, ..., X_J}.
#' @param C  A design matrix that describes the relationships between blocks (default: complete design).
#' @param tau tau is either a \eqn{1 * J} vector or a \eqn{max(ncomp) * J} matrix, and contains the values
#' of the shrinkage parameters (default: tau = 1, for each block and each dimension).
#' If tau = "optimal" the shrinkage paramaters are estimated for each block and each dimension using the Schafer and Strimmer (2005)
#' analytical formula . If tau is a \eqn{1* J} numeric vector, tau[j] is identical across the dimensions of block \eqn{X_j}.
#' If tau is a matrix, tau[k, j] is associated with \eqn{X_{jk}} (\eqn{k}th residual matrix for block \eqn{j})
#' @param ncomp  A \eqn{1 * J} vector that contains the numbers of components for each block (default: rep(1, length(A)), which gives one component per block.)
#' @param scheme The value is "horst", "factorial", "centroid" or any diffentiable convex scheme function g designed by the user (default: "centroid").
#' @param scale  If scale = TRUE, each block is standardized to zero means and unit variances and then divided by the square root of its number of variables (default: TRUE).
#' @param verbose  If verbose = TRUE, the progress will be report while computing (default: TRUE).
#' @param init The mode of initialization to use in RGCCA algorithm. The alternatives are either by Singular Value Decompostion ("svd") or random ("random") (Default: "svd").
#' @param bias A logical value for biaised or unbiaised estimator of the var/cov (default: bias = TRUE).
#' @param tol The stopping value for convergence.
#' @return \item{Y}{A list of \eqn{J} elements. Each element of \eqn{Y} is a matrix that contains the RGCCA components for the corresponding block.}
#' @return \item{a}{A list of \eqn{J} elements. Each element of \eqn{a} is a matrix that contains the outer weight vectors for each block.}
#' @return \item{astar}{A list of \eqn{J} elements. Each element of astar is a matrix defined as Y[[j]][, h] = A[[j]]\%*\%astar[[j]][, h].}
#' @return \item{C}{A design matrix that describes the relation between blocks (user specified).}
#' @return \item{tau}{A vector or matrix that contains the values of the shrinkage parameters applied to each block and each dimension (user specified).}
#' @return \item{scheme}{The scheme chosen by the user (user specified).}
#' @return \item{ncomp}{A \eqn{1 * J} vector that contains the numbers of components for each block (user specified).}
#' @return \item{crit}{A vector that contains the values of the criteria across iterations.}
#' @return \item{primal_dual}{A \eqn{1 * J} vector that contains the formulation ("primal" or "dual") applied to each of the \eqn{J} blocks within the RGCCA alogrithm}
#' @return \item{AVE}{indicators of model quality based on the Average Variance Explained (AVE): AVE(for one block), AVE(outer model), AVE(inner model).}
#' @references Tenenhaus M., Tenenhaus A. and Groenen PJF (2017), Regularized generalized canonical correlation analysis: A framework for sequential multiblock component methods, Psychometrika, in press
#' @references Tenenhaus A., Philippe C., & Frouin V. (2015). Kernel Generalized Canonical Correlation Analysis. Computational Statistics and Data Analysis, 90, 114-131.
#' @references Tenenhaus A. and Tenenhaus M., (2011), Regularized Generalized Canonical Correlation Analysis, Psychometrika, Vol. 76, Nr 2, pp 257-284.
#' @references Schafer J. and Strimmer K., (2005), A shrinkage approach to large-scale covariance matrix estimation and implications for functional genomics. Statist. Appl. Genet. Mol. Biol. 4:32.
#' @examples
#' #############
#' # Example 1 #
#' #############
#' data(Russett)
#' X_agric =as.matrix(Russett[,c("gini","farm","rent")])
#' X_ind = as.matrix(Russett[,c("gnpr","labo")])
#' X_polit = as.matrix(Russett[ , c("demostab", "dictator")])
#' A = list(X_agric, X_ind, X_polit)
#' #Define the design matrix (output = C)
#' C = matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' result.rgcca = rgcca(A, C, tau = c(1, 1, 1), scheme = "factorial", scale = TRUE)
#' lab = as.vector(apply(Russett[, 9:11], 1, which.max))
#' plot(result.rgcca$Y[[1]], result.rgcca$Y[[2]], col = "white",
#'      xlab = "Y1 (Agric. inequality)", ylab = "Y2 (Industrial Development)")
#' text(result.rgcca$Y[[1]], result.rgcca$Y[[2]], rownames(Russett), col = lab, cex = .7)
#'
#' #############
#' # Example 2 #
#' #############
#' data(Russett)
#' X_agric =as.matrix(Russett[,c("gini","farm","rent")])
#' X_ind = as.matrix(Russett[,c("gnpr","labo")])
#' X_polit = as.matrix(Russett[ , c("inst", "ecks", "death",
#'                                  "demostab", "dictator")])
#' A = list(X_agric, X_ind, X_polit, cbind(X_agric, X_ind, X_polit))
#'
#' #Define the design matrix (output = C)
#' C = matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0), 4, 4)
#' result.rgcca = rgcca(A, C, tau = c(1, 1, 1, 0), ncomp = rep(2, 4),
#'                      scheme = function(x) x^4, scale = TRUE) # HPCA
#' lab = as.vector(apply(Russett[, 9:11], 1, which.max))
#' plot(result.rgcca$Y[[4]][, 1], result.rgcca$Y[[4]][, 2], col = "white",
#'      xlab = "Global Component 1", ylab = "Global Component 2")
#' text(result.rgcca$Y[[4]][, 1], result.rgcca$Y[[4]][, 2], rownames(Russett),
#'      col = lab, cex = .7)
#'
#' \dontrun{
#' ######################################
#' # example 3: RGCCA and leave one out #
#' ######################################
#' Ytest = matrix(0, 47, 3)
#' X_agric =as.matrix(Russett[,c("gini","farm","rent")])
#' X_ind = as.matrix(Russett[,c("gnpr","labo")])
#' X_polit = as.matrix(Russett[ , c("demostab", "dictator")])
#' A = list(X_agric, X_ind, X_polit)
#' #Define the design matrix (output = C)
#' C = matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' result.rgcca = rgcca(A, C, tau = rep(1, 3), ncomp = rep(1, 3),
#'                      scheme = "factorial", verbose = TRUE)
#'
#' for (i in 1:nrow(Russett)){
#'  B = lapply(A, function(x) x[-i, ])
#'  B = lapply(B, scale2)
#'  resB = rgcca(B, C, tau = rep(1, 3), scheme = "factorial", scale = FALSE, verbose = FALSE)
#'  #  look for potential conflicting sign among components within the loo loop.
#'  for (k in 1:length(B)){
#'    if (cor(result.rgcca$a[[k]], resB$a[[k]]) >= 0)
#'      resB$a[[k]] = resB$a[[k]] else resB$a[[k]] = -resB$a[[k]]
#'  }
#'  Btest =lapply(A, function(x) x[i, ])
#'  Btest[[1]]=(Btest[[1]]-attr(B[[1]],"scaled:center")) /
#'                  (attr(B[[1]],"scaled:scale"))/sqrt(NCOL(B[[1]]))
#'  Btest[[2]]=(Btest[[2]]-attr(B[[2]],"scaled:center")) /
#'                  (attr(B[[2]],"scaled:scale"))/sqrt(NCOL(B[[2]]))
#'  Btest[[3]]=(Btest[[3]]-attr(B[[3]],"scaled:center")) /
#'                  (attr(B[[3]],"scaled:scale"))/sqrt(NCOL(B[[3]]))
#'  Ytest[i, 1] = Btest[[1]]%*%resB$a[[1]]
#'  Ytest[i, 2] = Btest[[2]]%*%resB$a[[2]]
#'  Ytest[i, 3] = Btest[[3]]%*%resB$a[[3]]
#' }
#' lab = apply(Russett[, 9:11], 1, which.max)
#' plot(result.rgcca$Y[[1]], result.rgcca$Y[[2]], col = "white",
#'      xlab = "Y1 (Agric. inequality)", ylab = "Y2 (Ind. Development)")
#' text(result.rgcca$Y[[1]], result.rgcca$Y[[2]], rownames(Russett),
#'      col = lab, cex = .7)
#' text(Ytest[, 1], Ytest[, 2], substr(rownames(Russett), 1, 1),
#'      col = lab, cex = .7)
#' }
#' @export rgcca


rgcca <- function(A, C = 1 - diag(length(A)), tau = rep(1, length(A)), ncomp = rep(1, length(A)), scheme = "centroid", scale = TRUE, init="svd", bias = TRUE, tol = 1e-8, verbose=TRUE) {
  if (any(ncomp < 1)) {
    stop("Compute at least one component per block!")
  }
  pjs <- vapply(A, NCOL, numeric(1L))
  js <- vapply(A, NROW, numeric(1L))

  if (length(unique(js)) != 1) {
    stop("The data don't have the same number of samples.")
  }

  nb_row <- NROW(A[[1]])
  if (any(ncomp - pjs > 0)) {
    stop("For each block, choose a number of components smaller than the number of variables!")
  }

  if ((is.vector(tau) && length(tau) != length(A) || any(is.na(tau)) ||
      !is.numeric(tau)) && (tau != "optimal")) {
    stop("The shrinkage parameters should be of the same length as the input",
         "data, or 'optimal'")
  }
  if (length(ncomp) != length(A) && all(ncomp >= 1)) {
    stop("The ncomp parameter should be of the same length as the input data")
  }
  #-------------------------------------------------------
  if (mode(scheme) != "function") {
    if ((scheme != "horst") & (scheme != "factorial") & (scheme != "centroid")) {
      stop("Choose one of the three following schemes: horst, centroid, factorial or design the g function")
    }
    if (verbose) message("Computation of the RGCCA block components based on the ", scheme, " scheme")
  }
  if (mode(scheme) == "function" & verbose) {
    message("Computation of the RGCCA block components based on the g scheme")
  }

  if (!init %in% c("svd", "random")) {
    stop("init should be either random or by SVD.")
  }
  #-------------------------------------------------------

  if (scale == TRUE) {
    A <- lapply(A, scale2_, bias = bias)
  }

  if (!is.numeric(tau) & verbose) {
    message("Optimal Shrinkage intensity paramaters are estimated")
  }
  else {
    if (is.numeric(tau) & verbose) {
      message("Shrinkage intensity paramaters are chosen manually")
    }
  }
  J <- length(A)

  if (!correct(C)) {
    stop("Design matrix should be symmetric and connected")
  }
  if (ncol(C) != J) {
    stop("Design matrix should match the number of blocks provided")
  }

  AVE_X <- vector("list", length = J)
  AVE_outer <- vector()
  ndefl <- ncomp - 1
  N <- max(ndefl)
  nb_ind <- NROW(A[[1]])
  primal_dual <- rep("primal", J)
  primal_dual[which(nb_row < pjs)] <- "dual"
  Y <- vector("list", length = J)


  if (N == 0) {
    result <- rgccak(A, C, tau = tau, scheme = scheme, init = init, bias = bias,
                     tol = tol, verbose = verbose)
    a <- lapply(result$a, cbind)
    for (b in seq_len(J)) {
      Y[[b]] <- result$Y[, b, drop = FALSE]

      # Average Variance Explained (AVE) per block
      rownames(a[[b]]) <- colnames(A[[b]])
      rownames(Y[[b]]) <- rownames(A[[b]])
      colnames(Y[[b]]) <- "comp1"
    }
    # AVE
    AVE_X <- ave_x(A, Y)
    AVE_outer <- sum(pjs * unlist(AVE_X)) / sum(pjs)

    AVE <- list(
      AVE_X = AVE_X,
      AVE_outer = AVE_outer,
      AVE_inner = result$AVE_inner
    )

    out <- list(
      Y = Y, a = a, astar = a, C = C,
      tau = result$tau, scheme = scheme,
      ncomp = ncomp, crit = result$crit,
      primal_dual = primal_dual,
      AVE = AVE
    )

    class(out) <- "rgcca"
    return(out)
  } else {
    crit <- vector("list", length = N)
    AVE_inner <- rep(NA, max(ncomp))

    R <- A
    P <- a <- astar <- NULL

    if (is.numeric(tau)) {
      tau_mat <- tau
    } else {
      tau_mat <- matrix(NA, max(ncomp), J)
    }

    for (b in seq_len(J)) {
      P[[b]] <- a[[b]] <- astar[[b]] <- matrix(NA, pjs[[b]], N + 1)
      Y[[b]] <- matrix(NA, nb_ind, N + 1)
    }

    for (n in seq_len(N)) {
      if (verbose) {
        message("Computation of the RGCCA block components #", n, " is under progress...")
      }
      if (is.vector(tau)) {
        rgcca.result <- rgccak(R, C, tau = tau, scheme = scheme, init = init, bias = bias, tol = tol, verbose = verbose)
      } else {
        rgcca.result <- rgccak(R, C, tau = tau[n, ], scheme = scheme, init = init, bias = bias, tol = tol, verbose = verbose)
      }
      if (!is.numeric(tau)) {
        tau_mat[n, ] <- rgcca.result$tau
      }
      AVE_inner[n] <- rgcca.result$AVE_inner
      crit[[n]] <- rgcca.result$crit

      defla.result <- defl.select(rgcca.result$Y, R, ndefl, n, nbloc = J)
      R <- defla.result$resdefl
      for (b in seq_len(J)) {
        Y[[b]][, n] <- rgcca.result$Y[, b]
        P[[b]][, n] <- defla.result$pdefl[[b]]
        a[[b]][, n] <- rgcca.result$a[[b]]
      }
      if (n == 1) {
        for (b in seq_len(J)) {
          astar[[b]][, n] <- rgcca.result$a[[b]]
        }
      } else {
        for (b in seq_len(J)) {
          astar[[b]][, n] <- rgcca.result$a[[b]] - astar[[b]][, (seq_len(n) - 1), drop = FALSE] %*% drop(crossprod(a[[b]][, n], P[[b]][, 1:(n - 1), drop = FALSE]))
        }
      }
    }

    if (verbose) {
      message("Computation of the RGCCA block components #", N + 1, " is under progress ...")
    }
    if (is.vector(tau)) {
      rgcca.result <- rgccak(R, C, tau = tau, scheme = scheme, init = init, bias = bias, tol = tol, verbose = verbose)
    } else {
      rgcca.result <- rgccak(R, C, tau = tau[N + 1, ], scheme = scheme, init = init, bias = bias, tol = tol, verbose = verbose)
    }
    crit[[N + 1]] <- rgcca.result$crit
    if (!is.numeric(tau)) {
      tau_mat[N + 1, ] <- rgcca.result$tau
    }
    AVE_inner[max(ncomp)] <- rgcca.result$AVE_inner

    for (b in seq_len(J)) {
      Y[[b]][, N + 1] <- rgcca.result$Y[, b]
      a[[b]][, N + 1] <- rgcca.result$a[[b]]
      astar[[b]][, N + 1] <- rgcca.result$a[[b]] - astar[[b]][, (seq_len(N)), drop = FALSE] %*% drop(crossprod(a[[b]][, (N + 1)], P[[b]][, 1:(N), drop = FALSE]))
      rownames(a[[b]]) <- rownames(astar[[b]]) <- colnames(A[[b]])
      rownames(Y[[b]]) <- rownames(A[[b]])
      colnames(Y[[b]]) <- paste0("comp", seq_len(max(ncomp)))

      # Average Variance Explained (AVE) per block
      AVE_X[[b]] <- apply(cor(A[[b]], Y[[b]])^2, 2, mean)
    }

    # AVE outer
    outer <- matrix(unlist(AVE_X), nrow = max(ncomp))
    AVE_outer <- as.numeric((outer %*% pjs)/sum(pjs))

    AVE <- list(
      AVE_X = shave(AVE_X, ncomp),
      AVE_outer_model = AVE_outer,
      AVE_inner_model = AVE_inner
    )

    out <- list(
      Y = shave(Y, ncomp),
      a = shave(a, ncomp),
      astar = shave(astar, ncomp),
      C = C, tau = tau_mat,
      scheme = scheme, ncomp = ncomp, crit = crit,
      primal_dual = primal_dual,
      AVE = AVE
    )

    class(out) <- "rgcca"
    return(out)
  }
}
