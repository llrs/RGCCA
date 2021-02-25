<!-- badges: start -->
[![R-CMD-check](https://github.com/llrs/RGCCA/workflows/R-CMD-check/badge.svg)](https://github.com/llrs/RGCCA/actions)
[![Travis build status](https://travis-ci.org/llrs/RGCCA.svg?branch=master)](https://travis-ci.org/llrs/RGCCA)
[![Coverage status](https://codecov.io/gh/llrs/RGCCA/branch/master/graph/badge.svg)](https://codecov.io/github/llrs/RGCCA?branch=master)
<!-- badges: end -->

# RGCCA

The goal of RGCCA is to provide Regularized Canonical Correlation Analysis.
This fork is for better understanding RGCCA and test the results.

## Installation

You can install the released version of RGCCA from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("RGCCA")
```

And this fork from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("llrs/RGCCA")
```
## Example

This is a basic example which shows you how the Agricultural inequality, the industrial development and the political enviroment classify some countries in 1964:

``` r
data(Russett)
X_agric =as.matrix(Russett[,c("gini","farm","rent")])
X_ind = as.matrix(Russett[,c("gnpr","labo")])
X_polit = as.matrix(Russett[ , c("demostab", "dictator")])
A = list(X_agric, X_ind, X_polit)
#Define the design matrix (output = C)
C = matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
result.rgcca = rgcca(A, C, tau = c(1, 1, 1), scheme = "factorial", scale = TRUE)
lab = as.vector(apply(Russett[, 9:11], 1, which.max))
plot(result.rgcca$Y[[1]], result.rgcca$Y[[2]], col = "white",
     xlab = "Y1 (Agric. inequality)", ylab = "Y2 (Industrial Development)")
text(result.rgcca$Y[[1]], result.rgcca$Y[[2]], rownames(Russett), col = lab, cex = .7)
```

