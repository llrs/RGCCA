ave_inner <- function(C, Y) {
  sum(C * cor(Y)^2/2)/(sum(C)/2)
}

ave_outer <- function(AVE_X, pjs) {
  sum(pjs * unlist(AVE_X)) / sum(pjs)
}


ave_x <- function(A, Y) {
  AVE_X <- vector("list", length = length(A))
  for (b in seq_along(A)) {
    # Average Variance Explained (AVE) per block
    corAY <- cor(A[[b]], Y[[b]])^2
    AVE_X[[b]] <- mean(corAY)
  }
  AVE_X
}
