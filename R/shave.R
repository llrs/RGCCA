
shave <- function(m, nbcomp) {
  UseMethod("shave")
}

shaving <- function(m, nbcomp) {
  UseMethod("shaving")
}

#' @export
shave.list <- function(m, nbcomp) {
  mapply(shaving, m, nbcomp, SIMPLIFY = FALSE)
}

#' @export
shaving.defult <- function(m, nbcomp) {
  m[seq_len(nbcomp)]
}

#' @export
shaving.matrix <- function(m, nbcomp) {
  m[, seq_len(nbcomp), drop = FALSE]
}

#' @export
shaving.vector <- function(m, nbcomp) {
  m[seq_len(nbcomp)]
}

#' @export
shaving.numeric <- function(m, nbcomp) {
  m[seq_len(nbcomp)]
}

#' @export
shaving.double <- function(m, nbcomp) {
  m[seq_len(nbcomp)]
}
