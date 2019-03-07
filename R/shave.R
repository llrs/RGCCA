
shave <- function(m, nbcomp) {
  UseMethod("shave")
}

shave.list <- function(m, nbcomp) {
  mapply(shaving, m, nbcomp, SIMPLIFY = FALSE)
}

shaving <- function(m, nbcomp) {
  UseMethod("shaving")
}

shaving.matrix <- function(m, nbcomp) {
  m[, seq_len(nbcomp), drop = FALSE]
}

shaving.vector <- function(m, nbcomp) {
  m[seq_len(nbcomp)]
}

shaving.numeric <- function(m, nbcomp) {
  m[seq_len(nbcomp)]
}
