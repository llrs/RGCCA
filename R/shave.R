
shave <- function(m, nbcomp) {
  UseMethod("shave")
}

shaving <- function(m, nbcomp) {
  UseMethod("shaving")
}

shave.list <- function(m, nbcomp) {
  mapply(shaving, m, nbcomp, SIMPLIFY = FALSE)
}

shaving.defult <- function(m, nbcomp) {
  m[seq_len(nbcomp)]
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
shaving.double <- function(m, nbcomp) {
  m[seq_len(nbcomp)]
}
