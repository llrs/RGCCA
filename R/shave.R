
shave.veclist <- function(vec_list, nb_elts) {
  mapply(function(m, nbcomp) {
    m[seq_len(nbcomp)]},
    vec_list, nb_elts, SIMPLIFY=FALSE)}


shave.matlist <- function(mat_list, nb_cols) {
  mapply(function(m, nbcomp) {
    m[, seq_len(nbcomp), drop = FALSE]},
    mat_list, nb_cols, SIMPLIFY=FALSE)

}
