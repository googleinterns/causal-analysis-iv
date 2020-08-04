rep_row <- function(mat, times) {
  return(do.call(cbind, map(seq_len(times), mat)))
}
