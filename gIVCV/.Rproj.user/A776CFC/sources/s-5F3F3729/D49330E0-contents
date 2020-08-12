#' @title Helper function - repeating rows.
#' @example
#' J = 20
#' matrix(seq(0.1, 1, length.out = J)) %>% rep_row(3)

rep_row <- function(mat, times) {
  return(do.call(rbind, map(seq_len(times), ~ mat)))
}


