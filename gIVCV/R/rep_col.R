rep_col <- function(mat, times) {
  return(do.call(cbind, map(seq_len(times), ~mat)))
}

#'
#'  @example
#'  J = 20
#' matrix(seq(0.1, 1, length.out = J)) %>% rep_col(3)

#'rep_col(matrix(seq(0.1, 1, length.out = J)) , 3)
#'map(seq_len(3), ~seq(0.1, 1, length.out = J))
#'map(seq_len(3), ~5)
