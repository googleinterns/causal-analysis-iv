#' @title Helper function - make dummy matrices.
#' @param fac_var take a vector and turn into a dummy coded matrix.
#' @example
#' a = 1:5
#' make_dummy(a)
make_dummy <- function(fac_var) {
  fac_var_ <- factor(fac_var)
  return(model.matrix( ~ -1 + fac_var_) %>%
           as_data_frame())
}
