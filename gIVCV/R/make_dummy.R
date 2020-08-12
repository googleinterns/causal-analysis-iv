make_dummy <- function(fac_var) {
  fac_var_ <- factor(fac_var)
  return(model.matrix( ~ -1 + fac_var_) %>%
           as_data_frame())
}
