threshold <- function(X, q, sd_ctrl, X_return){
  ifelse(((1 - pnorm(abs(
    X
  ), sd = sd_ctrl)) < q), X_return, 0)
}
