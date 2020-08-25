#' @title Helper function - thresholding predictors.
#' @param X predictors.
#' @param q tuning parameter.
#' @param sd_ctrl sds of X.
#' @param X_return alternative elements to be returned.
#' @return X with some elements thresheld or new X_return.
threshold <- function(X, q, sd_ctrl, X_return){
  ifelse(((1 - pnorm(abs(
    X
  ), sd = sd_ctrl)) < q), X_return, 0)
}

#' @example
#' single dimension
#' threshold(1, q = .5, sd_ctrl = 1, X_return = 3)
#' threshold(1, q = .001, sd_ctrl = 1, X_return = 3)

#' multiple dimension
#' threshold(c(1, 1, 1), q = c(.5, .5, .001), sd_ctrl = 1, X_return = 3)
#' threshold(c(1, 1, 1), q = c(.5, .5, .001), sd_ctrl = c(.01, 5, 10), X_return = 3)
#' threshold(c(1, 1, 1), q = c(.5, .2, .001), sd_ctrl = c(.01, 5, 10), X_return = 3)

#' matrix
#' mat <- matrix(rep(1, 6), nrow = 2)
#' threshold(mat, q = c(.5, .2, .001), sd_ctrl = c(.01, 5, 10), X_return = 3)
#' threshold(t(mat), q = c(.5, .2, .001), sd_ctrl = c(.01, 5, 10), X_return = 3) %>%
#'  t()

