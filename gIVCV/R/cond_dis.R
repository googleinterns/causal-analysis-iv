#' @title Simulating cross validation splits when only group means are avaiable.
#'
#' @param mu marginal mean of group means.
#' @param sigma marginal covariance matrix for group means.
#' @param tau sum of the two cv splits.
#' @param theta linear constrain of the two cv splits.
#' @return return a conditional mean and a covariance matrix.
#' @references
#' Peysakhovich, A., & Eckles, D., Learning causal effects from many randomized experiments
#' using regularized instrumental variables.
#' \emph{Proceedings of the 2018 World Wide Web Conference} (2018, April). URL
#' https://dl.acm.org/doi/10.1145/3178876.3186151'
#'
#' Owen, Art B. \emph{Monte Carlo Theory, Methods and Examples.} (2016). URL
#'  http://statweb.stanford.edu/~owen/mc/
#'
#' @example
#' # Marginal distribution
#' mu = c(1.5, 1.5)
#' sigma = matrix(c(0.5, 0, 0, 0.5), nrow = 2)
#' cond_dis(mu = rep(.5, 2), sigma = diag(rep(.5, 2)), tau = 2, theta = t(c(1, 1)))

#' # condition on tau
#' tau = 0

#' # linear transformation
#' theta = matrix(c(1, 1), nrow = 1)

#' # return a list of conditional mean and variance
#' cond_dis(mu, sigma, tau, theta)

#' # validation
#' X <- mvrnorm(n = 20000, mu, sigma)

#' # linear transformation
#' X_tran <- theta %*% t(X) %>% t() %>% as.double()

#' # select those with X_tran approximately equal to tau
#' X_con <- data.frame(value = X_tran, X = X) %>%
#'  mutate(if_tau =
#'           case_when(value > (tau -.1) & value < (tau + .1) ~ 1)
#'  ) %>% drop_na()

#' # conditional mean and variance based on empirical data
#' mean(X_con[, c(1)])

# estimated conditional mean and covariance
#' colMeans(X_con[, c(2, 3)])
#' cov(X_con[, c(2, 3)])

cond_dis <- function(mu, sigma, tau, theta) {
  # Conditional mean of the cv splits, conditoning on tau(the observed group mean)
  mu_cond <-
    mu + sigma %*% t(theta) %*% solve(theta %*% sigma %*% t(theta)) %*% (tau - theta %*% mu)
  # Conditional covariance matrix of the cv splits, conditoning on tau(the observed group mean)
  sigma_cond <-
    sigma - sigma %*% t(theta) %*% solve(theta %*% sigma %*% t(theta)) %*% theta %*%
    sigma
  return(list(mu_cond = mu_cond, sigma_cond = sigma_cond))
}




