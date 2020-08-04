#' @title A test Function
#'
#' @description This function allows you to express my excitment of working at Google being a remote intern.
#' @param agree Testing if the package works well.
#' @keywords google
#' @export
library(tidyverse)
library(MASS)

cond_dis <- function(mu, sigma, tau, theta) {
  mu_cond <-
    mu + sigma %*% t(theta) %*% solve(theta %*% sigma %*% t(theta)) %*% (tau - theta %*% mu)
  sigma_cond <-
    sigma - sigma %*% t(theta) %*% solve(theta %*% sigma %*% t(theta)) %*% theta %*%
    sigma
  return(list(mu_cond = mu_cond, sigma_cond = sigma_cond))
}

#' @examples
# marginal distribution
mu = c(1.5, 1.5)
sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2)

# condition on tau
tau = 3

# linear transformation
theta = matrix(c(1, 1), nrow = 1)

# return a list of conditional mean and variance
cond_dis(mu, sigma, tau, theta)

# validation
X <- mvrnorm(n = 20000, mu, sigma)

# linear transformation
X_tran <- theta %*% t(X) %>% t() %>% as.double()

# select those with X_tran approximately equal to tau
X_con <- data.frame(value = X_tran, X = X) %>%
  mutate(if_tau =
           case_when(value > (tau -.1) & value < (tau + .1) ~ 1)
  ) %>% drop_na()

# conditional mean and variance based on empirical data
mean(X_con[, c(1)])

# estimated conditional mean and covariance
colMeans(X_con[, c(2, 3)])
cov(X_con[, c(2, 3)])

