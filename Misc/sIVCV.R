library(tidyverse)
library(MASS)

# scalar form
mu = 1
sd = 2
sigma <- sd^2
X <- rnorm(10000, mean = 1, sd = 2)
theta = 1
t = 3

# conditional distribution
mu_cond <- mu + sigma*theta*(t - theta*mu)
var <- sigma - sigma*theta*(1/((theta^2*sigma)))*theta*sigma

# vector form
mu = c(1, 1)
Sigma = matrix(c( 1, .3, .3, 1), nrow = 2)
X <- mvrnorm(n = 10000, mu = c(1, 1), Sigma = matrix(c( 1, .3, .3, 1), nrow = 2))
cov(X)
theta <- matrix(c(2, 1), nrow = 2)
t = c(2, 2)
Sigma_t <- t(t)
mu_cond <- mu + Sigma %*% t(Sigma_t)%*%(t - theta %*% mu)
cov_cond <- Sigma - Sigma%*%t(Sigma_t)%*%solve(Sigma_t %*% Sigma %*% t(Sigma_t))%*%Sigma_t%*%Sigma
X_cond <- mvrnorm(n = 10000, mu = mu_cond, Sigma = cov_cond)

sigma <- matrix(c( 1, .3, .3, 1), nrow = 2)
theta <- matrix(c( 1, .3, .3, 1), nrow = 2)
sigma - sigma %*% t(theta) %*% solve(theta %*% sigma %*% t(theta)) %*% theta %*% sigma
