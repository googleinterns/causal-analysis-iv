library(tidyverse)
N = 1000000

# confounder U 
U = rnorm(N)
# instrument
Z = rnorm(N)

# simulate X and Y
X = 2*U + (2*Z + rnorm(N))
# set the causal effect as 1.5
# BETA = 1.5
Y = 1.5*X + (3*U + rnorm(N))

# ols with all variables
lm(Y ~ X + U)

# ols with omitted variable U
lm(Y ~ X)

# wald estimator
cov(Y, Z)/cov(X, Z)

# 2SLS
stage_1_fit <- lm(X ~ Z)
X_hat <- predict(stage_1_fit)

stage_2_fit <- lm(Y ~ X_hat)
summary(stage_2_fit)

# SEM estimator
library(lavaan)
sem_mod <- "
    X ~ Z
    Y ~ X
    X ~~ Y"
lavaan::sem(sem_mod, data = data.frame(X, Y, Z))
