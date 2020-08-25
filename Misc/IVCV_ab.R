# IVCV
library(tidyverse)
library(MASS)
library(metRology) # to simulate from scaled-t distribution, can be viewed as a infinite mixture of normal
J = 200
1:10
Z <- rnorm(J)
Z[1] <- 0
I = 100
dim_X <- 3
dat <- tibble(Z) %>% 
  mutate(id = 1:n(), 
         U = map(id, ~rnorm(I)), 
         e_X = map(id, ~rnorm(I)),
         e_Y = map(id, ~rnorm(I))) %>% 
  mutate(X = pmap(list(Z, U, e_X), 
                  ~..1 + ..2 + ..3)) %>% 
  mutate(Y = pmap(list(X, U, e_Y), 
                  ~..1 + ..2 + ..3)) %>% 
  unchop(c(X, Y, Z, U, e_X, e_Y))
lm(X ~ -1 + as.factor(id), dat) %>% summary()
dat$id %>% unique()

J = 50
I = 300
Z_exp <- c(1, 2, 3)
Z_sig <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
Z_eff <- c(.1, .2, .3)
X_exp <- c(1, 2, 3)
X_sig <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
X_eff <- c(.1, .2, .3)
e_X_exp <- c(1, 2, 3)
e_X_sig <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
e_X_eff <- c(.1, .2, .3)

N = J*I
U <- rnorm(N)
Z <- mvrnorm(J, mu = Z_exp, Sigma = Z_sig)
e_X <- mvrnorm(N, mu = e_X_exp, Sigma = e_X_sig)
e_Y <- rnorm(N)
Z <- Z %>% as_tibble() %>% slice(rep(1:n(), each = I)) %>% as.matrix()
X <- Z + U + e_X
Y <- X %*% X_eff + e_Y + U
data.frame(Y, X, id = rep(1:J, each = I)) %>% head
dat1 <- data.frame(Y, X, id = rep(1:J, each = I)) %>% group_by(id) %>% summarise(mean_Y = mean(Y), mean_X1 = mean(V1), mean_X2 = mean(V2), mean_X3 = mean(V3))
dat2 <- dat1 %>% as_tibble() %>% slice(rep(1:n(), each = I)) 
glm(mean_Y ~ mean_X1 + mean_X2 + mean_X3, dat1, weights = rep(I, J), family = gaussian()) %>% summary()
lm(mean_Y ~ mean_X1 + mean_X2 + mean_X3, dat2) %>% summary()

lm(Y ~ predict(lm(X ~ (Z)))) %>% summary()

tibble(Z) %>% 
  mutate(id = 1:n(), 
         U = map(id, ~rnorm(I)), 
         e_X = map(id, ~rnorm(I)),
         e_Y = map(id, ~rnorm(I)))
