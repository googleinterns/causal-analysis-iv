library(ivmodel)
library(tidyverse)
library(plm)
library(lme4)
library(MASS)
I = 1000
ri <- rnorm(I)
J = 50
dat <- tibble(ri) %>% 
  mutate(id = 1:n(), 
         X = map(id, ~rnorm(J)), 
         e = map(id, ~rnorm(J))) %>% 
  mutate(y = pmap(list(ri, X, e), 
                  ~..1 + 1 + 2*..2 + ..3)) %>% 
  unchop(c(X, e, y))

# verify with lme4 and plm
lmer(y ~ X + (1|id), data = dat) %>% 
  summary()
plm(y ~ X,   data = dat, 
             model = "random",
             index = "id") %>% 
  summary()

a = rnorm(100)
b = rnorm(100)

Y = 1 + 2*a + 3*b + rnorm(100)


data(card.data)
Y=card.data[,"lwage"]
D=card.data[,"educ"]
Z=card.data[,"nearc4"]
Xname=c("exper", "expersq", "black", "south", "smsa", "reg661",
        "reg662", "reg663", "reg664", "reg665", "reg666", "reg667",
        "reg668", "smsa66")
X=card.data[,Xname]
card.model = ivmodel(Y=Y,D=D,Z=Z,X=X)
IVpower(card.model)
IVpower(card.model, n=10^4, type="AR")


# IVCV
library(metRology) # to simulate from scaled-t distribution, can be viewed as a infinite mixture of normal
J = 200
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
dat_group <- 
  dat %>% 
  group_by(id) %>% 
  summarise(mean_X = mean(X), mean_Y = mean(Y))

dat_cv <-
  dat %>% add_column(cv_id = rep(rep(c(1, 2), each = I/2), J)) %>%
  group_by(id, cv_id) %>% 
  summarize(mean_X = mean(X),
            mean_Y = mean(Y),
            sd_X = sd(X)) %>%
  pivot_wider(names_from = cv_id,
              values_from = c(mean_X, mean_Y, sd_X))

lm(mean_Y_1 ~ mean_X_1, dat_cv)
lm(mean_Y_2 ~ mean_X_2, dat_cv)
i = 1
q <- plogis(seq(-10, 0, length.out = 1000))
mse <-  vector()
beta <- vector()
for(i in 1:1000){
  mean_X_1_g = if_else((1 - pnorm(abs(dat_cv$mean_X_1), sd = dat_cv$sd_X_1[1]/sqrt(I/2))) < q[i],  dat_cv$mean_X_1, 0)
  mean_X_2_g = if_else((1 - pnorm(abs(dat_cv$mean_X_2), sd = dat_cv$sd_X_1[1]/sqrt(I/2))) < q[i],  dat_cv$mean_X_2, 0)
  
  fit <- lm(dat_cv$mean_Y_1 ~ mean_X_1_g)
  beta[i] <- (fit$coefficients[2] - 1)^2
  mean_Y_hat <- predict(fit, newdata = data.frame(mean_X_1_g = dat_cv$mean_X_1))
  mse[i] <- mean((dat_cv$mean_Y_2 - mean_Y_hat)^2)

  
}
mean_X_g = if_else((1 - pnorm(abs(dat_group$mean_X), sd = dat_cv$sd_X_1[1]/sqrt(I/2))) < q[which.min(mse)],  dat_group$mean_X, 0)
lm(mean_Y ~ mean_X_g, dat_group) %>% summary()

# multi-dim Z
Z <- mvrnorm(n = J, mu = rep(0, dim_X), Sigma = diag(dim_X))
tibble(Z = data.frame(Z)) 


# ivcv scalar form ---
# new sim ----
N = 100000
K = 1000
uv <- mvrnorm(N, c(0, 0), matrix(c(1, .5, .5, 1), 2, 2))
#z <- rnorm(N, 2, 1)
z_id <- rep(1:K, each = N/K)
# .5*N(0, 5) + .5*N(0, 1)
z_mean <- rt.scaled(n = K, df = 3, mean = 0, sd = .4)
z <- rep(z_mean, each = N/K)
u <- uv[, c(2)]
v <- uv[, c(1)]
x =  z + u
y = .5*x + v
dat <-  tibble(x, y, z, z_factor = as.factor(z_id))

dat_cv <- dat %>% add_column(cv_id = rep(rep(c(1, 2), each = N/K/2), K)) %>% 
  group_by(z_factor, cv_id) %>% summarize(mean_x = mean(x), mean_y = mean(y)) %>% 
  pivot_wider(names_from = cv_id, values_from = c(mean_x, mean_y))  


lm(mean_y_1 ~ mean_x_1, dat_cv)
i = 1
q <- plogis(seq(-10, 0, length.out = 500))
mse <- mse2 <- mse3 <- vector()
beta <- vector()
for(i in 1:500){
  mean_x_1_g = ifelse((1 - pnorm(abs(dat_cv$mean_x_1), sd = sqrt(2/(N/K)))) < q[i],  dat_cv$mean_x_1, 0)
  mean_x_2_g = ifelse((1 - pnorm(abs(dat_cv$mean_x_2), sd = sqrt(2/(N/K)))) < q[i],  dat_cv$mean_x_2, 0)
  
  fit <- lm(dat_cv$mean_y_1 ~ mean_x_1_g)
  beta[i] <- (fit$coefficients[2] - .5)^2
  mean_y_hat <- predict(fit, newdata = data.frame(mean_x_1_g = dat_cv$mean_x_1))
  mean_y_hat1 <- predict(fit, newdata = data.frame(mean_x_1_g = dat_cv$mean_x_2))
  mse[i] <- mean((dat_cv$mean_y_2 - mean_y_hat)^2)
  mse2[i] <- mean((dat_cv$mean_x_1 - mean_x_1_g)^2)
  mse3[i] <- mean((dat_cv$mean_y_2 - mean_y_hat1)^2)
  
}

# causal loss
plot(log(q), scale(beta), col="red")
# l0 loss
points(log(q), scale(mse), col = "green")
# two naive loss
points(log(q), scale(mse2), col="blue" )
points(log(q), scale(mse3), col = "yellow")


# ivcv vector format ---- replicating the experiment on page 5 - 2-dimension X
# I use the true variance in the control group instead of the estimated one, should be similar
N = 100000
K = 1000
error <-
  mvrnorm(N, c(0, 0, 0), matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), 3, 3))
#experiment id
z_id <- rep(1:K, each = N / K)
# .5*N(0, 5) + .5*N(0, 1)
z1_mean <- rt.scaled(n = K,
                     df = 3,
                     mean = 0,
                     sd = .4)
z1 <- rep(z1_mean, each = N / K)
z2_mean <- rt.scaled(n = K,
                     df = 3,
                     mean = 0,
                     sd = .4)
z2 <- rep(z2_mean, each = N / K)

v <- error[, c(1)]
u1 <- error[, c(2)]
u2 <- error[, c(3)]

x1 =  z1 + u1
x2 =  z2 + u2

# population model: beta  = c(.5, .2)
y = .5 * x1 + .2 * x2 + v
dat <- data.frame(x1, x2, y, z1, z2, z_factor = as.factor(z_id))

# 2sls: second stage ols with experimental level statistics
dat_mean <-
  dat %>% group_by(z_factor) %>% summarise(mean_x1 = mean(x1),
                                           mean_x2 = mean(x2),
                                           mean_y = mean(y))
lm(mean_y ~ mean_x1 + mean_x2, dat_mean)

# ols on raw data
lm(y ~ x1 + x2, dat)

# data for ivcv
dat_cv <-
  dat %>% add_column(cv_id = 
                       rep(rep(c(1, 2), each = N / K / 2), K)) %>%
  group_by(z_factor, cv_id) %>% 
  summarize(mean_x1 = mean(x1),           
            mean_x2 = mean(x2),
            mean_y = mean(y)) %>%
  pivot_wider(names_from = cv_id,
              values_from = c(mean_x1, mean_x2, mean_y))

# tune parameters
i = 1
q <- plogis(seq(-10, 0, length.out = 500))
mse <- mse2 <- mse3 <- vector()
beta <- vector()
for (i in 1:500) {
  mean_x1_1_g = ifelse(((1 - pnorm(
    abs(dat_cv$mean_x1_1), sd = sqrt(2 / (N / K))
  )) < q[i]) |
    ((1 - pnorm(
      abs(dat_cv$mean_x2_1), sd = sqrt(2 / (N / K))
    )) < q[i]),  dat_cv$mean_x1_1, 0)
  mean_x2_1_g = ifelse(((1 - pnorm(
    abs(dat_cv$mean_x1_1), sd = sqrt(2 / (N / K))
  )) < q[i]) |
    ((1 - pnorm(
      abs(dat_cv$mean_x2_1), sd = sqrt(2 / (N / K))
    )) < q[i]),  dat_cv$mean_x2_1, 0)
  
  mean_x1_2_g = ifelse(((1 - pnorm(
    abs(dat_cv$mean_x1_2), sd = sqrt(2 / (N / K))
  )) < q[i]) |
    ((1 - pnorm(
      abs(dat_cv$mean_x2_2), sd = sqrt(2 / (N / K))
    )) < q[i]),  dat_cv$mean_x1_2, 0)
  mean_x2_2_g = ifelse(((1 - pnorm(
    abs(dat_cv$mean_x1_2), sd = sqrt(2 / (N / K))
  )) < q[i]) |
    ((1 - pnorm(
      abs(dat_cv$mean_x2_2), sd = sqrt(2 / (N / K))
    )) < q[i]),  dat_cv$mean_x2_2, 0)
  
  fit <- lm(dat_cv$mean_y_1 ~ mean_x1_1_g + mean_x2_1_g)
  beta[i] <- (fit$coefficients[c(2, 3)] - c(.5, .2)) ^ 2 %>% sum()
  mean_y_hat <-
    predict(fit,
            newdata = data.frame(
              mean_x1_1_g = dat_cv$mean_x1_1,
              mean_x2_1_g = dat_cv$mean_x2_1
            ))
  mean_y_hat1 <-
    predict(fit,
            newdata = data.frame(
              mean_x1_1_g = dat_cv$mean_x1_2,
              mean_x2_1_g = dat_cv$mean_x2_2
            ))
  mse[i] <- mean((dat_cv$mean_y_2 - mean_y_hat) ^ 2)
  mse2[i] <- mean((dat_cv$mean_x1_1 - mean_x1_1_g) ^ 2)
  mse3[i] <- mean((dat_cv$mean_y_2 - mean_y_hat1) ^ 2)
  
}

tibble(
  log_q = log(q), # hyperparameter
  beta = scale(beta), # causal loss
  mse = scale(mse), # l0 cv loss
  mse2 = scale(mse2), # naive loss
  mse3 = scale(mse3) # naive loss
) %>%
  gather(key = "loss_type", value = "scaled_loss", beta:mse3) %>%
  ggplot(aes(log_q, scaled_loss, color = loss_type)) + geom_point()


