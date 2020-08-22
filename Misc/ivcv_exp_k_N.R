# loading packages ----
library(tidyverse)
library(MASS)
# to simulate from scalted-t distribution, can be viewed as a infinite mixture of normal
library(metRology) 


n_sim <- 100
beta_final_full <- matrix(NA, nrow = n_sim, ncol = 2)
beta_final <- matrix(NA, nrow = n_sim, ncol = 2)
beta_2sls <- matrix(NA, nrow = n_sim, ncol = 2)
true_par <-  c(.5, .2)
for (j in 1:n_sim){
# ivcv vector format ---- replicating the experiment on page 5 - 2-dimension X
# I use the true variance in the control group instead of the estimated one, should be similar
n = c(100, 1000, 5000, 10000, 50000)
K = c(30, 100, 500, 1000)
crossing(n, K)
error <-
  mvrnorm(N, c(0, 0, 0), matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), 3, 3))
#experiment id
z_id <- rep(1:K, each = n)

# .5*N(0, 5) + .5*N(0, 1)
z1_mean <- rt.scaled(n = K,
                     df = 3,
                     mean = 0,
                     sd = .4)
z1 <- rep(z1_mean, each = n)
z2_mean <- rt.scaled(n = K,
                     df = 3,
                     mean = 0,
                     sd = .4)
z2 <- rep(z2_mean, each = n)

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
                       rep(rep(c(1, 2), each = n / 2), K)) %>%
  group_by(z_factor, cv_id) %>% 
  summarize(mean_x1 = mean(x1),           
            mean_x2 = mean(x2),
            mean_y = mean(y)) %>%
  pivot_wider(names_from = cv_id,
              values_from = c(mean_x1, mean_x2, mean_y))

# tune parameters
i = 1
n_q <- 500
q <- plogis(seq(-12, 0, length.out = n_q))
mse <- mse2 <- mse3 <- vector()
beta <- matrix(, nrow = n_q, ncol = 2)
beta_mse <- vector()

for (i in 1:n_q) {
  mean_x1_1_g = ifelse(((1 - pnorm(
    abs(dat_cv$mean_x1_1), sd = sqrt(2 / (n))
  )) < q[i]) |
    ((1 - pnorm(
      abs(dat_cv$mean_x2_1), sd = sqrt(2 / (n))
    )) < q[i]),  dat_cv$mean_x1_1, 0)
  mean_x2_1_g = ifelse(((1 - pnorm(
    abs(dat_cv$mean_x1_1), sd = sqrt(2 / (n))
  )) < q[i]) |
    ((1 - pnorm(
      abs(dat_cv$mean_x2_1), sd = sqrt(2 / (n))
    )) < q[i]),  dat_cv$mean_x2_1, 0)
  
  mean_x1_2_g = ifelse(((1 - pnorm(
    abs(dat_cv$mean_x1_2), sd = sqrt(2 / (n))
  )) < q[i]) |
    ((1 - pnorm(
      abs(dat_cv$mean_x2_2), sd = sqrt(2 / (n))
    )) < q[i]),  dat_cv$mean_x1_2, 0)
  mean_x2_2_g = ifelse(((1 - pnorm(
    abs(dat_cv$mean_x1_2), sd = sqrt(2 / (n))
  )) < q[i]) |
    ((1 - pnorm(
      abs(dat_cv$mean_x2_2), sd = sqrt(2 / (n))
    )) < q[i]),  dat_cv$mean_x2_2, 0)
  
  fit <- lm(dat_cv$mean_y_1 ~ mean_x1_1_g + mean_x2_1_g)
  beta[i, ] <- (fit$coefficients[c(2, 3)]) 
  beta_mse[i] <- (beta[i, ] - c(.5, .2))^2 %>% sum()
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

mean_x1_g = ifelse(((1 - pnorm(
  abs(dat_mean$mean_x1), sd = sqrt(2 / (n))
)) < q[which.min(q)]) |
  ((1 - pnorm(
    abs(dat_mean$mean_x2), sd = sqrt(2 / (n))
  )) < q[which.min(q)]),  dat_mean$mean_x1, 0)
mean_x2_g = ifelse(((1 - pnorm(
  abs(dat_mean$mean_x1), sd = sqrt(2 / (n))
)) < q[which.min(q)]) |
  ((1 - pnorm(
    abs(dat_mean$mean_x2), sd = sqrt(2 / (n))
  )) < q[which.min(q)]),  dat_mean$mean_x2, 0)
beta_final_full[j, ] <- lm(dat_mean$mean_y ~ mean_x1_g + mean_x2_g)$coefficients[c(2, 3)]
beta_final[j, ] <- beta[which.min(mse), ]
beta_2sls[j, ] <- lm(mean_y ~ mean_x1 + mean_x2, dat_mean)$coefficients[c(2, 3)]

}

# mse
(beta_final_full_mse <- (beta_final_full - true_par)^2 %>% sum())
# bias
(beta_final_full_bias <- (beta_final_full - true_par) %>% sum())


# mse
(beta_final_mse <- (beta_final - true_par)^2 %>% sum())
# bias
(beta_final_bias <- (beta_final - true_par) %>% sum())


# mse
(beta_2sls_mse <- (beta_2sls - true_par)^2 %>% sum())
# bias
(beta_2sls_bias <- (beta_2sls - true_par) %>% sum())

c(beta_final_full_mse, beta_final_full_bias, beta_2sls_mse, beta_2sls_bias)
#sum_tab <- array(NA, dim = c(10, 4))
sum_tab[6, ] <- c(beta_final_mse, beta_final_bias, beta_2sls_mse, beta_2sls_bias)


tibble(
  log_q = log(q), # hyperparameter
  beta = scale(beta_mse), # causal loss
  mse = scale(mse), # l0 cv loss
  mse2 = scale(mse2), # naive loss
  mse3 = scale(mse3) # naive loss
) %>%
  gather(key = "loss_type", value = "scaled_loss", beta:mse3) %>%
  ggplot(aes(log_q, scaled_loss, color = loss_type)) + geom_point()



