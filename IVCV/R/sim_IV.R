library(MASS)
sim_IV <-
  function(mu_Z = c(0, 0, 0),
           sig_Z = matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3),
           mu_U = c(0, 0, 0),
           sig_U = matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3),
           beta_X = c(.1, .2, .3),
           beta_UX = matrix(c(.5, .5, .5, .5, .5, .5, .5, .5, .5), nrow = 3),
           beta_UY = c(1, 2, 3),
           mu_e_X = c(0, 0, 0),
           sig_e_X = matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3),
           mu_e_Y = c(0, 0, 0),
           sig_e_Y = matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3),
           J = 50,
           I = 30,
           ctrl_id = 1) {
    Z <- mvrnorm(n = J, mu = mu_Z, Sigma = sig_Z)
    Z[ctrl_id, ] <- 0
    tibble(Z) %>%
      mutate(
        id = 1:n() %>% as.factor(),
        Z_vec = map(id, ~ Z[.x, ] %>% rep_row(times = I)),
        e_X = map(id, ~ mvrnorm(I, mu_e_X, Sigma = sig_e_X)),
        e_Y = map(id, ~ rnorm(I)),
        U = map(id, ~ mvrnorm(I, mu_U, sig_U))
      ) %>%
      mutate(X = pmap(list(Z_vec, U, e_X),
                      ~ ..1+..2 %*% beta_UX + ..3)) %>%
      mutate(Y = pmap(list(X, U, e_Y),
                      ~ ..1 %*% beta_X + ..2 %*% beta_UY + ..3)) %>%
      unchop(c(X, Y, U))
  }
#
# dat <- sim_IV(J = 20, I = 10)
# dat <- sim_IV(J = 200, I = 100)
#
# dat %>% summarise(sd_U1 = sd(U[,1]), sd_U2 = sd(U[,2]), sd_U3 = sd(U[,3]))
#
# dat$Y
# J = 50
# I = 300
# Z_exp <- c(1, 2, 3)
# Z_sig <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
# Z_eff <- c(.1, .2, .3)
# X_exp <- c(1, 2, 3)
# X_sig <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
# X_eff <- c(.1, .2, .3)
# e_X_exp <- c(1, 2, 3)
# e_X_sig <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
# e_X_eff <- c(.1, .2, .3)
#
# N = J*I
# U <- rnorm(N)
# Z <- mvrnorm(J, mu = Z_exp, Sigma = Z_sig)
# e_X <- mvrnorm(N, mu = e_X_exp, Sigma = e_X_sig)
# e_Y <- rnorm(N)
# Z <-
#   Z %>% as_tibble() %>% slice(rep(1:n(), each = I)) %>% as.matrix()
# X <- Z + U + e_X
# Y <- X %*% X_eff + e_Y + U
# data.frame(Y, X, id = rep(1:J, each = I)) %>% head
# dat1 <- data.frame(Y, X, id = rep(1:J, each = I)) %>% group_by(id) %>% summarise(mean_Y = mean(Y), mean_X1 = mean(V1), mean_X2 = mean(V2), mean_X3 = mean(V3))
# dat2 <- dat1 %>% as_tibble() %>% slice(rep(1:n(), each = I))
# glm(mean_Y ~ mean_X1 + mean_X2 + mean_X3, dat1, weights = rep(I, J), family = gaussian()) %>% summary()
# lm(mean_Y ~ mean_X1 + mean_X2 + mean_X3, dat2) %>% summary()
#
# lm(Y ~ predict(lm(X ~ (Z)))) %>% summary()
#
# tibble(Z) %>%
#   mutate(id = 1:n(),
#          U = map(id, ~rnorm(I)),
#          e_X = map(id, ~rnorm(I)),
#          e_Y = map(id, ~rnorm(I)))
#
# mu_Z <- c(1, 2, 3)
# sig_Z <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
# beta_Z <- c(.1, .2, .3)
# mu_U <- c(1, 2, 3)
# sig_U <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
# beta_U <- c(.1, .2, .3)
# mu_X <- c(1, 2, 3)
# sig_X <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
#
# beta_X <- c(.1, .2, .3)
# mu_e_X <- c(1, 2, 3)
# sig_e_X <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
# beta_e_X <- c(.1, .2, .3)
#
# mu_e_Y <- c(1, 2, 3)
# sig_e_Y <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow = 3)
# beta_e_Y <- c(.1, .2, .3)
#
# beta_UY <- beta_UX <- c(1, 2 ,3)
# beta_UX <- matrix(c(.1, .2, .3, .1, .2, .3, .1, .2, .3), nrow = 3)
# N = J*I
# I = 50
# U <- rnorm(N)
# Z <- mvrnorm(J, mu = Z_exp, Sigma = Z_sig)
#
# asd <- tibble(Z) %>%
#   mutate(id = 1:n() %>% as.factor(),
#          e_X = map(id, ~mvrnorm(I, mu_e_X, Sigma = sig_e_X)),
#          e_Y = map(id, ~mvrnorm(I, mu_e_X, Sigma = sig_e_X)),
#          U = map(id, ~mvrnorm(I, mu_e_X, sig_e_X) %*% beta_e_X))
# asd["e_X"][[1]]
# asd["U"][[1]]
# lm(dat$Y ~ predict(lm(dat$X ~ dat$id))) %>% summary()
# dat_cv$mean_Y_1
# lm(dat_cv$mean_Y_1 ~ predict(lm(as.matrix(dat_cv$mean_X_1_1, dat_cv$mean_X_1_2, dat_cv$mean_X_1_3) ~ dat_cv$id))) %>% summary() %>% broom::tidy()
# library(purrr)
#
# asd <- tibble(Z) %>% mutate(id = row_number()) %>% mutate(Z_vec = map(id, ~ Z[.x, ] ))
# asd$Z_vec
# asd %>% view
#
#
# map_dfr(seq_len(J), ~ Z)
#
# tibble(Z) %>%
#   mutate(
#     id = 1:n() %>% as.factor(),
#     Z_vec = map(id, ~ Z[.x,] %>% rep_row(times = J)),
#     e_X = map(id, ~ mvrnorm(I, mu = mu_e_X, Sigma = sig_e_X)),
#     e_Y = map(id, ~ mvrnorm(I, mu = mu_e_X, Sigma = sig_e_X)),
#     U = map(id, ~ mvrnorm(I, mu_U, sig_U))
#   ) %>%
#   mutate(X = pmap(list(Z_vec, U, e_X),
#                   ~ ..1+..2 %*% beta_UX + ..3)) %>%
#   mutate(Y = pmap(list(X, U, e_Y),
#                   ~ ..1 %*% beta_X + ..2+..3)) %>%
#   unchop(c(Y, X, U)) ->asd
#
# tibble(Z) %>%
#   mutate(id = 1:n() %>% as.factor(),
#          Z_vec = map(id, ~ Z[.x, ] %>%
#                        slice(rep(1:3, each = 2))))
#
# tibble(Z) %>% slice(rep(1:n(), each = 2))
#
# c = v = 2
# apply(Z[1, ], 2, function(c) rep(c,v))
# Z[1, ] %>% rep_row(., times = 3)
#
# I = J = 100
# beta_UY <- c(.1, .2, .3)
# asd <- tibble(Z) %>%
#   mutate(id = 1:n() %>% as.factor(),
#          Z_vec = map(id, ~ Z[.x, ] %>% rep_row(times = I)),
#          e_X = map(id, ~mvrnorm(I, mu_e_X, Sigma = sig_e_X)),
#          e_Y = map(id, ~rnorm(I)),
#          U = map(id, ~mvrnorm(I, mu_U, sig_U))) %>%
#   mutate(X = pmap(list(Z_vec, U, e_X),
#                   ~ ..1 + ..2 %*% beta_UX + ..3 )) %>%
#   mutate(Y = pmap(list(X, U, e_Y),
#                   ~ ..1 %*% beta_X + ..2 %*% beta_UY + ..3)) %>%
#   unchop(c(X, Y, U))
#
# asd$Z_vec[[1]] + asd$U[[1]] %*% beta_UX + asd$e_X[[1]] - (asd$U[[1]] + asd$e_X[[1]])
#
# lm(asd$X ~ -1 + asd$id + asd$U)
# lm(asd$Y ~ asd$X + asd$U)
# lm(asd$Y ~ asd$X)
# predict(lm(asd$X ~ asd$id))
# lm(asd$Y ~ predict(lm(asd$X ~ -1 + asd$id)))
# lm(asd$X ~ -1 + asd$id + asd$U)
# lm(asd$Y ~ predict(lm(asd$X ~ -1 + asd$id)))
#
#
#
# tibble(Z) %>%
#   mutate(id = 1:n() %>% as.factor(),
#          e_X = map(id, ~mvrnorm(I, mu_e_X, Sigma = sig_e_X)),
#          e_Y = map(id, ~mvrnorm(I, mu_e_X, Sigma = sig_e_X)),
#          U = map(id, ~mvrnorm(I, mu_e_X, sig_e_X) %*% beta_e_X)) %>% view
#
# tibble(Z) %>%
#   mutate(id = 1:n() %>% as.factor(),
#          e_X = map(id, ~mvrnorm(I, mu_e_X, Sigma = sig_e_X)),
#          e_Y = map(id, ~mvrnorm(I, mu_e_X, Sigma = sig_e_X)),
#          U = map(id, ~mvrnorm(I, mu_e_X, sig_e_X) %*% beta_e_X)) %>%
#   mutate(X = pmap(list(Z, U, e_X),
#                   ~ ..1 + ..2 %*% beta_U  + ..3))# %>%
#   unchop(X)
#
#
#
# tibble(ri, rc) %>%
#   mutate(id = 1:n() %>% as.factor(),
#          e = map(id, ~rnorm(n, sd = sd_e)),
#          x = map(id, ~runif(n, 0, 6)))
#
# # icc = sd_ri/(sd_e + sd_ri)
# dat <- tibble(ri, rc) %>%
#   mutate(id = 1:n() %>% as.factor(),
#          e = map(id, ~rnorm(n, sd = sd_e)),
#          x = map(id, ~runif(n, 0, 6))) %>%
#   mutate(y = pmap(list(ri, rc, x, e),
#                   ~ ..1 + b0 + (..2 + b1)*..3 + ..4)) %>%
#   unchop(c(e, y, x))
#
