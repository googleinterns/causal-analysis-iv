sim_gIV <- function(mu_Z = c(0, 0, 0),
                    beta_ctrl = .2,
                    summary_data = T,
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
                    I = 30){
  Z <- mvrnorm(n = J, mu = mu_Z, Sigma = sig_Z)
  ctrl_id <- c(T, F)
  ctrl_id = (1:J)[ctrl_id]
  Z[c(ctrl_id), ] <- 0
  dat <-
    tibble(Z) %>%
    mutate(
      id = 1:n() %>% as.factor(),
      Z_vec = map(id, ~ Z[.x, ] %>% rep_row(times = I)),
      e_X = map(id, ~ mvrnorm(I, mu_e_X, Sigma = sig_e_X)),
      e_Y = map(id, ~ rnorm(I)),
      U = map(id, ~ mvrnorm(I, mu_U, sig_U))
    ) %>%
    mutate(X = pmap(list(Z_vec, U, e_X),
                    ~ ..1 +..2 %*% beta_UX + ..3)) %>%
    mutate(Y = pmap(list(X, U, e_Y),
                    ~ ..1 %*% beta_X + ..2 %*% beta_UY + ..3)) %>%
    unchop(c(X, Y, U)) %>%
    add_column(ctrl_id = rep(0:1, J*I/2)) %>%
    mutate(Y = Y + ctrl_id*beta_ctrl)

  if (summary_data == T){
    colnames(dat$Y)  <- "y"
    colnames(dat$X)  <- paste0("x", 1:ncol(dat$X))
    colnames(dat$Z)  <- paste0("z", 1:ncol(dat$Z))
    colnames(dat$U)  <- paste0("u", 1:ncol(dat$Z))

    dat <-
      data.frame(id = dat$id,
                 ctrl_id = rep(0:1, J*I/2),
                 dat$X,
                 dat$Y,
                 dat$Z,
                 dat$U)
    dat_mean <- dat %>%
      group_by(id, ctrl_id) %>%
      summarise_at(vars(x1:u3), mean)
    data <- dat_mean
  } else {
    data <- dat
  }
  return(data)
}

