#' @title Simulating data for IV with multitple experiment arms and one control arm
#' @ctrl_id indicator for the control arm.
#' @param mu_Z expectation for the true effects.
#' @param beta_ctrl true effect of the experiment arm.
#' @param summary_data return a data, averaged across experiments and arms.
#' @param mu_U expectation of the multidimensional confounders.
#' @param sig_U covariance matrix of the multidimensional confounders.
#' @param beta_X true effects of the causal variables of intrest.
#' @param beta_UX regression coefficients of the confounders on the causal variables of interest.
#' @param beta_UY regression coefficients of the confounders on the outcome variable.
#' @param mu_e_X expectation of the first stage errors - within group errors.
#' @param sig_e_X covariance matrix of the first stage errors - within group errors.
#' @param mu_e_Y expectation of the second stage errors - between group errors.
#' @param sig_e_Y variance of the second stage errors - between group errors.
#' @param I size of each experiment arm.
#' @param J number of experiments.
#' # true effect: .1 .2 .3
#' I = 100
#' J = 500
#' sim_IV(I = I,
#'        J = J,
#'        beta_X = c(.1, .2, .3),
#'        ctrl_id = 1)
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
    dim_X <- length(mu_e_X)
    data <-
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
    colnames(data$Y)  <- "y"
    colnames(data$X)  <- paste0("x", 1:dim_X)
    colnames(data$Z)  <- paste0("z", 1:dim_X)
    data <- data.frame(id = data$id, data$X, data$Y, data$Z)
    return(data)
  }
