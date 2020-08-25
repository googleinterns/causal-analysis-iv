#' @title Simulating data for IV with multitple experiments.
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
#' @note Currently only support generate balanced panel.
#' @import MASS dplyr purrr tidyr Formula
#' @examples
#' # true effect is .1, .2, .3
#' J = 2000
#' I = 100
#' sim_gIV(beta_ctrl = .3,
#'         beta_X = c(.1, .1, .1),
#'         J = J, I = I)
#' @export
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
                    mu_e_Y = 0,
                    sig_e_Y = 1,
                    J = 50,
                    I = 30) {
  Z <- MASS::mvrnorm(n = J, mu = mu_Z, Sigma = sig_Z)
  ctrl_id <- c(T, F)
  ctrl_id = (1:J)[ctrl_id]
  Z[c(ctrl_id), ] <- 0

  # simulating following the a linear structural model
  dat <-
    tibble(Z) %>%
    mutate(
      id = 1:n() %>% as.factor(),
      Z_vec = purrr::map(id, ~ Z[.x, ] %>% rep_row(times = I)),
      e_X = purrr::map(id, ~ MASS::mvrnorm(I, mu_e_X, Sigma = sig_e_X)),
      e_Y = purrr::map(id, ~ rnorm(
        I, mean = mu_e_Y, sd = sqrt(sig_e_Y)
      )),
      U = purrr::map(id, ~ MASS::mvrnorm(I, mu_U, sig_U))
    ) %>%
    mutate(X = purrr::pmap(list(Z_vec, U, e_X),
                           ~ ..1+..2 %*% beta_UX + ..3)) %>%
    mutate(Y = purrr::pmap(list(X, U, e_Y),
                           ~ ..1 %*% beta_X + ..2 %*% beta_UY + ..3)) %>%
    tidyr::unchop(c(X, Y, U)) %>%
    tibble::add_column(ctrl_id = rep(0:1, J * I / 2)) %>%
    mutate(Y = Y + ctrl_id * beta_ctrl)

  if (summary_data == T) {
    colnames(dat$Y)  <- "y"
    colnames(dat$X)  <- paste0("x", 1:ncol(dat$X))
    colnames(dat$Z)  <- paste0("z", 1:ncol(dat$Z))
    colnames(dat$U)  <- paste0("u", 1:ncol(dat$Z))

    dat <-
      data.frame(id = dat$id,
                 ctrl_id = rep(0:1, J * I / 2),
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
