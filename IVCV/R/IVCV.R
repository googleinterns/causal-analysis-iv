#' @title A test Function
#'
#' @description
#' @param agree Testing if the package works well.
#' @keywords instrumental variable
#' @export nlme
#' #' @examples
#'
#'
#'
library(nlme)
library(tidyverse)
library(MASS)

select <- dplyr::select
library(car)
IVCV <- function(dat = data, I = 800, J = 100, n_cv = 2, ctrl_id = 1){
  dat_ctrl <- dat %>% filter(id == ctrl_id) %>% pull(X)

  nrow_X = 3
  # X_cvid_var
  dat_cv <- dat %>%
    make_cv(group_id = id, n_cv = 2) %>%
    group_by(id, cv_id) %>%
    summarise(mean_X = map(list(X), ~colMeans(X)), mean_Y = mean(Y)) %>%
    pivot_wider(names_from = cv_id, values_from = c(mean_X, mean_Y)) %>%
    mutate(var_name = list(paste(seq_len(nrow_X)))) %>%
    unnest(everything()) %>%
    pivot_wider(names_from = var_name,
                values_from = c(mean_X_1, mean_X_2)) %>% ungroup()
  mean_X_1 <- dat_cv %>% select(starts_with("mean_X_1"))
  mean_Y_1 <- dat_cv %>% select(starts_with("mean_Y_1"))
  mean_X_2 <- dat_cv %>% select(starts_with("mean_X_2"))
  mean_Y_2 <- dat_cv %>% select(starts_with("mean_Y_2"))

  sd_ctrl = sqrt(var(dat_ctrl)/(I/2)) %>% diag() %>% max()
  q <- plogis(seq(-30, 0, length.out = 1000))

  ctrl_X <- mean_X_1[ctrl_id, ] %>% rep_row(J)

  q_final <- tibble(
    q,
    X_1 = list(as.matrix(mean_X_1)),
    X_2 = list(as.matrix(mean_X_2)),
    Y_1 = list(as.matrix(mean_Y_1)),
    Y_2 = list(as.matrix(mean_Y_2)),
    ctrl_X = list(as.matrix(ctrl_X))
  ) %>% mutate(id = row_number(),
               X = pmap(list(X_1, ctrl_X, q),
                        ~ threshold(X = ..1 - ..2,
                                    q = ..3,
                                    sd_ctrl = sd_ctrl,
                                    X_return = ..1))) %>%
    mutate(X_1_q = map(X, ~ as.matrix(.x))) %>%
    mutate(fit = pmap(list(Y_1, X_1_q), ~ lm(..1 ~ ..2))) %>%
    mutate(Y_hat = pmap(list(fit, X_1), ~ predict(..1, newdata = as.data.frame(..2)))) %>%
    mutate(error = pmap_dbl(list(Y_2, Y_hat), ~ mean((..1-..2) ^ 2))) %>%
    slice(which.min(error)) %>% pull(q)


  dat_final <- dat %>%
    group_by(id) %>%
    summarise(mean_X = map(list(X), ~colMeans(X)), mean_Y = mean(Y)) %>%
    mutate(ctrl_X = list(as.matrix(ctrl_X[ctrl_id, ]))) %>%
    mutate(mean_X_q = pmap(list(mean_X, ctrl_X, q_final), ~threshold(X = ..1 - ..2,
                                                                     q = ..3,
                                                                     sd_ctrl = .2, X_return = ..1)))
  dat_final$mean_X
  dat_final$mean_X_q
  Y <- dat_final$mean_Y
  X <- do.call(rbind, dat_final$mean_X_q)
  return(list(q_final = q_final, fit = lm(Y ~ X), data = data.frame(Y, X)))
}
#
# IVCV()
# X %>% repl
#
# lm(dat$Y ~ predict(lm(dat$Z ~ dat$id)))

