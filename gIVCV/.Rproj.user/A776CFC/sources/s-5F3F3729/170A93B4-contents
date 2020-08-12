#' @title Instrumental variable with cross validation
#'
#' @description
#' @param agree Testing if the package works well.
#' @keywords instrumental variable
#' @export
#' @examples
#'
#'
#'
library(tidyverse)
library(MASS)
select <- dplyr::select

IVCV <-
  function(formula,
           id,
           data,
           subset = NULL,
           weights = NULL,
           wt = NULL,
           ctrl_id = 1 ,
           instruments = NULL,
           na.action = NULL,
           L0 = F,
           raw = F,
           ...){
    # build model frame
    mf <- match.call(expand.dots = T)
    id <- data %>% pull({{id}})

    # of experiments
    J = length(unique(id))

    m <-
      match(c("formula", "id", "data", "weights", "subset", "instruments" ,"na.action"),
            names(mf), 0)
    mf <- mf[c(1, m)]

    f <- if(!is.null(instruments)) as.Formula(formula, instruments)
    else as.Formula(formula)
    stopifnot(isTRUE(all.equal(length(f), c(1, 2))))

    mf[[1]] <- as.name("model.frame")
    mf$formula <- f
    mf$instruments <- NULL
    mf$na.action <- NULL


    mf <- eval(mf, parent.frame())
    y <- model.response(mf)
    w <- as.vector(model.weights(mf))
    x <- model.matrix(f, data = mf, rhs = 1) %>%
      as_tibble() %>%
      dplyr::select(-`(Intercept)`)
    z <- model.matrix(f, data = mf, rhs = 2) %>%
      as_tibble() %>%
      dplyr::select(-`(Intercept)`)

    dat <- tibble(id = id, Y = y, X = x, Z = z)
    if(L0 == T){
      ctrl_X <- dat %>% filter(id == ctrl_id) %>% pull(X)
      ctrl_size <- nrow(ctrl_X)

      dim_X = ncol(x)
      # X_cvid_var
      dat_cv <- dat %>%
        make_cv(group_id = id, n_cv = 2) %>%
        group_by(id, cv_id) %>%
        summarise(mean_X = map(list(X), ~ colMeans(X)), mean_Y = mean(Y)) %>%
        pivot_wider(names_from = cv_id,
                    values_from = c(mean_X, mean_Y)) %>%
        mutate(var_name = list(paste(seq_len(dim_X)))) %>%
        unnest(everything())  %>%
        pivot_wider(names_from = var_name,
                    values_from = c(mean_X_1, mean_X_2)) %>%
        ungroup()
      mean_X_1 <- dat_cv %>% select(starts_with("mean_X_1"))
      mean_Y_1 <- dat_cv %>% select(starts_with("mean_Y_1"))
      mean_X_2 <- dat_cv %>% select(starts_with("mean_X_2"))
      mean_Y_2 <- dat_cv %>% select(starts_with("mean_Y_2"))

      sd_ctrl = sqrt(diag(var(ctrl_X))) / sqrt(ctrl_size / 2)
      q <- plogis(seq(-50, 0, length.out = 1000))
      message("Cross validation...")

     q_final <- tibble(
        q,
        X_1 = list(as.matrix(mean_X_1)),
        X_2 = list(as.matrix(mean_X_2)),
        Y_1 = list(as.matrix(mean_Y_1)),
        Y_2 = list(as.matrix(mean_Y_2)),
        ctrl_X = list(as.matrix(rep_row(colMeans(ctrl_X), J)))
      ) %>% mutate(id = row_number(),
                   X = pmap(
                     list(X_1, ctrl_X, q),
                     ~ threshold(
                       X = ..1-..2,
                       q = ..3,
                       sd_ctrl = sd_ctrl,
                       X_return = ..1
                     )
                   )) %>%
        mutate(X_1_q = map(X, ~ as.matrix(.x))) %>%
        mutate(fit = pmap(list(Y_1, X_1_q), ~ lm(..1 ~ ..2, weights = wt))) %>%
        mutate(Y_hat = pmap(list(fit, X_1), ~ predict(..1, newdata = as.data.frame(..2)))) %>%
        mutate(error = pmap_dbl(list(Y_2, Y_hat), ~ mean((..1-..2) ^ 2))) %>%
        slice(which.min(error)) %>% pull(q)
     message(paste("Tuning parameter q selected:", q_final))

      dat_final <- dat %>%
        group_by(id) %>%
        summarise(mean_X = map(list(X), ~colMeans(X)), mean_Y = mean(Y)) %>%
        mutate(ctrl_X = list(colMeans(as.matrix(ctrl_X)))) %>%
        mutate(mean_X_q = pmap(list(mean_X, ctrl_X, q_final),
                               ~threshold(X = ..1 - ..2,
                                          q = ..3,
                                          sd_ctrl = sd_ctrl,
                                          X_return = ..1)))
      Y <-
        dat_final$mean_Y %>%
        as.matrix()  %>%
        set_colnames(.,
                     sub("^(\\w*)\\s~\\s.*$",
                         "\\1",
                         deparse(f)))

      X <-
        do.call(rbind,
                dat_final$mean_X_q) %>%
        set_colnames(colnames(x)) %>%
        as.matrix()
      fit <- lm(Y ~ X, weights = wt, ...)
      names(fit$coefficients)[-1]<- colnames(x)
      return_obj <- list(
        q_final = q_final,
        fit = fit,
        data = data.frame(Y, X)
      )
    }else{
      if(raw == T){
      xz <- as.matrix(lm.fit(as.matrix(z), as.matrix(x))$fitted.values)
      fit <- lm(y ~ xz)
      names(fit$coefficients)[-1]<- colnames(x)
      return_obj <- list(
        fit = fit
      )}else{
          dt_mean <- dat %>%
          group_by(id) %>%
          summarise(mean_X = map(list(X), ~ colMeans(X)), mean_Y = mean(Y))
          X <- do.call(rbind, dt_mean$mean_X)
          Y <-  dt_mean$mean_Y
          fit <- lm(Y ~ X)
        names(fit$coefficients)[-1]<- colnames(x)
        return_obj <- list(
          fit = fit
        )
      }
    }
    return(return_obj)
  }





