#' @title A test Function
#'
#' @description
#' @param agree Testing if the package works well.
#' @keywords instrumental variable
#' @export
#' @examples
# rearrange
rearrange <- function(list, n_list = 2, n_var = 3){
  tibble(list) %>%
    unnest(list) %>%
    unchop(list) %>%
    add_column(id = rep(1:n_list,
                        each = n_var)) %>%
    nest(var = c(list)) %>%
    pull(var) %>%
    map(~unname(.x) %>% as.list)
}

sIVCV <-
  function(formula,
           id,
           data,
           ctrl_sd_x,
           ctrl_sd_y,
           subset = NULL,
           weights = NULL,
           wt = NULL,
           ctrl_id = 1 ,
           instruments = NULL,
           na.action = NULL,
           L0 = F,
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
      select(-`(Intercept)`)
    z <- model.matrix(f, data = mf, rhs = 2) %>%
      as_tibble() %>%
      select(-`(Intercept)`)
    dat <- tibble(id = id, Y = y, X = x, Z = z)

    # xz <- as.matrix(lm.fit(z, x)$fitted.values)
    #  lm.fit(xz, y)$coefficients
    if(L0 == T){
      dat_ctrl <- dat %>% filter(id == ctrl_id) %>% pull(X)
      dim_X = ncol(dat$X)

      dat_cv <- dat %>%
        group_by(id) %>%
        summarise(mean_X = map(list(X), ~colMeans(X)), mean_Y = mean(Y))


      dat_cv <- dat_cv %>%
        mutate(mean_Y_cv_cond = map(mean_Y, ~cond_dis(mu = rep(.x, 2), diag(rep(ctrl_sd_y^2, 2)), 2*.x, theta))) %>%
        mutate(mean_Y_cv = map(mean_Y_cv_cond, ~mvrnorm(n = 1, mu = .x$mu_cond, Sigma = .x$sigma_cond))) %>%
        mutate(mean_X = map(mean_X, ~unname(.x))) %>%
        add_column(ctrl_sd_x = rep_row(ctrl_sd_x, J)) %>%
        mutate(mean_X_cv_cond = map(mean_X, ~imap(., ~cond_dis(mu = rep(.x, 2), diag(rep(ctrl_sd_x[.y], 2)), 2*.x, theta)))) %>%
        mutate(mean_X_cv = map_depth(mean_X_cv_cond,.depth = 2, ~mvrnorm(n = 1, mu = .x$mu_cond, Sigma = .x$sigma_cond))) %>%
        mutate(mean_X_cv = map(mean_X_cv, ~rearrange(list = .x, n_list = 2, n_var = dim_X))) %>%
        add_column(cv_id = list((c(1, 2)))) %>%
        unnest(c(cv_id, mean_Y_cv, mean_X_cv)) %>%
        mutate(mean_X_cv = map(mean_X_cv, ~unlist(.x))) %>%
        pivot_wider(names_from = cv_id, values_from = c(mean_Y_cv, mean_X_cv)) %>%
        unchop(c(mean_X_cv_1 , mean_X_cv_2)) %>%
        add_column(var_id = rep(1:dim_X, J)) %>%
        pivot_wider(names_from = var_id, values_from = c(mean_X_cv_1, mean_X_cv_2), names_glue = "{.value}_{var_id}")


      mean_X_1 <- dat_cv %>% select(starts_with("mean_X_cv_1"))
      mean_Y_1 <- dat_cv %>% select(starts_with("mean_Y_cv_1"))
      mean_X_2 <- dat_cv %>% select(starts_with("mean_X_cv_2"))
      mean_Y_2 <- dat_cv %>% select(starts_with("mean_Y_cv_2"))


      #sd_ctrl = sqrt(var(dat_ctrl)/(I/2)) %>% diag() %>% max()
      q <- plogis(seq(-30, 0, length.out = 1000))
      message("Cross validation...")

      ctrl_X <- mean_X_1[ctrl_id, ] %>% rep_row(J)
      # sd_ctrl = .1
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
                                        sd_ctrl = ctrl_sd_x,
                                        X_return = ..1))) %>%
        mutate(X_1_q = map(X, ~ as.matrix(.x))) %>%
        mutate(fit = pmap(list(Y_1, X_1_q), ~ lm(..1 ~ ..2, weights = wt))) %>%
        mutate(Y_hat = pmap(list(fit, X_1), ~ predict(..1, newdata = as.data.frame(..2)))) %>%
        mutate(error = pmap_dbl(list(Y_2, Y_hat), ~ mean((..1-..2) ^ 2))) %>%
        slice(which.min(error)) %>% pull(q)
      message(paste("Tuning parameter q selected:", q_final))


      dat_final <- dat %>%
        group_by(id) %>%
        summarise(mean_X = map(list(X), ~colMeans(X)), mean_Y = mean(Y)) %>%
        mutate(ctrl_X = list(as.matrix(ctrl_X[ctrl_id, ]))) %>%
        mutate(mean_X_q = pmap(list(mean_X, ctrl_X, q_final), ~threshold(X = ..1 - ..2,
                                                                         q = ..3,
                                                                         sd_ctrl = ctrl_sd_x,
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
        data = data
      )
    }else{
      xz <- as.matrix(lm.fit(as.matrix(z), as.matrix(x))$fitted.values)
      fit <- lm(y ~ xz)
      names(fit$coefficients)[-1]<- colnames(x)
      return_obj <- list(
        fit = fit
      )
    }
    return(return_obj)

  }
