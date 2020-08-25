#' @title Instrumental variable with cross validation with summary statistics.
#'
#' @keywords instrumental variable
#' @references
#' Peysakhovich, A., & Eckles, D., Learning causal effects from many randomized experiments
#' using regularized instrumental variables.
#' \emph{Proceedings of the 2018 World Wide Web Conference} (2018, April).
#' URL https://dl.acm.org/doi/10.1145/3178876.3186151
#' @param formula representing relations among variables, following the syntax used in `lm`.
#' `y ~ x | z` means the first stage OLS is `x ~ z` and the second stage is `y ~ x`.
#' @param id experiment id.
#' @param ctrl_id indicators for the control arm.
#' @param instruments model for the first stage OLS, omitted when use `|` syntax in formula.
#' @param raw if run TSLS with the raw data
#' @param L0 a logic statement whether to apply the L0 regularization and cross validation or not.
#' @param wt for the second stage regression.
#' @return a list that contains the final tuning parameter, the data set after L0 regularization and a lm fitted object.
#' @import purrr dplyr Formula magrittr tidyr tibble
#' @examples

#' # true effect: .1 .2 .3
#' I = 100
#' J = 500
#' dat_smy <- sim_IV(I = I,
#'                   J = J,
#'                   beta_X = c(.1, .2, .3),
#'                   ctrl_id = 1)
#' ctrl_sd_x <-
#'   dat_smy %>% filter(id == 1) %>%
#'   select(starts_with("x")) %>%
#'   var() %>%
#'   diag() %>%
#'   sqrt()/sqrt(I) %>%
#'   as.double()
#'
#' ctrl_sd_y <-
#'   dat_smy %>% filter(id == 1) %>%
#'   select(starts_with("y")) %>%
#'   var() %>%
#'   diag() %>%
#'   sqrt()/sqrt(I) %>%
#'   as.double()
#'
#'
#' data_mean <-
#'   dat_smy %>%
#'   group_by(id) %>%
#'   summarise_all(mean)

#' sim_gIV() %>% head %>% kable()
#' fit_smy_tsls <- sIVCV(formula = y ~ x1 + x2 + x3 | factor(id),
#'                       id = id,
#'                       ctrl_id = 1,
#'                       data = data_mean,
#'                       L0 = F)$fit
#' fit_smy_ivcv <- sIVCV(formula = y ~ x1 + x2 + x3 | factor(id),
#'                       id = id,
#'                       ctrl_id = 1,
#'                       data = data_mean,
#'                       ctrl_sd_x = ctrl_sd_x,
#'                       ctrl_sd_y = ctrl_sd_y,
#'                       L0 = T)$fit
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
           theta = t(c(1, 1)),
           ...) {
    # build model frame
    mf <- match.call(expand.dots = T)
    id <- data %>% pull({
      {
        id
      }
    })


    # of experiments
    J = length(unique(id))

    m <-
      match(
        c(
          "formula",
          "id",
          "data",
          "weights",
          "subset",
          "instruments" ,
          "na.action"
        ),
        names(mf),
        0
      )
    mf <- mf[c(1, m)]

    f <- if (!is.null(instruments))
      Formula::as.Formula(formula, instruments)
    else
      Formula::as.Formula(formula)
    stopifnot(isTRUE(all.equal(length(f), c(1, 2))))

    mf[[1]] <- as.name("model.frame")
    mf$formula <- f
    mf$instruments <- NULL
    mf$na.action <- NULL


    mf <- eval(mf, parent.frame())
    y <- model.response(mf)
    w <- as.vector(model.weights(mf))

    x <- model.matrix(f, data = mf, rhs = 1) %>%
      dplyr::as_tibble() %>%
      dplyr::select(-`(Intercept)`)
    z <- model.matrix(f, data = mf, rhs = 2) %>%
      dplyr::as_tibble() %>%
      dplyr::select(-`(Intercept)`)
    dat <- dplyr::tibble(id = id,
                         Y = y,
                         X = x,
                         Z = z)

    # xz <- as.matrix(lm.fit(z, x)$fitted.values)
    #  lm.fit(xz, y)$coefficients
    if (L0 == T) {
      dat_ctrl <- dat %>% filter(id == ctrl_id) %>% pull(X)
      dim_X = ncol(dat$X)

      dat_cv <- dat %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(mean_X = purrr::map(list(X), ~ colMeans(X)),
                         mean_Y = mean(Y))


      dat_cv <- dat_cv %>%
        dplyr::mutate(mean_Y_cv_cond = purrr::map(mean_Y, ~ cond_dis(mu = rep(.x, 2), diag(
          rep(ctrl_sd_y ^ 2, 2)
        ), 2 * .x, theta))) %>%
        dplyr::mutate(mean_Y_cv = purrr::map(
          mean_Y_cv_cond,
          ~ MASS::mvrnorm(
            n = 1,
            mu = .x$mu_cond,
            Sigma = .x$sigma_cond
          )
        )) %>%
        dplyr::mutate(mean_X = purrr::map(mean_X, ~ unname(.x))) %>%
        tibble::add_column(ctrl_sd_x = rep_row(ctrl_sd_x, J)) %>%
        mutate(mean_X_cv_cond = purrr::map(mean_X, ~ purrr::imap(
          ., ~ cond_dis(mu = rep(.x, 2), diag(rep(ctrl_sd_x[.y], 2)), 2 * .x, theta)
        ))) %>%
        dplyr::mutate(mean_X_cv = purrr::map_depth(
          mean_X_cv_cond,
          .depth = 2,
          ~ MASS::mvrnorm(
            n = 1,
            mu = .x$mu_cond,
            Sigma = .x$sigma_cond
          )
        )) %>%
        dplyr::mutate(mean_X_cv = purrr::map(mean_X_cv, ~ rearrange(
          list = .x,
          n_list = 2,
          n_var = dim_X
        ))) %>%
        tibble::add_column(cv_id = list((c(1, 2)))) %>%
        tidyr::unnest(c(cv_id, mean_Y_cv, mean_X_cv)) %>%
        dplyr::mutate(mean_X_cv = purrr::map(mean_X_cv, ~ unlist(.x))) %>%
        tidyr::pivot_wider(names_from = cv_id,
                           values_from = c(mean_Y_cv, mean_X_cv)) %>%
        tidyr::unchop(c(mean_X_cv_1 , mean_X_cv_2)) %>%
        tibble::add_column(var_id = rep(1:dim_X, J)) %>%
        tidyr::pivot_wider(
          names_from = var_id,
          values_from = c(mean_X_cv_1, mean_X_cv_2),
          names_glue = "{.value}_{var_id}"
        )


      mean_X_1 <-
        dat_cv %>% dplyr::select(starts_with("mean_X_cv_1"))
      mean_Y_1 <-
        dat_cv %>% dplyr::select(starts_with("mean_Y_cv_1"))
      mean_X_2 <-
        dat_cv %>% dplyr::select(starts_with("mean_X_cv_2"))
      mean_Y_2 <-
        dat_cv %>% dplyr::select(starts_with("mean_Y_cv_2"))


      #sd_ctrl = sqrt(var(dat_ctrl)/(I/2)) %>% diag() %>% max()
      q <- plogis(seq(-30, 0, length.out = 1000))
      message("Cross validation...")

      ctrl_X <- mean_X_1[ctrl_id, ] %>% rep_row(J)
      # sd_ctrl = .1
      q_final <- dplyr::tibble(
        q,
        X_1 = list(as.matrix(mean_X_1)),
        X_2 = list(as.matrix(mean_X_2)),
        Y_1 = list(as.matrix(mean_Y_1)),
        Y_2 = list(as.matrix(mean_Y_2)),
        ctrl_X = list(as.matrix(ctrl_X))
      ) %>% dplyr::mutate(id = row_number(),
                          X = purrr::pmap(
                            list(X_1, ctrl_X, q),
                            ~ threshold(
                              X = ..1-..2,
                              q = ..3,
                              sd_ctrl = ctrl_sd_x,
                              X_return = ..1
                            )
                          )) %>%
        dplyr::mutate(X_1_q = purrr::map(X, ~ as.matrix(.x))) %>%
        dplyr::mutate(fit = purrr::pmap(list(Y_1, X_1_q), ~ lm(..1 ~ ..2, weights = wt))) %>%
        dplyr::mutate(Y_hat = purrr::pmap(list(fit, X_1), ~ predict(..1, newdata = as.data.frame(..2)))) %>%
        dplyr::mutate(error = purrr::pmap_dbl(list(Y_2, Y_hat), ~ mean((..1-..2) ^ 2))) %>%
        dplyr::slice(which.min(error)) %>% pull(q)
      message(paste("Tuning parameter q selected:", q_final))


      dat_final <- dat %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(mean_X = purrr::map(list(X), ~ colMeans(X)),
                         mean_Y = mean(Y)) %>%
        dplyr::mutate(ctrl_X = list(as.matrix(ctrl_X[ctrl_id, ]))) %>%
        dplyr::mutate(mean_X_q = purrr::pmap(
          list(mean_X, ctrl_X, q_final),
          ~ threshold(
            X = ..1-..2,
            q = ..3,
            sd_ctrl = ctrl_sd_x,
            X_return = ..1
          )
        ))

      Y <-
        dat_final$mean_Y %>%
        as.matrix()  %>%
        magrittr::set_colnames(.,
                             sub("^(\\w*)\\s~\\s.*$",
                                 "\\1",
                                 deparse(f)))

      X <-
        do.call(rbind,
                dat_final$mean_X_q) %>%
        magrittr::set_colnames(colnames(x)) %>%
        as.matrix()
      fit <- lm(Y ~ X, weights = wt, ...)
      names(fit$coefficients)[-1] <- colnames(x)
      return_obj <- list(q_final = q_final,
                         fit = fit,
                         data = data)
    } else{
      xz <- as.matrix(lm.fit(as.matrix(z), as.matrix(x))$fitted.values)
      fit <- lm(y ~ xz)
      names(fit$coefficients)[-1] <- colnames(x)
      return_obj <- list(fit = fit)
    }
    return(return_obj)

  }
