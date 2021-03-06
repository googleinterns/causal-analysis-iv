#' Generalized instrumental variable with cross validation using summary statistics.
#' @param formula representing relations among variables, following the syntax used in `lm`.
#' @param id experiment id.
#' @param ctrl_id indicators for the control arms.
#' @param ctrl_sd_x estimated standard error for the group-level means for the independent variables.
#' @param ctrl_sd_y estimated standard error for the group-level means for the dependent variables.
#' @param data data frame with all the variables in the model specified in formula.
#' @param L0 a logic statement whether to apply the L0 regularization and cross validation or not.
#' @param formula representing relations among variables, following the syntax used in `lm`.
#' `r y ~ x | z` means the first stage OLS is `r x ~ z` and the second stage is `y ~ x`.
#' @param wt for the second stage regression.
#' @references
#' Peysakhovich, A., & Eckles, D., Learning causal effects from many randomized experiments
#' using regularized instrumental variables. \emph{Proceedings of the 2018 World Wide Web Conference} (2018, April).
#' URL https://dl.acm.org/doi/10.1145/3178876.3186151
#' @return a list that contains the final tuning parameter, the data set after L0 regularization and a lm fitted object.
#' @import purrr dplyr Formula magrittr tidyr  tible
#' @importFrom magrittr "%>%"
#' @examples
#' # true effect is .1, .2, .3
#' J = 2000
#' I = 100
#' data_gIVCVs <- sim_gIV(beta_ctrl = .3,
#'                       beta_X = c(.1, .1, .1),
#'                       J = J, I = I)
#'data_gIVCVs %>% head()


#' # generalized IVCV with L0
#'gIVCVs(y ~ x1 + x2 + x3,
#'       id = id,
#'       ctrl_id = ctrl_id,
#'       ctrl_sd_x = sqrt(2/J) %>%
#'         rep_col(3) %>%
#'         rep_row(J),
#'      ctrl_sd_y = rep(sqrt(3/J), J),
#'      L0 = T,
#'      data = data_gIVCVs)$fit
gIVCVs <-
  function(formula,
           id,
           ctrl_id,
           ctrl_sd_x,
           ctrl_sd_y,
           data,
           subset = NULL,
           weights = NULL,
           wt = NULL,
           instruments = NULL,
           na.action = NULL,
           L0 = F,
           theta = t(c(1, 1)),
           ...) {
    # experiment id indicator
    id <- data %>% dplyr::pull(.data$id)
    # the indicator whether one is on the treatment arm or control arm
    ctrl_id <- data %>% dplyr::pull({
      {
        ctrl_id
      }
    })

    # build model frame
    mf <- match.call(expand.dots = T)


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

    f <- if (!is.null(instruments)) {
      Formula::as.Formula(formula, instruments)
    } else {
      Formula::as.Formula(formula)
    }

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

    data <- tibble(
      id = id,
      Y = y,
      X = x,
      ctrl_id = ctrl_id
    )
    x <- as.matrix(x)
    # L0 regularization
    if (L0 == T) {
      dim_X = ncol(data$X)
      dat_cv <- data %>%
        dplyr::group_by(id, ctrl_id) %>%
        dplyr::filter(ctrl_id == 1) %>%
        dplyr::summarise(mean_X = purrr::map(list(X), ~ colMeans(X)),
                         mean_Y = mean(Y))

      dat_cv_ctrl <- data %>%
        dplyr::group_by(id, ctrl_id) %>%
        dplyr::filter(ctrl_id == 0) %>%
        dplyr::summarise(mean_X_ctrl = purrr::map(list(X), ~ colMeans(X)),
                         mean_Y_ctrl = mean(Y)) %>%
        tidyr::unchop(mean_X_ctrl) %>%
        tibble::add_column(var_id = rep(1:dim_X, J)) %>%
        tidyr::pivot_wider(
          names_from = var_id,
          values_from = mean_X_ctrl,
          names_glue = "{.value}_{var_id}"
        )  %>%
        ungroup()


      dat_cv <- dat_cv %>%
        dplyr::mutate(mean_Y_cv_cond = purrr::map(
          list(mean_Y),
          ~ cond_dis(
            mu = rep(.x, 2),
            sigma = diag(rep((ctrl_sd_y[row_number()]) ^ 2, 2)),
            tau = 2 * .x,
            theta = theta
          )
        )) %>%
        dplyr::mutate(mean_Y_cv = purrr::map(
          mean_Y_cv_cond,
          ~ MASS::mvrnorm(
            n = 1,
            mu = .x$mu_cond,
            Sigma = .x$sigma_cond
          )
        )) %>%
        dplyr::mutate(mean_X = purrr::map(mean_X, ~ unname(.x))) %>%
        tibble::add_column(ctrl_sd_x = ctrl_sd_x) %>%
        dplyr::mutate(mean_X_cv_cond = purrr::map(mean_X, ~ purrr::imap(
          ., ~ cond_dis(mu = rep(.x, 2), diag(rep((ctrl_sd_x[.y]) ^ 2, 2
          )), 2 * .x, theta)
        )))  %>%
        dplyr::mutate(mean_X_cv = purrr::map_depth(
          mean_X_cv_cond,
          .depth = 2,
          ~ MASS::mvrnorm(
            n = 1,
            mu = .x$mu_cond,
            Sigma = .x$sigma_cond
          )
        )) %>%
        dplyr::mutate(mean_X_cv = purrr::map(mean_X_cv, ~ rearrange(list = .x))) %>%
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
        ) %>%
        dplyr::ungroup()



      mean_X_1 <-
        dat_cv %>% dplyr::select(starts_with("mean_X_cv_1"))
      mean_Y_1 <-
        dat_cv %>% dplyr::select(starts_with("mean_Y_cv_1"))
      mean_X_2 <-
        dat_cv %>% dplyr::select(starts_with("mean_X_cv_2"))
      mean_Y_2 <-
        dat_cv %>% dplyr::select(starts_with("mean_Y_cv_2"))


      # sd_ctrl = sqrt(var(dat_ctrl)/(I/2)) %>% diag() %>% max()
      q <- plogis(seq(-30, 0, length.out = 1000))

      ctrl_X <- dat_cv_ctrl %>%
        dplyr::select(starts_with("mean_X"))
      message("Cross validation...")
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
                              # matrix of sd
                              X_return = ..1
                            )
                          )) %>%
        dplyr::mutate(X_1_q = purrr::map(X, ~ as.matrix(.x))) %>%
        dplyr::mutate(fit = purrr::pmap(list(Y_1, X_1_q), ~ lm(..1 ~ ..2, weights = wt))) %>%
        dplyr::mutate(Y_hat = purrr::pmap(list(fit, X_1), ~ predict(..1, newdata = as.data.frame(..2)))) %>%
        dplyr::mutate(error = purrr::pmap_dbl(list(Y_2, Y_hat), ~ mean((..1-..2) ^ 2))) %>%
        dplyr::slice(which.min(error)) %>% pull(q)
      message(paste("Tuning parameter q selected:", q_final))

      dat_final <-
        data %>%
        dplyr::group_by(id, ctrl_id) %>%
        dplyr::filter(ctrl_id == 1) %>%
        dplyr::summarise(mean_X = purrr::map(list(X), ~ colMeans(X)),
                         mean_Y = mean(Y))
      data_x <- do.call(rbind, dat_final$mean_X)
      mean_X_q = threshold(
        X = as.matrix(data_x) - as.matrix(ctrl_X),
        q = q_final,
        sd_ctrl = ctrl_sd_x,
        X_return = data_x
      )

      Y <-
        c(dat_cv_ctrl$mean_Y_ctrl, dat_final$mean_Y) %>%
        as.matrix()  %>%
        magrittr::set_colnames(.,
                               sub("^(\\w*)\\s~\\s.*$",
                                   "\\1",
                                   deparse(f)))


      mean_X_q %>%
        magrittr::set_colnames(c("x1", "x2", "x3")) %>%
        rbind(as.matrix(unname(ctrl_X)))
      X <-
        mean_X_q %>%
        magrittr::set_colnames(c("x1", "x2", "x3")) %>%
        rbind(as.matrix(unname(ctrl_X)))

      id = c(dat_cv_ctrl$id, dat_final$id)
      ctrl_id = c(dat_cv_ctrl$ctrl_id, dat_final$ctrl_id)
      fit <- lm(Y ~ X + ctrl_id, weights = wt, ...)
      names(fit$coefficients)[-1] <- c(colnames(x), "ctrl_id")
      return_obj <- list(
        q_final = q_final,
        fit = fit,
        data = data.frame(id, ctrl_id, Y, X) %>% arrange(id)
      )
    } else{
      fit <- lm(y ~ x + ctrl_id)
      names(fit$coefficients)[-1] <- c(colnames(x), "ctrl_id")
      return_obj <- fit
    }
    return(return_obj)
  }
