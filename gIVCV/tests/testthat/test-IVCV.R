test_that("generally works", {
  data <- sim_IV(beta_X = c(.1, .1, .1) ,I = 100, J = 200)
  IVCV(formula = y ~ x1 + x2 + x3 | factor(id),
       id = id,
       ctrl_id = 1,
       data = data,
       L0 = T)$fit
  IVCV(formula = y ~ x1 + x2 + x3 | factor(id),
       id = id,
       ctrl_id = 1,
       data = data,
       L0 = F)$fit
  summary(fit_2sls)


  })
