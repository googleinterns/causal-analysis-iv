test_that("generally works", {
  library(dplyr)
  library(purrr)
  data <- sim_gIV()
  # IVCV(formula = y ~ x1 + x2 + x3 | factor(id),
  #      id = id,
  #      ctrl_id = 1,
  #      data = data,
  #      L0 = T)$fit
  # IVCV(formula = y ~ x1 + x2 + x3 | factor(id),
  #      id = id,
  #      ctrl_id = 1,
  #      data = data,
  #      L0 = F)$fit
  })
