test_that("simulate data with expected format", {
  # default
  beta_X <- c(1, 2, 3)
  x_col_names <-
    sim_IV(beta_X = beta_X) %>%
    head() %>%
    select(starts_with("x")) %>%
    colnames()
  expect_equal(x_col_names,
               paste0("x",
                      seq_along(beta_X)))
  })
