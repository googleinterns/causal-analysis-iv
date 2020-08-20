#' #' @title Shiny app for comparing FE and RE.
#' #'
#' #' @return return a new Shinyapp session.
#' #' @example
#' #' @import shiny lme4 knitr dplyr tidyr plm MASS broom.mixed kableExtra sjstats
#' #' demo_RE_FE()
#' select <- dplyr::select
#' demo_RE_FE <- function() {
#'   # UI end ----
#'   ui <- fluidPage(pageWithSidebar(
#'     headerPanel(windowTitle = "FE vs RE",
#'                 div("FE vs RE")),
#'     sidebarPanel(
#'       div(
#'         br(),
#'         br(),
#'         br(),
#'         sliderInput(
#'           "n",
#'           "Experiment size",
#'           min = 2,
#'           max = 100,
#'           step = 2,
#'           value = 2,
#'           ticks = FALSE
#'         ),
#'         sliderInput(
#'           "k",
#'           "Number of experiment",
#'           min = 6,
#'           max = 100,
#'           step = 2,
#'           value = 10,
#'           ticks = FALSE
#'         ),
#'         sliderInput(
#'           "b0",
#'           "Intercept (\\(\\beta_{1}\\))",
#'           min = -5,
#'           max = 5,
#'           step = .1,
#'           value = 0,
#'           ticks = FALSE
#'         ),
#'         sliderInput(
#'           "b1",
#'           "Coef of X (\\(\\beta_{2}\\))",
#'           min = -5,
#'           max = 5,
#'           step = .1,
#'           value = 0,
#'           ticks = FALSE
#'         ),
#'         sliderInput(
#'           "var_ri",
#'           "Random intercept variance (\\(\\psi\\))",
#'           min = 0,
#'           max = 5,
#'           step = .05,
#'           value = 1,
#'           ticks = FALSE
#'         ),
#'         sliderInput(
#'           "var_X",
#'           "Covariate variance (\\(\\psi_X\\))",
#'           min = 0,
#'           max = 5,
#'           step = .05,
#'           value = 1,
#'           ticks = FALSE
#'         ),
#'         sliderInput(
#'           "var_e",
#'           "Error variance (\\(\\theta\\))",
#'           min = 0,
#'           max = 5,
#'           step = .05,
#'           value = 1,
#'           ticks = FALSE
#'         ),
#'         sliderInput(
#'           "cor_rx",
#'           "Correlation between X and the random intercept (\\(\\rho\\))",
#'           min = -1,
#'           max = 1,
#'           step = .05,
#'           value = 0,
#'           ticks = FALSE
#'         ),
#'         br(),
#'         br(),
#'         actionButton("resample", "Regenerate data")
#'       )
#'     ),
#'
#'     mainPanel(div(
#'       plotOutput("reg_plot", width = 600, height = 480)
#'     ),
#'     div(
#'       class = "span6",
#'       uiOutput('table'),
#'       uiOutput('ex2')
#'     ))
#'   ))
#'
#'
#'
#'   # Server end ----
#'   server <- function(input, output) {
#'     rand_sample <- reactive({
#'       # data input
#'       resam <- input$resample
#'       n <- input$n
#'       k <- input$k
#'
#'       b0 <- input$b0
#'       b1 <- input$b1
#'       var_X <- input$var_X
#'       var_ri <- input$var_ri
#'       var_e <- input$var_e
#'       cor_rx <- input$cor_rx
#'
#'
#'       sd_ri <- sqrt(as.numeric(var_ri))
#'       sd_e <- sqrt(as.numeric(var_e))
#'       sd_X <- sqrt(as.numeric(var_X))
#'
#'       ri_x_g <-
#'         MASS::mvrnorm(n = k, c(0, 0), Sigma = matrix(c(1, cor_rx, cor_rx, 1), nrow = 2))
#'       # data generating process
#'       dat <-
#'         data.frame(ri = sd_ri * ri_x_g[, 1], x_true_mean = sd_X * ri_x_g[, 2]) %>%
#'         mutate(
#'           id = 1:n() %>% as.factor(),
#'           e = map(id, ~ rnorm(n, sd = sd_e)),
#'           x_true_demean = map(id, ~ rnorm(n, sd = sd_X))
#'         ) %>%
#'         mutate(x = pmap(list(x_true_mean, x_true_demean), ~ ..1+..2)) %>%
#'         mutate(y = pmap(list(ri, x, e),
#'                         ~ b0 + ..1+b1 * ..2+..3)) %>%
#'         unchop(c(e, y, x)) %>% group_by(id) %>% mutate(x_mean = mean(x), x_demean = x - x_mean) %>% ungroup()
#'
#'
#'       return(list(
#'         dat = dat,
#'         b0 = b0,
#'         b1 = b1,
#'         ri = dat$ri,
#'         sd_ri = sd_ri,
#'         sd_e = sd_e,
#'         sd_X = sd_X
#'       ))
#'
#'     })
#'
#'
#'
#'     vcm_fit <- reactive({
#'       # Get the current model structure
#'       dat <- rand_sample()$dat
#'       fit_ols <- lm(y ~ x, dat)
#'       fit_w <-
#'         plm::plm(
#'           formula = y ~ x,
#'           data = dat,
#'           model = "within",
#'           index = "id"
#'         )
#'       fit_b <-
#'         plm::plm(
#'           formula = y ~ x,
#'           data = dat,
#'           model = "between",
#'           index = "id"
#'         )
#'       omega_b <-
#'         diag(fit_b$vcov)[2] / (diag(fit_b$vcov)[2] + fit_w$vcov)
#'       fit_w$coefficients * omega_b  + (1 - omega_b) * fit_b$coefficients[2]
#'       fit_re <- plm(y ~ x,
#'                     data = dat,
#'                     model = "random",
#'                     index = c("id"))
#'       tb <- tribble(
#'         ~ estimator,
#'         ~ effect,
#'         ~ se,
#'         "ols",
#'         as.double(fit_ols$coefficients[2]),
#'         as.double(sqrt(diag(vcov(
#'           fit
#'         ))[2])),
#'         "between",
#'         as.double(fit_b$coefficients[2]),
#'         as.double(sqrt(diag(fit_b$vcov)[2])),
#'         "within(FE)",
#'         as.double(fit_w$coefficients),
#'         as.double(sqrt(fit_w$vcov)),
#'         "RE",
#'         as.double(fit_re$coefficients[2]),
#'         as.double(sqrt(diag(fit_re$vcov)[2]))
#'       ) %>% as.data.frame() %>% kable(format = "html")
#'
#'
#'       return(list(tb = tb))
#'
#'     })
#'
#'     output$reg_plot <- renderPlot({
#'       dat <- rand_sample()$dat
#'       b0 = rand_sample()$b0
#'       b1 = rand_sample()$b1
#'       ri = rand_sample()$ri
#'       # population RI
#'       #       ri_make <- function(ri, b0, b1) {
#'       #         function(x) {
#'       #           ri + b0 + b1 * x
#'       #         }
#'       #       }
#'       #       funcs <- tibble(ri, b0, b1) %>%
#'       #         mutate(fun_list = pmap(list(ri, b0, b1),
#'       #                                ~ ri_make(..1, ..2, ..3))) %>% pull(fun_list)
#'       #
#'       #       p <-
#'       #         ggplot(aes(y = y, x = x, color = id), data = dat) + geom_point() + xlim(-2, 6)
#'       #
#'       #       g <- ggplot_build(p)
#'       #       g_colour <- unique(g$data[[1]]["colour"])
#'       #       for (i in 1:length(funcs))
#'       #         p <- p + stat_function(fun = funcs[[i]], colour = g_colour[i,])
#'       #       print(p)
#'       ggplot(dat, aes(
#'         y = y,
#'         x = x,
#'         colour = id
#'       )) +
#'         geom_point() + geom_smooth(method = "lm", fill = NA)
#'     })
#'
#'
#'     output$table <- reactive({
#'       vcm_fit()$tb %>%
#'         kable_styling(
#'           font_size = 15,
#'           bootstrap_options = c("striped", "hover", "condensed"),
#'           protect_latex = TRUE
#'         )
#'     })
#'
#'     output$ex2 <- renderUI({
#'       withMathJax(
#'         helpText(
#'           '$$\\text {data generating model: }y_{i j}=\\underbrace{\\beta_1+\\beta_{2} x_{i j}}_{\\text {fixed part }}+\\underbrace{\\zeta_{j}+\\epsilon_{i j}}_{\\text {random part } \\xi_{i j}}$$'
#'         ),
#'         helpText(
#'           ' $$\\text {where }\\zeta_{j}\\left|x_{i j} \\sim N(0, \\psi), \\quad \\epsilon_{i j}\\right| x_{i j}, \\zeta_{j} \\sim N(0, \\theta)$$'
#'         )
#'       )
#'     })
#'
#'
#'   }
#'   shinyApp(ui, server)
#' }
#' demo_RE_FE()
