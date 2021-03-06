#' @title Shiny app for the data generating process of random intercept models.
#'
#' @return return a new Shinyapp session.
#' @example
#' demo_RE()
#' @importFrom shiny lme4 dplyr knitr tidyr broom.mixed kableExtra sjstats
select <- dplyr::select
demo_RE <- function() {
  # UI end ----
  ui <- fluidPage(pageWithSidebar(
    headerPanel(
      windowTitle = "Random Intercept Models (One-way Error Component Model)",
      div("Random Intercept Models (One-way Error Component Model)")
    ),

    sidebarPanel(
      div(
        br(),
        br(),
        br(),
        sliderInput(
          "n",
          "Cluster size",
          min = 2,
          max = 100,
          step = 2,
          value = 2,
          ticks = FALSE
        ),
        sliderInput(
          "k",
          "Number of cluster",
          min = 6,
          max = 100,
          step = 2,
          value = 10,
          ticks = FALSE
        ),
        sliderInput(
          "b0",
          "Intercept (\\(\\beta_{1}\\))",
          min = -5,
          max = 5,
          step = .1,
          value = 0,
          ticks = FALSE
        ),
        sliderInput(
          "b1",
          "Coef of X (\\(\\beta_{2}\\))",
          min = -5,
          max = 5,
          step = .1,
          value = 0,
          ticks = FALSE
        ),
        sliderInput(
          "var_ri",
          "Random intercept variance (\\(\\psi\\))",
          min = 0,
          max = 5,
          step = .05,
          value = 1,
          ticks = FALSE
        ),
        sliderInput(
          "var_e",
          "Error variance (\\(\\theta\\))",
          min = 0,
          max = 5,
          step = .05,
          value = 1,
          ticks = FALSE
        ),
        br(),
        br(),
        actionButton("resample", "Regenerate data")
      )
    ),

    mainPanel(div(
      plotOutput("reg_plot", width = 600, height = 480)
    ),
    div(
      class = "span6",
      uiOutput('table'),
      uiOutput('ex2')
    ))

  ))



  # Server end ----
  server <- function(input, output) {
    rand_sample <- reactive({
      resam <- input$resample
      n <- input$n
      k <- input$k

      b0 <- input$b0
      b1 <- input$b1

      var_ri <- input$var_ri
      var_e <- input$var_e
      sd_ri <- sqrt(as.numeric(var_ri))
      sd_e <- sqrt(as.numeric(var_e))

      # random intercept
      ri <- rnorm(k, sd = sd_ri)

      icc = var_ri / (var_e + var_ri)
      dat <- tibble(ri) %>%
        mutate(
          id = 1:n() %>% as.factor(),
          e = map(id, ~ rnorm(n = n, sd = sd_e)),
          x = map(id, ~ runif(n = n, -2, 6))
        ) %>%
        mutate(y = pmap(list(ri, x, e),
                        ~ ..1+b0 + b1 * ..2+..3)) %>%
        unchop(c(e, y, x))


      return(list(
        dat = dat,
        icc = icc,
        b0 = b0,
        b1 = b1,
        ri = ri,
        sd_ri = sd_ri,
        sd_e = sd_e
      ))

    })



    vcm_fit <- reactive({
      # Get the current model structure
      dat <- rand_sample()$dat
      icc <- rand_sample()$icc
      lmer_fit <- lmer(y ~ x + (1 | id), dat)
      icc_est <- icc(lmer_fit)
      lmer_fit_tidy <- tidy(lmer_fit)

      tidy_out <-
        tidy(lmer_fit) %>%
        add_column(
          `generating parameter` = c(
            rand_sample()$b0,
            rand_sample()$b1,
            rand_sample()$sd_ri ^ 2,
            rand_sample()$sd_e ^ 2
          ),
          .before = "estimate"
        ) %>% mutate(
          estimate = c(
            lmer_fit_tidy$estimate[1],
            lmer_fit_tidy$estimate[2],
            lmer_fit_tidy$estimate[3] ^ 2,
            lmer_fit_tidy$estimate[4] ^ 2
          )
        ) %>%
        select(-group, -effect, -term) %>% add_row(`generating parameter` = icc,
                                                   estimate = icc_est$ICC_adjusted) %>%
        mutate(
          description = c(
            "fixed intercept",
            "fixed coef for X" ,
            "random intercept variance",
            "error variance",
            "intraclass correlation"
          ) ,
          .before = `generating parameter`
        ) %>%
        as.data.frame()

      rownames(tidy_out) <- c("β1", "β2", "ψ", "θ", "ICC")
      tidy_out <- tidy_out %>%
        kable(format = "html",
              row.names = T,
              escape = FALSE)


      # Get the model summary
      if (is.null(lmer_fit)) {
        fit_sum <- NULL
      } else {
        fit_sum <- summary(lmer_fit)
      }

      return(list(lmer_fit = lmer_fit,
                  tidy_out = tidy_out))

    })

    output$reg_plot <- renderPlot({
      dat <- rand_sample()$dat
      b0 = rand_sample()$b0
      b1 = rand_sample()$b1
      ri = rand_sample()$ri

      ri_make <- function(ri, b0, b1) {
        function(x) {
          ri + b0 + b1 * x
        }
      }
      funcs <- tibble(ri, b0, b1) %>%
        mutate(fun_list = pmap(list(ri, b0, b1),
                               ~ ri_make(..1, ..2, ..3))) %>% pull(fun_list)

      p <-
        ggplot(aes(y = y, x = x, color = id), data = dat) + geom_point() + xlim(-2, 6)

      g <- ggplot_build(p)
      g_colour <- unique(g$data[[1]]["colour"])
      for (i in 1:length(funcs))
        p <-
        p + stat_function(fun = funcs[[i]], colour = g_colour[i, ])
      print(p)
    })



    output$table <- reactive({
      vcm_fit()$tidy_out %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("striped", "hover", "condensed"),
          protect_latex = TRUE
        )
    })

    output$ex2 <- renderUI({
      withMathJax(
        helpText(
          '$$\\text {data generating model: }y_{i j}=\\underbrace{\\beta_1+\\beta_{2} x_{i j}}_{\\text {fixed part }}+\\underbrace{\\zeta_{j}+\\epsilon_{i j}}_{\\text {random part } \\xi_{i j}}$$'
        ),
        helpText(
          ' $$\\text {where }\\zeta_{j}\\left|x_{i j} \\sim N(0, \\psi), \\quad \\epsilon_{i j}\\right| x_{i j}, \\zeta_{j} \\sim N(0, \\theta)$$'
        ),
        helpText(' $$\\text {ICC}\\equiv \\frac{\\psi}{\\psi+\\theta} $$')
      )
    })


  }
  shinyApp(ui, server)
}
