
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gIVCV

<!-- badges: start -->

<!-- badges: end -->

The goal of gIVCV is to implement instrumental variable with cross
validation for massive A/B test experiments.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gIVCV)
library(MASS)
#> 
#> Attaching package: 'MASS'
#> The following object is masked from 'package:gIVCV':
#> 
#>     select
library(tidyverse)
#> ── Attaching packages ───────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.1     ✓ purrr   0.3.4
#> ✓ tibble  3.0.1     ✓ dplyr   1.0.0
#> ✓ tidyr   1.1.0     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.5.0
#> ── Conflicts ──────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
#> x dplyr::select() masks MASS::select(), gIVCV::select()
library(Formula)
library(magrittr)
#> 
#> Attaching package: 'magrittr'
#> The following object is masked from 'package:purrr':
#> 
#>     set_names
#> The following object is masked from 'package:tidyr':
#> 
#>     extract
library(sjPlot)
library(gIVCV)
library(knitr)
select <- dplyr::select
## basic example code
```

# IVCV with raw data

``` r
# true effect: .1 .2 .3
I = 100
J = 500

dat_raw <- sim_IV(I = I, # experiment size
              J = J, # number of experiment
              beta_X = c(.1, .2, .3), # effects
              ctrl_id = 1) # the indicator of treatment group


head(dat_raw) %>% kable()
```

| id |          x1 |          x2 |          x3 |           y | z1 | z2 | z3 |
| :- | ----------: | ----------: | ----------: | ----------: | -: | -: | -: |
| 1  | \-3.1196687 | \-3.7637576 | \-2.9235185 | \-15.485644 |  0 |  0 |  0 |
| 1  | \-0.6086220 | \-2.9643427 | \-2.0675456 |  \-4.557909 |  0 |  0 |  0 |
| 1  | \-1.1750888 | \-0.6738290 | \-1.1558850 |  \-4.851155 |  0 |  0 |  0 |
| 1  | \-0.4442821 | \-0.3351100 |   0.1888315 |  \-1.266059 |  0 |  0 |  0 |
| 1  | \-1.0952521 |   0.3195877 | \-0.8161754 |  \-3.911370 |  0 |  0 |  0 |
| 1  | \-1.0808578 | \-0.3770526 | \-2.2857056 |  \-2.495555 |  0 |  0 |  0 |

``` r

# TSLS
fit_raw_tsls <- IVCV(y ~ x1 + x2 + x3 | factor(id),
     id = id,
     data = dat_raw,
     L0 = F)$fit

# IVCV
fit_raw_ivcv <- IVCV(y ~ x1 + x2 + x3 | factor(id),
     id = id,
     ctrl_id = 1,
     data = dat_raw,
     L0 = T)$fit
#> Cross validation...
#> Tuning parameter q selected: 0.240278883409322

tab_model(
  fit_raw_tsls,
  fit_raw_ivcv,
  show.ci = FALSE,
  show.se = TRUE,
  auto.label = FALSE,
  string.se = "SE",
  show.icc = TRUE,
  show.r2 = FALSE,
  show.aic = FALSE,
  show.obs = T,
  title = "True effect is .1, .2, .3, respectively for x1, x2, x3",
  dv.labels = c("TSLS with raw data",
                "IVCV with raw data"),
  digits = 5
)
```

<table style="border-collapse:collapse; border:none;">

<caption style="font-weight: bold; text-align:left;">

True effect is .1, .2, .3, respectively for x1, x2,
x3

</caption>

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

TSLS with raw
data

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

IVCV with raw
data

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

SE

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

SE

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.03510

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02204

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.112

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.03834

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02207

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

0.083

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x1

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.13329

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02844

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.13115

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02852

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x2

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.15992

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02604

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.15955

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02611

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x3

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.36849

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02699

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.37004

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02705

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

500

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

500

</td>

</tr>

</table>

# IVCV with summary

``` r
# true effect: .1 .2 .3
I = 100
J = 500
dat_smy <- sim_IV(I = I,
               J = J,
               beta_X = c(.1, .2, .3),
               ctrl_id = 1)
ctrl_sd_x <-
  dat_smy %>% filter(id == 1) %>%
  select(starts_with("x")) %>%
  var() %>%
  diag() %>%
  sqrt()/sqrt(I) %>%
  as.double()

ctrl_sd_y <-
  dat_smy %>% filter(id == 1) %>%
  select(starts_with("y")) %>%
  var() %>%
  diag() %>%
  sqrt()/sqrt(I) %>%
  as.double()


data_mean <-
  dat_smy %>%
  group_by(id) %>%
  summarise_all(mean)
sim_gIV() %>% head %>% kable()
```

| id | ctrl\_id |        x1 |        x2 |          x3 |           y |       z1 |        z2 |          z3 |          u1 |          u2 |          u3 |
| :- | -------: | --------: | --------: | ----------: | ----------: | -------: | --------: | ----------: | ----------: | ----------: | ----------: |
| 1  |        0 | 0.4663323 | 0.3458796 |   0.3957534 |   1.0601548 | 0.000000 | 0.0000000 |   0.0000000 |   0.4185372 |   0.0295926 |   0.1536422 |
| 1  |        1 | 0.1924017 | 0.1436132 |   0.3373064 |   1.0928508 | 0.000000 | 0.0000000 |   0.0000000 | \-0.0426714 |   0.1880261 |   0.0366377 |
| 2  |        0 | 1.2008723 | 0.6827388 |   0.1663055 | \-0.3017467 | 1.073788 | 0.3394452 | \-0.2673223 |   0.1306809 |   0.0970395 | \-0.2229372 |
| 2  |        1 | 1.1636567 | 0.7542416 | \-0.0808052 |   1.6271563 | 1.073788 | 0.3394452 | \-0.2673223 | \-0.1844821 |   0.1809695 |   0.2757669 |
| 3  |        0 | 0.1794828 | 0.2052314 |   0.3820199 |   0.6741503 | 0.000000 | 0.0000000 |   0.0000000 |   0.0330036 |   0.1606385 |   0.1200606 |
| 3  |        1 | 0.2577042 | 0.4234567 |   0.0745322 |   1.0665381 | 0.000000 | 0.0000000 |   0.0000000 |   0.0561981 | \-0.1128025 |   0.2537819 |

``` r
fit_smy_tsls <- sIVCV(formula = y ~ x1 + x2 + x3 | factor(id),
                 id = id,
                 ctrl_id = 1,
                 data = data_mean,
                 L0 = F)$fit

theta <- t(c(1, 1))
fit_smy_ivcv <- sIVCV(formula = y ~ x1 + x2 + x3 | factor(id),
                 id = id,
                 ctrl_id = 1,
                 data = data_mean,
                 ctrl_sd_x = ctrl_sd_x,
                 ctrl_sd_y = ctrl_sd_y,
                 L0 = T)$fit
#> Cross validation...
#> Tuning parameter q selected: 0.145322130172085
tab_model(
  fit_smy_tsls,
  fit_smy_ivcv,
  show.ci = FALSE,
  show.se = TRUE,
  auto.label = FALSE,
  string.se = "SE",
  show.icc = TRUE,
  show.r2 = FALSE,
  show.aic = FALSE,
  show.obs = T,
  title = "True effect is .1, .2, .3, respectively for x1, x2, x3",
  dv.labels = c("TSLS with summary data",
                "IVCV with summary data"),
  digits = 5
)
```

<table style="border-collapse:collapse; border:none;">

<caption style="font-weight: bold; text-align:left;">

True effect is .1, .2, .3, respectively for x1, x2,
x3

</caption>

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

TSLS with summary
data

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

IVCV with summary
data

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

SE

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

SE

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.02095

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02307

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.364

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.02590

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02311

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

0.263

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x1

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.07084

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02872

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>0.014</strong>

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.06940

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02866

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>0.016</strong>

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x2

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.22716

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02743

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.22767

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02745

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x3

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.36806

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02982

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.37082

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.02968

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

500

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

500

</td>

</tr>

</table>

# gIVCV for summary data

``` r
# true effect is .1, .2, .3
J = 500
I = 100
data_gIVCVs <- sim_gIV(beta_ctrl = .3,
                       beta_X = c(.1, .2, .3),
                       J = J,
                       I = I,
                       summary_data = F)
data_gIVCVs_sd <-
  data_gIVCVs %>%
  add_column(var_id = list((1:3))) %>%
        unchop(c(var_id, X)) %>%
  pivot_wider(names_from = var_id, values_from = X, names_glue = "{.value}{var_id}") %>%
  filter(ctrl_id == 0) %>%
  group_by(id) %>%
  summarise(ctrl_sd_x1 = sd(X1)/sqrt(I),
            ctrl_sd_x2 = sd(X2)/sqrt(I),
            ctrl_sd_x3 = sd(X3)/sqrt(I),
            ctrl_sd_y = sd(Y)/sqrt(I))
#> `summarise()` ungrouping output (override with `.groups` argument)

data_gIVCVs <- sim_gIV(beta_ctrl = .3,
                       beta_X = c(.1, .2, .3),
                       J = J,
                       I = I,
                       summary_data = T)
# generalized IVCV with L0
fit_ivcv_g <-
  gIVCVs(y ~ x1 + x2 + x3,
      id = id,
      ctrl_id = ctrl_id,
      ctrl_sd_x = as.matrix(data_gIVCVs_sd[, 2:4]),
      ctrl_sd_y = data_gIVCVs_sd$ctrl_sd_y,
      L0 = T,
      data = data_gIVCVs)$fit
#> Cross validation...
#> Tuning parameter q selected: 0.270716382818275

# generalized IVCV without L0
fit_tsls_g <-
  gIVCVs(y ~ x1 + x2 + x3,
      id = id,
      ctrl_id = ctrl_id,
      L0 = F,
      data = data_gIVCVs)
fit_tsls_g
#> 
#> Call:
#> lm(formula = y ~ x + ctrl_id)
#> 
#> Coefficients:
#> (Intercept)           x1           x2           x3      ctrl_id  
#>   -0.006208     0.276840     0.286746     0.341765     0.287388


tab_model(
  fit_ivcv_g,
  fit_tsls_g,
  show.ci = FALSE,
  show.se = TRUE,
  auto.label = FALSE,
  string.se = "SE",
  show.icc = TRUE,
  show.r2 = FALSE,
  show.aic = FALSE,
   show.obs = TRUE,
   title = "True effect is .1, .2, .3, .3, respectively for x1, x2, x3, ctrl_id",
  dv.labels = c("gIVCV with summary data",
                "TSLS with summary data"),
  digits = 5
)
```

<table style="border-collapse:collapse; border:none;">

<caption style="font-weight: bold; text-align:left;">

True effect is .1, .2, .3, .3, respectively for x1, x2, x3,
ctrl\_id

</caption>

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

gIVCV with summary
data

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

TSLS with summary
data

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

SE

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

SE

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.01961

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.03777

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.604

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.00621

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.03068

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

0.840

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x1

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.13797

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.04764

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>0.004</strong>

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.27684

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.03701

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x2

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.20053

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.04321

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.28675

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.03489

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x3

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.22438

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.04738

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.34177

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.03740

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

ctrl\_id

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.29061

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.05335

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.28739

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.04335

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

1000

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

1000

</td>

</tr>

</table>

# Model demo

``` r
#demo_RE()
#demo_RE_FE()
```
