library(tidyverse)
library(plm)
# maps ----
maps <- read.csv("~/Downloads/maps (1).csv")
dat_maps <- maps %>% 
  filter(metric == "android_metrics::data_app_crash_sessions" |
           metric == "android_metrics::data_app_native_crash_sessions"|
           metric == "android_metrics::system_app_native_crash_sessions"|
           metric == "android_metrics::jank_rate"|
           metric == "android_metrics::net_sessions"|
           metric == "android_metrics::geometric_mean_screen_off_hours_per_battery"|
           metric == "android_metrics::battery_sessions"|
           metric == "search::agsa_first_draw_done_95th_latency"|
           metric == "android_metrics::system_app_anr_sessions"|
           metric == "android_metrics::data_app_anr_sessions"|
           metric == "android_metrics::system_app_crash_sessions"|
           metric == "androidmetrics::ew_drain_rate_screen_off_battery_level") %>% 
  pivot_longer(cols = value.control:value.experiment, names_to = "ifExp", values_to = "metric_value") %>% 
  pivot_wider(id_cols = c(control.id, ifExp), names_from = metric, values_from = c(metric_value)) %>% 
  mutate(
    crash_rate_m1 =  100*((
      `android_metrics::data_app_crash_sessions` + `android_metrics::system_app_native_crash_sessions` +
        `android_metrics::data_app_native_crash_sessions` + `android_metrics::system_app_crash_sessions`
    ) / `android_metrics::net_sessions`)
  ) %>% mutate(
    crash_rate_m2 =  100*((
      `android_metrics::data_app_crash_sessions` + `android_metrics::system_app_native_crash_sessions` +
        `android_metrics::data_app_native_crash_sessions` + `android_metrics::system_app_crash_sessions`
      + `android_metrics::system_app_anr_sessions` + `android_metrics::data_app_anr_sessions`) / `android_metrics::net_sessions`)
  ) %>% mutate(
    crash_rate_crash =  100*((
      `android_metrics::data_app_crash_sessions` +
        `android_metrics::system_app_crash_sessions`
    ) / `android_metrics::net_sessions`)
  ) %>% mutate(
    crash_rate_native =  100*((
      `android_metrics::system_app_native_crash_sessions` +
        `android_metrics::data_app_native_crash_sessions` ) / `android_metrics::net_sessions`)
  ) %>% mutate(
    crash_rate_anr =  100*((
      `android_metrics::system_app_anr_sessions` + `android_metrics::data_app_anr_sessions`) / `android_metrics::net_sessions`)
  ) %>% rename(data_app_crash_sessions = `android_metrics::data_app_crash_sessions`,
               data_app_native_crash_sessions = `android_metrics::data_app_native_crash_sessions`,
               system_app_native_crash_sessions = `android_metrics::system_app_native_crash_sessions`,
               jank_rate = `android_metrics::jank_rate`,
               net_sessions = `android_metrics::net_sessions`,
               geometric_mean_screen_off_hours_per_battery = `android_metrics::geometric_mean_screen_off_hours_per_battery`,
               battery_sessions = `android_metrics::battery_sessions`,
               latency = `search::agsa_first_draw_done_95th_latency`,
               drain_rate = `androidmetrics::ew_drain_rate_screen_off_battery_level`)



power_boot_map <- function(size) {
  boot.index <- sample(unique(dat_maps$control.id), replace = TRUE, size = size)
  dat_maps_boot <-
    do.call(rbind, lapply(boot.index, function(x)
      dat_maps[dat_maps$control.id == x, ]))
  mod_FE_m1 <-
    net_sessions/battery_sessions ~ factor(ifExp) + crash_rate_m1 + jank_rate   + drain_rate
  wi_m1 <-
    plm(
      mod_FE_m1,
      data = dat_maps_boot,
      model = "within",
      index = "control.id",
      weights = battery_sessions
    )
  return(wi_m1)
}

power_boot_map(100)
power_boot_map(dat_maps, size = 5)
map_sim_res <- tibble(size = seq(50, 100, by = 50)) %>% 
  mutate(wi_fit = map(size, ~power_boot_map(.x))) %>% 
  mutate(wi_fit_sum = map(wi_fit, ~summary(.x))) %>% 
  mutate(if_sig = map(wi_fit, ~(.x$coefficients[, 4] < .05)))

map_sim_res$wi_fit_sum
summary(wi_m1)
wi_m1
names(summary(wi_m1)$coefficients[, 4])
n_rep = 500
n_size = 50
map_sim_res_50 <- map(1:n_rep, ~power_boot_map(size = n_size))
map_sim_res_50_sig <- 
  map_sim_res_50 %>% map(~summary(.x)$coefficients[, 4] < .05) %>% 
  map_dfr(~ .x %>% as_tibble(), .id = "name") %>% as.data.frame()
map_sim_res_50_sig$name <- c(names(summary(wi_m1)$coefficients[, 4]))

map_sim_res_50_sig  %>% group_by(name) %>% summarise(mean_sig = mean(value))
map_sim_res_50_power

n_size = 200
n_rep = 100
map_sim_res_200 <- map(1:n_rep, ~power_boot_map(size = n_size))
map_sim_res_200_sig <- 
  map(1:n_rep, ~power_boot_map(size = n_size)) %>% map(~summary(.x)$coefficients[, 4] < .05) %>% 
  map_dfr(~ .x %>% as_tibble(), .id = "name") %>% as.data.frame()
map_sim_res_200_sig$name <- c(names(summary(wi_m1)$coefficients[, 4]))

map_sim_res_200_sig  %>% group_by(name) %>% summarise(mean_sig = mean(value))


n_size = 500
n_rep = 50
map_sim_res_500 <- map(1:n_rep, ~power_boot_map(size = n_size))
map_sim_res_500_sig <- 
  map(1:n_rep, ~power_boot_map(size = n_size)) %>% map(~summary(.x)$coefficients[, 4] < .05) %>% 
  map_dfr(~ .x %>% as_tibble(), .id = "name") %>% as.data.frame()
map_sim_res_500_sig$name <- c(names(summary(wi_m1)$coefficients[, 4]))

map_sim_res_500_sig  %>% group_by(name) %>% summarise(mean_sig = mean(value))


# search ----
agsa <- read.csv("~/Downloads/agsa (4).csv")

dat_agsa <- 
  agsa %>% 
  filter(metric == "android_metrics::data_app_crash_sessions" |
           metric == "android_metrics::data_app_native_crash_sessions"|
           metric == "android_metrics::system_app_native_crash_sessions"|
           metric == "android_metrics::jank_rate"|
           metric == "android_metrics::net_sessions"|
           metric == "android_metrics::geometric_mean_screen_off_hours_per_battery"|
           metric == "android_metrics::battery_sessions"|
           metric == "search::agsa_first_draw_done_95th_latency"|
           metric == "android_metrics::system_app_anr_sessions"|
           metric == "android_metrics::data_app_anr_sessions"|
           metric == "android_metrics::system_app_crash_sessions"|
           metric == "androidmetrics::ew_drain_rate_screen_off_battery_level") %>%
  filter(control.id != 8540245 & control.id != 8555414  & control.id != 8562335) %>% 
  pivot_longer(cols = value.control:value.experiment, names_to = "ifExp", values_to = "metric_value") %>% 
  pivot_wider(id_cols = c(control.id, ifExp), names_from = metric, values_from = c(metric_value)) %>% 
  mutate(
    crash_rate_m1 =  100*((
      `android_metrics::data_app_crash_sessions` + `android_metrics::system_app_native_crash_sessions` +
        `android_metrics::data_app_native_crash_sessions` + `android_metrics::system_app_crash_sessions`
    ) / `android_metrics::net_sessions`)
  ) %>% mutate(
    crash_rate_m2 =  100*((
      `android_metrics::data_app_crash_sessions` + `android_metrics::system_app_native_crash_sessions` +
        `android_metrics::data_app_native_crash_sessions` + `android_metrics::system_app_crash_sessions`
      + `android_metrics::system_app_anr_sessions` + `android_metrics::data_app_anr_sessions`) / `android_metrics::net_sessions`)
  ) %>% mutate(
    crash_rate_crash =  100*((
      `android_metrics::data_app_crash_sessions` +
        `android_metrics::system_app_crash_sessions`
    ) / `android_metrics::net_sessions`)
  ) %>% mutate(
    crash_rate_native =  100*((
      `android_metrics::system_app_native_crash_sessions` +
        `android_metrics::data_app_native_crash_sessions` ) / `android_metrics::net_sessions`)
  ) %>% mutate(
    crash_rate_anr =  100*((
      `android_metrics::system_app_anr_sessions` + `android_metrics::data_app_anr_sessions`) / `android_metrics::net_sessions`)
  ) %>%  rename(data_app_crash_sessions = `android_metrics::data_app_crash_sessions`,
                data_app_native_crash_sessions = `android_metrics::data_app_native_crash_sessions`,
                system_app_native_crash_sessions = `android_metrics::system_app_native_crash_sessions`,
                jank_rate = `android_metrics::jank_rate`,
                net_sessions = `android_metrics::net_sessions`,
                geometric_mean_screen_off_hours_per_battery = `android_metrics::geometric_mean_screen_off_hours_per_battery`,
                battery_sessions = `android_metrics::battery_sessions`,
                latency = `search::agsa_first_draw_done_95th_latency`,
                drain_rate = `androidmetrics::ew_drain_rate_screen_off_battery_level`)
mod_FE_m1 <- net_sessions ~ factor(ifExp) + crash_rate_m1 + jank_rate + latency + drain_rate
wi_m1 <-
  plm(
    mod_FE_m1,
    data = dat_agsa,
    model = "within",
    index = "control.id",
    weights = battery_sessions
  )
size = 50

power_boot_map <- function(size) {
  boot.index <- sample(unique(dat_agsa$control.id), replace = TRUE, size = size)
  dat_agsa_boot <-
    do.call(rbind, lapply(boot.index, function(x)
      dat_agsa[dat_agsa$control.id == x, ]))
   wi_m1 <-
    plm(
      mod_FE_m1,
      data = dat_agsa_boot,
      model = "within",
      index = "control.id",
      weights = battery_sessions
    )
  return(wi_m1)
}

n_size = 300
n_rep = 500
map_sim_res_50 <- map(1:n_rep, ~power_boot_map(size = n_size))
map_sim_res_50_sig <- 
  map_sim_res_50 %>% map(~summary(.x)$coefficients[, 4] < .05) %>% 
  map_dfr(~ .x %>% as_tibble(), .id = "name") %>% as.data.frame()
map_sim_res_50_sig$name <- c(names(summary(wi_m1)$coefficients[, 4]))

map_sim_res_50_sig  %>% group_by(name) %>% summarise(mean_sig = mean(value))
