# get

power_boot_map <- function(n_boot, model) {
  boot.index <- sample(unique(dat_maps$control.id), replace = TRUE, n_boot = n_boot)
  dat_maps_boot <-
    do.call(rbind, lapply(boot.index, function(x)
      dat_maps[dat_maps$control.id == x, ]))
  mod_FE_m1 <-
    net_sessions ~ factor(ifExp) + crash_rate_m1 + jank_rate   + drain_rate
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

