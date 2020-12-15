library(progress)
source('simulations.R')

# Power simulation for the number of days in followup
day_range <- seq(5, 20, 1)
pb <- progress_bar$new(total = length(day_range))
day_power <- map_dfr(day_range, function(d) {
  pb$tick()
  df <- suppressMessages(rerun(500, sim_analysis(10, 10, d)) %>% bind_rows())
  success <- df$p_value < 0.05
  list(days = d, mean = mean(success), sd = sd(success))
})
write_csv(day_power, "cached/day_power.csv")

# Power simulation for the number of participants (even groups)
participant_range <- seq(10, 100, 5)
pb <- progress_bar$new(total = length(participant_range))
participant_power <- map_dfr(participant_range, function(n) {
  pb$tick()
  df <- suppressMessages(rerun(1000, sim_analysis(n, n, 5)) %>% bind_rows())
  success <- df$p_value < 0.05
  list(participants = n, mean = mean(success), sd = sd(success))
})
write_csv(participant_power, "cached/participant_power.csv")

# How little can the placebo group be?
uneven_range <- seq(10, 70, 5)
pb <- progress_bar$new(total = length(uneven_range))
uneven_power <- map_dfr(uneven_range, function(n) {
  pb$tick()
  df <- suppressMessages(rerun(1000, sim_analysis(n, 70, 5)) %>% bind_rows())
  success <- df$p_value < 0.05
  list(placebo = n, mean = mean(success), sd = sd(success))
})
write_csv(uneven_power, "cached/uneven_power.csv")
