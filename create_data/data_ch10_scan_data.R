#-----------------------------------------------------------------
# scan_data  (used in Chapter 10)
#
# Builds three evenings of per-minute gate-scan counts and stacks them,
# producing `FOSBAAS::scan_data`. The scan-curve simulation now lives in the
# package, so this script just calls f_get_scan_data() and attaches a time of
# day and a date to each session.
#
# Output schema: observations, scans, action_time, date.
#-----------------------------------------------------------------

library(FOSBAAS)
library(dplyr)

# Each session is 300 minutes (5:00 pm - 9:59 pm).
session_times <- f_get_time_observations(17, 21)

scans_a <- f_get_scan_data(x_value = 230, y_value = 110, seed = 714, sd_mod = 14)
scans_a$action_time <- session_times
scans_a$date        <- "4/1/2024"

scans_b <- f_get_scan_data(x_value = 230, y_value = 123, seed = 755, sd_mod = 17)
scans_b$action_time <- session_times
scans_b$date        <- "4/2/2024"

scans_c <- f_get_scan_data(x_value = 230, y_value = 133, seed = 660, sd_mod = 19)
scans_c$action_time <- session_times
scans_c$date        <- "4/3/2024"

scan_data <- dplyr::bind_rows(scans_a, scans_b, scans_c)

# readr::write_csv(scan_data, "data-raw/scan_data.csv")
