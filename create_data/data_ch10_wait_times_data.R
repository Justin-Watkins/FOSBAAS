#-----------------------------------------------------------------
# wait_times_data  (used in Chapter 10)
#
# Builds three sessions of concession-stand wait times and stacks them,
# producing `FOSBAAS::wait_times_data`. The wait-time simulation now lives in
# the package, so this script just calls f_get_wait_times() three times.
#
# Output schema: transaction, orderTimes, paymentTimes, fulfillTimes, totalTime.
#-----------------------------------------------------------------

library(FOSBAAS)
library(dplyr)

wait_times_a <- f_get_wait_times(seed = 755, n = 300, rate1 = .03,
                                 rate2 = .06, rate3 = .15)
wait_times_b <- f_get_wait_times(seed = 714, n = 300, rate1 = .02,
                                 rate2 = .05, rate3 = .13)
wait_times_c <- f_get_wait_times(seed = 60,  n = 300, rate1 = .02,
                                 rate2 = .05, rate3 = .13)

wait_times_data <- dplyr::bind_rows(wait_times_a, wait_times_b, wait_times_c)

# readr::write_csv(wait_times_data, "data-raw/wait_times_data.csv")
