#-----------------------------------------------------------------
# customer_renewals  (used in Chapter 7)
#
# Builds a multi-season panel of season-ticket accounts and their renewal
# outcomes, producing `FOSBAAS::customer_renewals`. The lead-scoring logic now
# lives in the package, so this script drops the old embedded copies and calls
# the exported functions; it keeps only the year-over-year assembly (new sales
# plus renewed accounts, with tenure incremented each season).
#
# Output schema: accountID, corporate, season, planType, ticketUsage, tenure,
# spend, tickets, distance, renewed.
#-----------------------------------------------------------------

library(FOSBAAS)
library(dplyr)

# Convenience wrapper so each call uses the package helper functions.
build_year <- function(seed, n, season, renew) {
  f_create_lead_scoring_data(
    seed = seed, num_purchasers = n, season = season,
    f_tenure = f_calculate_tenure, f_spend = f_calculate_spend,
    f_use = f_calculate_ticket_use,
    f_renewal_assignment = f_renewal_assignment,
    f_assign_renewal = f_assign_renewal, renew = renew)
}

# 2021: initial book of business, with renewals simulated.
sth_2021 <- build_year(714, 5000, "2021", renew = TRUE)

# 2022: new accounts plus the 2021 accounts that renewed.
sth_2022_new     <- build_year(755, 450, "2022", renew = FALSE)
sth_2021_renewed <- sth_2021 %>% filter(renewed == "r") %>% select(-renewed)
sth_2022         <- bind_rows(sth_2022_new, sth_2021_renewed)
sth_2022$tenure  <- sth_2022$tenure + 1
sth_2022_renewals_m <- f_renewal_assignment(660, sth_2022, f_assign_renewal)

# 2023: new accounts plus the 2022 accounts that renewed.
sth_2023_new      <- build_year(868, 600, "2023", renew = FALSE)
sth_2022_renewals <- sth_2022_renewals_m %>% filter(renewed == "r") %>% select(-renewed)
sth_2023          <- bind_rows(sth_2023_new, sth_2022_renewals)
sth_2023_renewals_m <- f_renewal_assignment(660, sth_2023, f_assign_renewal)

# 2024: new accounts plus the 2023 accounts that renewed.
sth_2024_new      <- build_year(660, 375, "2024", renew = FALSE)
sth_2023_renewals <- sth_2023_renewals_m %>% filter(renewed == "r") %>% select(-renewed)
sth_2024          <- bind_rows(sth_2024_new, sth_2023_renewals)

# Modeling table: every account-season with a renewal outcome.
customer_renewals <- bind_rows(sth_2021, sth_2022_renewals_m, sth_2023_renewals_m)

# write.csv(customer_renewals, "data-raw/customer_renewals.csv", row.names = FALSE)
