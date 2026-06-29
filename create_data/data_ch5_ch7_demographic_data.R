#-----------------------------------------------------------------
# demographic_data  (used in Chapters 5 and 7)
#
# Builds the customer demographic table shipped as `FOSBAAS::demographic_data`.
# All of the simulation logic now lives in the package, so this script simply
# calls the exported orchestrator with the seeds used for the book.
#
# Output schema (13 simulation columns): custID, nameF, nameL, nameFull, gender,
# age, latitude, longitude, distance, maritalStatus, ethnicity, children,
# hhIncome. The published data set adds a 14th column, `county`, derived from
# the latitude/longitude (reverse geocoding) during final assembly.
#-----------------------------------------------------------------

library(FOSBAAS)

demographic_data <- f_build_demographic_data(
  seed1      = 755,  # customer IDs and names
  seed2      = 714,  # ages
  mean_age_1 = 29,   # younger age group mean
  mean_age_2 = 52,   # older age group mean
  sd_age_1   = 4,
  sd_age_2   = 6,
  seed3      = 755,  # locations / distance
  seed4      = 316,  # marital status
  seed5      = 278,  # ethnicity
  seed6      = 110,  # children
  seed7      = 714   # household income
)

# `county` is appended from the coordinates during final assembly, e.g.:
# demographic_data$county <- f_lookup_county(demographic_data$latitude,
#                                            demographic_data$longitude)

# readr::write_csv(demographic_data, "data-raw/demographic_data.csv")
