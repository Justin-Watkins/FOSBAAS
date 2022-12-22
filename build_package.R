
#install.packages("devtools")
#install.packages("roxygen2")

library(devtools)
library(roxygen2)

setwd('..')
getwd()

devtools::document()

# aggregated_crm_data <- readr::read_csv("data-raw/aggregated_crm_data.csv")
# scan_data <- readr::read_csv("data-raw/scan_data.csv")
# wait_times_data <- readr::read_csv("data-raw/wait_times_data.csv")
# wait_times_distribution_data <- readr::read_csv("data-raw/wait_times_distribution_data.csv")
# freq_table_data <- readr::read_csv("data-raw/freq_table_data.csv")
# customer_data <- readr::read_csv("data-raw/customer_data.csv")
 demographic_data <- readr::read_csv("data-raw/demographic_data.csv")
# manifest_data <- readr::read_csv("data-raw/manifest_data.csv")
# perceptual_data <- readr::read_csv("data-raw/perceptual_data.csv")
# season_data <- readr::read_csv("data-raw/season_data.csv")
# secondary_data <- readr::read_csv("data-raw/secondary_sales_data.csv")
# customer_renewals <-  readr::read_csv("data-raw/customer_renewals.csv")
# aggregated_crm_data <- readr::read_csv("data-raw/aggregated_crm_data.csv")
# fa_survey_data <- readr::read_csv("data-raw/fa_survey_data.csv")

# usethis::use_data(demographic_data,overwrite = T)
# usethis::use_data(customer_data,
#                   demographic_data,
#                   manifest_data,
#                   perceptual_data,
#                   season_data,
#                   customer_renewals,
#                   secondary_data,
#                   wait_times_distribution_data,
#                   freq_table_data,
#                   fa_survey_data,
#                   aggregated_crm_data,
#                   overwrite = T)


# data documentation

# sinew::makeOxygen(  aggregated_crm_data)
# sinew::makeOxygen(  scan_data)
# sinew::makeOxygen(  wait_times_data)
# sinew::makeOxygen(  wait_times_distribution_data)
# sinew::makeOxygen(  customer_data)
# sinew::makeOxygen(  demographic_data)
# sinew::makeOxygen(  manifest_data)
# sinew::makeOxygen(  perceptual_data)
# sinew::makeOxygen(  season_data)
# sinew::makeOxygen(  secondary_data)
# sinew::makeOxygen(  customer_renewals)
# sinew::makeOxygen(  aggregated_crm_data)
# sinew::makeOxygen(  fa_survey_data )


# Run after all documentation is complete
devtools::document()

# install package
devtools::install()
# devtools::load_all()
# devtools::build() # Creates Tarball


#======================================================================
# Test:
#======================================================================

#remove.packages("FOSBAAS")

library(FOSBAAS)

?FOSBAAS::demographic_data
?FOSBAAS::f_add_zero



# install from git

devtools::install_github("Justin-Watkins/FOSBAAS",force = T)






