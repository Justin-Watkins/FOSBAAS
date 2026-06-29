#' Build a synthetic customer demographic data set
#'
#' Assembles a full demographic table for a synthetic customer base by combining
#' the demographic-simulation helpers: it generates IDs and names, infers
#' gender, draws a bimodal age distribution, places customers geographically,
#' and then simulates marital status, ethnicity, children and household income.
#' The result mirrors the simulated columns of the bundled [demographic_data].
#'
#' @param seed1 Seed for IDs and names ([f_build_customer_ids_names()]).
#' @param seed2 Seed for ages ([f_get_age()]).
#' @param mean_age_1,mean_age_2 Means of the two age groups.
#' @param sd_age_1,sd_age_2 Standard deviations of the two age groups.
#' @param seed3 Seed for locations ([f_get_distance()]).
#' @param seed4 Seed for marital status.
#' @param seed5 Seed for ethnicity.
#' @param seed6 Seed for children.
#' @param seed7 Seed for household income.
#'
#' @return A data frame of 200,000 customers with the columns `custID`, `nameF`,
#'   `nameL`, `nameFull`, `gender`, `age`, `latitude`, `longitude`, `distance`,
#'   `maritalStatus`, `ethnicity`, `children` and `hhIncome`.
#'
#' @details
#' Each random column is seeded once (here in the orchestrator) before being
#' filled row-wise, which is why the per-row demographic helpers no longer take
#' a `seed` argument. The bundled [demographic_data] adds a `county` column
#' derived from the coordinates during data-set assembly; this function returns
#' the 13 simulation columns.
#'
#' @examples
#' \donttest{
#' demo <- f_build_demographic_data(seed1 = 100, seed2 = 200,
#'           mean_age_1 = 30, mean_age_2 = 60, sd_age_1 = 10, sd_age_2 = 10,
#'           seed3 = 300, seed4 = 400, seed5 = 500, seed6 = 600, seed7 = 700)
#' head(demo)
#' }
#'
#' @family demographic_simulation
#' @importFrom stats quantile
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_build_demographic_data.R>
#' @export
f_build_demographic_data <- function(seed1, seed2, mean_age_1, mean_age_2,
                                     sd_age_1, sd_age_2, seed3, seed4, seed5,
                                     seed6, seed7) {

  customer_data <- f_build_customer_ids_names(seed1)
  n <- nrow(customer_data)

  customer_data$gender <- vapply(customer_data$nameF, f_determine_gender,
                                 character(1), USE.NAMES = FALSE)

  customer_data$age <- f_get_age(seed = seed2, num_rows = n,
                                 g1_mean = mean_age_1, g2_mean = mean_age_2,
                                 g1_sd = sd_age_1, g2_sd = sd_age_2)

  location <- f_get_distance(seed3, n)
  customer_data$latitude  <- location[[1]]
  customer_data$longitude <- location[[2]]
  customer_data$distance  <- location[[3]]

  set.seed(seed4)
  customer_data$maritalStatus <- with(customer_data,
                                       mapply(f_demographics_married, gender, age))

  set.seed(seed5)
  customer_data$ethnicity <- with(customer_data,
                                  mapply(f_demographics_ethnicity, distance, age))

  set.seed(seed6)
  customer_data$children <- with(customer_data,
                                 mapply(f_demographics_children, maritalStatus, age))

  age_quants <- quantile(customer_data$age, probs = c(.30, .60))
  set.seed(seed7)
  customer_data$hhIncome <- mapply(f_demographics_income,
                                   customer_data$maritalStatus,
                                   customer_data$gender,
                                   customer_data$age,
                                   age_quants[[1]],
                                   age_quants[[2]])

  customer_data
}
