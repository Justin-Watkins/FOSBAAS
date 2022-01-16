#' @title f_build_demographic_data
#' @description combine demographic functions and build demographic data set
#' @source \url{GIT}
#' @export



f_build_demographic_data <- function(seed1,seed2,mean_age_1,mean_age_2,
                                     sd_age_1,sd_age_2,seed3,seed4,seed5,
                                     seed6,seed7){

  require(FOSBAAS)

  customer_data <- f_build_customer_ids_names(seed1)

  customer_data$gender <- sapply(customer_data$nameF,
                                 function(x) f_determine_gender(x))

  customer_data$age <- unlist(f_get_age(seed     = seed2,
                                        num_rows = 200000,
                                        g1_mean  = mean_age_1,
                                        g2_mean  = mean_age_2,
                                        g1_sd    = sd_age_1,
                                        g2_sd    = sd_age_2))

  distance_data <- f_get_distance(seed3,200000,f_get_miles_NV)

  customer_data$latitude  <- unlist(distance_data[1])
  customer_data$longitude <- unlist(distance_data[2])
  customer_data$distance  <- unlist(distance_data[3])

  customer_data$maritalStatus <- with(customer_data,mapply(f_demographics_married,
                                                           seed4,
                                                           gender,
                                                           age))

  customer_data$ethnicity <- with(customer_data,mapply(f_demographics_ethnicity,
                                                       seed5,
                                                       distance,
                                                       age))

  customer_data$children <- with(customer_data,mapply(f_demographics_children,
                                                      seed6,
                                                      maritalStatus,
                                                      age))

  age_quants         <- quantile(customer_data$age,probs = c(.30,.6))
  set.seed(seed7)
  customer_data$hhIncome <- mapply(f_demographics_income,
                                   customer_data$maritalStatus,
                                   customer_data$gender,
                                   customer_data$age,
                                   age_quants[[1]],
                                   age_quants[[2]])

  return(customer_data)

}
