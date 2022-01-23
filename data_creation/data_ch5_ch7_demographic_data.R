#-----------------------------------------------------------------
# customer_renewals data set
#
# This data set approximates a processed demographic data set
# It requires that customer data is already created
# library(dplyr)
# library(lubridate)
# library(readr)
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# BEGIN Demographic data set
#-----------------------------------------------------------------

library(dplyr)
library(lubridate)
library(readr)

f_get_miles_NV <- function(newLat,newLon){
  # Convert to Radians
  lat2 <- newLat * (pi/180)
  lon2 <- newLon * (pi/180)
  # Rough radius of the earth at our latitude
  R = 3958
  # convert starting points to radians
  startLat <- 36.1613  * (pi/180)
  startLon <- -86.7786 * (pi/180)
  # Get the distance between points in radians
  dLon = lat2 - startLat
  dLat = lon2 - startLon
  # Apply Haversine formula
  a <- (sin(dLat/2))^2 + cos(startLat) * (sin(dLon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R*c
  return(round(d,2))
}

#demographic_data <- data.frame(matrix(nrow = nrow(customer_data), ncol = 7))
#names(demographic_data) <- c("custID", "ethnicity", "age",
#                             "married", "children", "hhIncome",
#                            "distance")
#-----------------------------------------------------------------
# Age

f_get_age <- function(seed,num_rows = 200000, g1_mean,g2_mean,g1_sd,g2_sd){

  #set.seed(755)
  #n       <-  200000
  group_1 <-  rnorm(num_rows, g1_mean, g1_sd)
  group_2 <-  rnorm(num_rows, g2_mean, g2_sd)
  age <- list()
  age <- sample(round(c(sample(group_1,num_rows *.4),
                        sample(group_2,num_rows*.6)),0),
                num_rows,replace = FALSE)
  return(age)
}

customer_data$age <- unlist(f_get_age(seed     = 714,
                                      num_rows = 200000,
                                      g1_mean  = 29,
                                      g2_mean  = 52,
                                      g1_sd    = 4,
                                      g2_sd    = 6))
#-----------------------------------------------------------------
# Distance from venue

f_get_distance <- function(seed,n = 200000,f_get_miles_NV){

  # set.seed(305)
  # n     <- 200000
  perc  <- c(.55,.05,.05,.05,.1,.2)
  sd    <- c(.5,1,1,1,1.2,.5)
  draws <- n * perc
  lats  <- c(36.1612,33.7490,34.7304,35.5951,35.0456,35.9606 )
  lngs  <- c(-86.7785,-84.3880,-86.5861,-82.5515,-85.3097,-83.9207 )

  latitude  <- list()
  longitude <- list()

  x <- 1
  while(x <= length(perc)){

    latitude[[x]]  <- rnorm(draws[x], lats[x], sd[x])
    longitude[[x]] <- rnorm(draws[x], lngs[x], sd[x])

    x <- x + 1
  }

  distances <- list()

  distances[[1]] <- unlist(latitude)
  distances[[2]] <- unlist(longitude)
  distances[[3]] <- unlist(mapply(f_get_miles_NV,latitude,longitude))

  return(distances)

}

distance_data <- f_get_distance(755,200000,f_get_miles_NV)

customer_data$latitude  <- unlist(distance_data[1])
customer_data$longitude <- unlist(distance_data[2])
customer_data$distance  <- unlist(distance_data[3])
#-----------------------------------------------------------------
# Begin to bias data

f_demographics_married <- function(seed,gender,age){

  set.seed(seed)
  married <- c("m", "s")

  if(gender == 'm' & age <= 40){status <-sample(married, 1, replace = TRUE, prob = c(.35, .65))}
  else if(gender == 'm' & age > 40){status <-sample(married, 1, replace = TRUE, prob = c(.70, .30))}
  else if(gender == 'f' & age <= 40){status <-sample(married, 1, replace = TRUE, prob = c(.45, .55))}
  else if(gender == 'f' & age > 40){status <-sample(married, 1, replace = TRUE, prob = c(.85, .15))}
  else{status <- sample(married, 1, replace = TRUE, prob = c(.50, .50))}

  return(status)
}
customer_data$maritalStatus <- with(customer_data,mapply(f_demographics_married,
                                                         316,
                                                         gender,
                                                         age))
#-----------------------------------------------------------------
# Ethnicity

f_demographics_ethnicity <- function(seed,distance,age){

  ethnicity <- c("w", "aa", "h", "a")

  if(distance <= 35 & age <= 40){eth <-sample(ethnicity, 1, replace = TRUE, prob = c(.87, .02, .10, .01))}
  else if(distance > 35 & age > 40){eth <-sample(ethnicity, 1, replace = TRUE, prob = c(.80, .12, .08, .02))}
  else if(distance <= 35 & age <= 40){eth <-sample(ethnicity, 1, replace = TRUE, prob = c(.70, .15, .12, .03))}
  else if(distance > 35 & age > 40){eth <-sample(ethnicity, 1, replace = TRUE, prob = c(.95, .11, .06, .03))}
  else{eth <-sample(ethnicity, 1, replace = TRUE, prob = c(.80, .11, .06, .03))}

  return(eth)

}

customer_data$ethnicity <- with(customer_data,mapply(f_demographics_ethnicity,
                                                     278,
                                                     distance,
                                                     age))
#-----------------------------------------------------------------
# Children

f_demographics_children <- function(seed,married,age){

  children <- c("y", "n")

  if(married == 'm' & age <= 40){child <-sample(children, 1, replace = TRUE, prob = c(.60, .40))}
  else if(married == 's' & age < 40){child <-sample(children, 1, replace = TRUE, prob = c(.15, .85))}
  else if(married == 'm' & age > 40){child <-sample(children, 1, replace = TRUE, prob = c(.70, .30))}
  else if(married =='s' & age > 40){child <-sample(children, 1, replace = TRUE, prob = c(.10, .90))}
  else{child <-sample(children, 1, replace = TRUE, prob = c(.50, .50))}

  return(child)

}

customer_data$children <- with(customer_data,mapply(f_demographics_children,
                                                    110,
                                                    maritalStatus,
                                                    age))
#-----------------------------------------------------------------
# Household Income
# married, age, and gender

age_quants            <- quantile(customer_data$age,probs = c(.30,.6))
f_demographics_income <- function(married,gender,age,age_low,age_high){

  means <- c(3000, 2500, 2000, 1500, 1000, 500)

  if(married == 'm' & gender == 'm' & age >= age_high){income <- sample(means, 1, prob = c(.5,.2,.1,.1,.1,.00))
  }else if(married == 'm' & gender == 'f' & age >= age_high){income <- sample(means, 1, prob = c(.5,.2,.1,.1,.05,.05))
  }else if(married == 'm' & gender == 'm' & age >= age_low & age < age_high){income <- sample(means, 1, prob = c(.5,.2,.1,.1,.1,.00))
  }else if(married == 'm' & gender == 'f' & age >= age_low & age < age_high){income <- sample(means, 1, prob = c(.5,.2,.1,.1,.05,.05))
  }else if(married == 'm' & gender == 'm' & age < age_low){income <- sample(means, 1, prob = c(.2,.2,.2,.2,.1,.1))
  }else if(married == 'm' & gender == 'f' & age < age_low){income <- sample(means, 1, prob = c(.1,.1,.2,.2,.2,.2))
  }else if(married == 's' & gender == 'm' & age >= age_high){income <- sample(means, 1, prob = c(.3,.2,.3,.1,.05,.05))
  }else if(married == 's' & gender == 'f' & age >= age_high){income <- sample(means, 1, prob = c(.1,.1,.2,.3,.2,.1))
  }else if(married == 's' & gender == 'm' & age >= age_low & age < age_high){income <- sample(means, 1, prob = c(.2,.2,.2,.2,.1,.1))
  }else if(married == 's' & gender == 'f' & age >= age_low & age < age_high){income <- sample(means, 1, prob = c(.1,.1,.2,.2,.2,.2))
  }else if(married == 's' & gender == 'm' & age < age_low){income <- sample(means, 1, prob = c(.1,.1,.1,.2,.2,.3))
  }else if(married == 's' & gender == 'f' & age < age_low){income <- sample(means, 1, prob = c(.0,.1,.1,.2,.3,.3))
  }else{income <- sample(means, 1, prob = c(.2,.2,.2,.2,.1,.1))}

  income <- rnorm(1, income, income/10)
  return(round(abs(income),0))

}

age_quants         <- quantile(customer_data$age,probs = c(.30,.6))
set.seed(714)
customer_data$hhIncome <- mapply(f_demographics_income,
                                 customer_data$maritalStatus,
                                 customer_data$gender,
                                 customer_data$age,
                                 age_quants[[1]],
                                 age_quants[[2]])

#-----------------------------------------------------------------

f_build_demographic_data <- function(seed1,seed2,mean_age_1,mean_age_2,
                                     sd_age_1,sd_age_2,seed3,seed4,seed5,
                                     seed6,seed7){

  require(FOSBAAS)

  customer_data        <- f_build_customer_ids_names(seed1)

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


require(FOSBAAS)

customer_data <- f_build_demographic_data(seed1      = 755,
                                          seed2      = 714,
                                          mean_age_1 = 29,
                                          mean_age_2 = 52,
                                          sd_age_1   = 4,
                                          sd_age_2   = 6,
                                          seed3      = 755,
                                          seed4      = 316,
                                          seed5      = 278,
                                          seed6      = 110,
                                          seed7      = 714)

#readr::write_csv(customer_data,'demographic_data.csv')

#-----------------------------------------------------------------
# End Demographic data set
#-----------------------------------------------------------------
