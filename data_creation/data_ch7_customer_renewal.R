#-----------------------------------------------------------------
# customer_renewals data set
#
# This data set approximates a processed renewal data set
# require(dplyr)
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# Begin Customer Renewals
#-----------------------------------------------------------------

f_calculate_tenure <- function(corporate,planType,distance,avgDist){
  if(corporate == "c" & planType == "f" & distance <= avgDist){ten <-round(abs(rnorm(1,mean = 14,sd = 6)),0)}
  else if(corporate == "i" & planType == "f" & distance <= avgDist){ten <-round(abs(rnorm(1,mean = 10,sd = 6)),0)}
  else if(corporate == "c" & planType == "p" & distance <= avgDist){ten <-round(abs(rnorm(1,mean = 3,sd = 2)),0)}
  else if(corporate == "i" & planType == "p" & distance <= avgDist){ten <-round(abs(rnorm(1,mean = 3,sd = 2)),0)}
  else if(corporate == "c" & planType == "f" & distance >= avgDist){ten <-round(abs(rnorm(1,mean = 9,sd = 3)),0)}
  else if(corporate == "i" & planType == "f" & distance >= avgDist){ten <-round(abs(rnorm(1,mean = 7,sd = 3)),0)}
  else if(corporate == "c" & planType == "p" & distance >= avgDist){ten <-round(abs(rnorm(1,mean = 2,sd = 1)),0)}
  else if(corporate == "i" & planType == "p" & distance >= avgDist){ten <-round(abs(rnorm(1,mean = 2,sd = 1)),0)}
  else{ten <-round(abs(rnorm(1,mean = 8,sd = 3)),0)}
  return(ten)
}

f_calculate_spend <- function(corporate,planType,tenure,avgTenure){
  if(corporate == "c" & planType == "f" & tenure >= avgTenure){spend <-round(abs(rnorm(1,mean = 7500,sd = 800)),0)}
  else if(corporate == "i" & planType == "f" & tenure >= avgTenure){spend <-round(abs(rnorm(1,mean = 2100,sd = 500)),0)}
  else if(corporate == "c" & planType == "p" & tenure >= avgTenure){spend <-round(abs(rnorm(1,mean = 2000,sd = 300)),0)}
  else if(corporate == "i" & planType == "p" & tenure >= avgTenure){spend <-round(abs(rnorm(1,mean = 1200,sd = 200)),0)}
  else if(corporate == "c" & planType == "f" & tenure <= avgTenure){spend <-round(abs(rnorm(1,mean = 5000,sd = 500)),0)}
  else if(corporate == "i" & planType == "f" & tenure <= avgTenure){spend <-round(abs(rnorm(1,mean = 2000,sd = 300)),0)}
  else if(corporate == "c" & planType == "p" & tenure <= avgTenure){spend <-round(abs(rnorm(1,mean = 2000,sd = 400)),0)}
  else if(corporate == "i" & planType == "p" & tenure <= avgTenure){spend <-round(abs(rnorm(1,mean = 800,sd = 75)),0)}
  else{spend <-round(abs(rnorm(1,mean = 2500,sd = 300)),0)}
  return(spend)
}

f_calculate_ticket_use <- function(corporate,distance,avgDist){
  if(corporate == "c" & distance <= avgDist){tu <- runif(1,min = .89, max = 1)}
  else if(corporate == "i" & distance <= avgDist){tu <- runif(1,min = .82, max = .94)}
  else if(corporate == "c" & distance >= avgDist){tu <- runif(1,min = .65, max = .9)}
  else if(corporate == "i" & distance >= avgDist){tu <- runif(1,min = .55, max = .85)}
  else{tu <- runif(1,min = .65, max = .95)}
  return(tu)
}

f_assign_renewal <- function(x,renew){

  if(x == 10){sample(renew,1,prob = c(.99,.01))}
  else if(x == 9){sample(renew,1,prob = c(.98,.02))}
  else if(x == 8){sample(renew,1,prob = c(.95,.05))}
  else if(x == 7){sample(renew,1,prob = c(.95,.05))}
  else if(x == 6){sample(renew,1,prob = c(.92,.08))}
  else if(x == 5){sample(renew,1,prob = c(.90,.10))}
  else if(x == 4){sample(renew,1,prob = c(.85,.15))}
  else if(x == 3){sample(renew,1,prob = c(.80,.20))}
  else if(x == 2){sample(renew,1,prob = c(.30,.70))}
  else if(x == 1){sample(renew,1,prob = c(.25,.75))}
  else{sample(renew,1,prob = c(.5,.5))}
}

f_renewal_assignment <- function(seed,sth_data,f_assign_renewal){

  require(dplyr)

  ids <- as.data.frame(sth_data$accountID)
  names(ids) <- "accountID"

  set.seed(seed)
  centers1 <- kmeans(sth_data$ticketUsage, centers = 5)$centers
  centers1 <- sort(centers1)
  ids$clusterTU <- kmeans(sth_data$ticketUsage, centers = centers1)$cluster

  set.seed(seed)
  centers2 <- kmeans(sth_data$distance, centers = 5)$centers
  centers2 <- rev(sort(centers2))
  ids$clusterDI <- kmeans(sth_data$distance, centers = centers2)$cluster

  ids$clustSum <- ids$clusterTU + ids$clusterDI
  sth_data_renew <- dplyr::left_join(ids,sth_data, by = "accountID")

  x <- 1
  renew <- c("r","nr")
  a_renew <- list()
  while(x <= nrow(sth_data_renew)){
    clust <- sth_data_renew[x,3]
    a_renew[x] <- f_assign_renewal(clust,renew)
    x <- x + 1
  }

  sth_data_renew$renewed <- unlist(a_renew)
  sth_data_renew <- dplyr::select(sth_data_renew,accountID,corporate,season,planType,
                                  ticketUsage,tenure,spend,tickets,distance,
                                  renewed)
  return(sth_data_renew)

} # End

f_create_lead_scoring_data <- function(seed,num_purchasers,season,f_tenure,f_spend,f_use,f_renewal_assignment,f_assign_renewal,renew = T){
  require(dplyr)
  #-----------------------------------------------------------------
  # Create a data frame to hold our data

  # Build a data frame to hold the customer data
  sth_data <- data.frame(matrix(nrow=num_purchasers,ncol=9))
  names(sth_data) <- c("accountID","corporate","season", "planType",
                       "ticketUsage","tenure","spend","tickets",
                       "distance")
  # Build ids and append to customer data frame
  set.seed(seed)
  sth_data[,1] <- sapply(seq(nrow(sth_data)), function(x)
    paste(sample(c(0:9, LETTERS), 12, replace=TRUE),
          collapse = ""))

  # add data with some caveats: corporations spend more
  sth_data$season <- season

  #-----------------------------------------------------------------
  # corporate
  set.seed(seed)
  corporate <- c("c", "i")
  sth_data$corporate <-
    sapply(seq(nrow(sth_data)), function(x)
      sample(corporate, 1, replace = TRUE, prob = c(.20, .80)))
  # Corporate accounts 95% full, Individuals 40% partial
  #-----------------------------------------------------------------
  # Plan type
  set.seed(seed)
  planType <- c("f","p")
  sth_data[which(sth_data$corporate == "c"),]$planType <-
    sapply(seq(nrow(sth_data[which(sth_data$corporate == "c"),])), function(x)
      sample(planType, 1, replace = TRUE, prob = c(.95, .5)))
  # Individuals
  planType <- c("f","p")
  sth_data[which(sth_data$corporate == "i"),]$planType <-
    sapply(seq(nrow(sth_data[which(sth_data$corporate == "i"),])), function(x)
      sample(planType, 1, replace = TRUE, prob = c(.60, .40)))
  #-----------------------------------------------------------------
  # DISTANCE
  set.seed(seed)
  distances_corp <- rexp(num_purchasers) * 12
  distances_indv <- rexp(num_purchasers) * 30

  # Corporate
  set.seed(seed)
  sth_data[which(sth_data$corporate == "c"),]$distance <-
    sapply(seq(nrow(sth_data[which(sth_data$corporate == "c"),])), function(x)
      sample(distances_corp, 1, replace = TRUE))
  # Individuals
  sth_data[which(sth_data$corporate == "i"),]$distance <-
    sapply(seq(nrow(sth_data[which(sth_data$corporate == "i"),])), function(x)
      sample(distances_indv, 1, replace = TRUE))
  #-----------------------------------------------------------------
  # TICKETS: Corporate > Individual | Full > Partial

  tickets <- c(10,8,6,5,4,3,2,1)
  set.seed(seed)

  sth_data[which(sth_data$corporate == "c"),]$tickets <-
    sapply(seq(nrow(sth_data[which(sth_data$corporate == "c"),])), function(x)
      sample(tickets, 1, replace = TRUE, prob = c(.02,.08,.10,.05,.50,.05,.20,0)))
  # Individuals
  sth_data[which(sth_data$corporate == "i"),]$tickets <-
    sapply(seq(nrow(sth_data[which(sth_data$corporate == "i"),])), function(x)
      sample(tickets, 1, replace = TRUE, prob = c(0,0,.10,.05,.40,.05,.30,.10)))
  #-----------------------------------------------------------------
  # TENURE

  if(renew == T){
    avgDist <- mean(sth_data$distance)
    set.seed(seed)
    tenures <- with(sth_data,mapply(f_tenure,corporate,planType,distance,avgDist))
    sth_data$tenure <- as.vector(tenures)
  }else{sth_data$tenure = 0}
  #-----------------------------------------------------------------
  # SPEND
  avgTenure <- mean(sth_data$tenure)
  set.seed(seed)
  spend <- with(sth_data,mapply(f_spend,corporate,planType,tenure,avgTenure))
  sth_data$spend <- as.vector(spend) * sth_data$tickets
  #-----------------------------------------------------------------
  # USAGE: ticketUsage: Corporate > Individual | Full > Partial | Close > far
  avgDist <- mean(sth_data$distance)
  set.seed(seed)
  ticket_use <- with(sth_data,mapply(f_use,corporate,distance,avgDist))
  sth_data$ticketUsage <- as.vector(ticket_use)
  #-----------------------------------------------------------------
  # Return proper data frame
  if(renew == T){
    sth_data_renew <-  f_renewal_assignment(seed,sth_data,f_assign_renewal)
    return(sth_data_renew)
  }else{ return(sth_data)}

} # End Function

require(FOSBAAS)
# Get a list of sths in 2021
sth_2021 <- f_create_lead_scoring_data(714,
                                       5000,
                                       "2021",
                                       f_calculate_tenure,
                                       f_calculate_spend,
                                       f_calculate_ticket_use,
                                       f_renewal_assignment,
                                       f_assign_renewal,
                                       renew = T)
# Create new sths for 2022
sth_2022_new <- f_create_lead_scoring_data(755,
                                           450,
                                           "2022",
                                           f_calculate_tenure,
                                           f_calculate_spend,
                                           f_calculate_ticket_use,
                                           f_renewal_assignment,
                                           f_assign_renewal,
                                           renew = F)

# Renewals in 2022
sth_2021_renewed <- sth_2021 %>% filter(renewed == 'r') %>% select(-renewed)
# Combine new and renewed accounts
sth_2022 <- bind_rows(sth_2022_new,sth_2021_renewed)

# Get new sales for 2023
sth_2023_new <- f_create_lead_scoring_data(868,
                                           600,
                                           "2023",
                                           f_calculate_tenure,
                                           f_calculate_spend,
                                           f_calculate_ticket_use,
                                           f_renewal_assignment,
                                           f_assign_renewal,
                                           renew = F)


sth_2022$tenure <- sth_2022$tenure + 1
# Assign renewal acconts for 2022
sth_2022_renewals_m <- f_renewal_assignment(660,sth_2022,f_assign_renewal)

sth_2022_renewals <- sth_2022_renewals_m %>% filter(renewed == 'r') %>% select(-renewed)

sth_2023 <- bind_rows(sth_2023_new,sth_2022_renewals)

sth_2024_new <- f_create_lead_scoring_data(660,
                                           375,
                                           "2024",
                                           f_calculate_tenure,
                                           f_calculate_spend,
                                           f_calculate_ticket_use,
                                           f_renewal_assignment,
                                           f_assign_renewal,
                                           renew = F)

# Renewed for 2024
sth_2023_renewals_m <- f_renewal_assignment(660,sth_2023,f_assign_renewal)

sth_2023_renewals <- sth_2023_renewals_m %>% filter(renewed == 'r') %>% select(-renewed)

sth_2024 <- bind_rows(sth_2024_new,sth_2023_renewals)

#-----------------------------------------------------------------
# Create table for modeling
mod_data <- bind_rows(sth_2021,sth_2022_renewals_m,sth_2023_renewals_m)
#write.csv(mod_data,"customer_renewals.csv",row.names = FALSE)
