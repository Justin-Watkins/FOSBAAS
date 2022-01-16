#' @title f_create_lead_scoring_data
#' @description Build a dataset that resembles season ticket holder renewal data
#' @param seed PARAM_DESCRIPTION
#' @param num_purchasers How many purchasers do you want
#' @param season What year is the season you are describing
#' @param f_tenure function see ?f_calculate_tenure
#' @param f_spend function see ?f_calculate_spend
#' @param f_use function see ?f_calculate_ticket_use
#' @param f_renewal_assignment function see ?f_assign_renewal
#' @param f_assign_renewal function see ?f_assign_renewal
#' @param renew Do you want simulated renewals, Default: T
#' @return A dataframe with 10 columns corresponding to features associated with season ticket holders
#' @source \url{GIT}
#' @export
f_create_lead_scoring_data <- function(seed,num_purchasers,season,f_tenure,f_spend,f_use,f_renewal_assignment,f_assign_renewal,renew = T){

  require(dplyr)

  #--------------------------------------------------------
  # Create a data frame to hold our data
  #--------------------------------------------------------
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

  #------------------------------------------------------------------------------
  # corporate
  #------------------------------------------------------------------------------
  set.seed(seed)
  corporate <- c("c", "i")
  sth_data$corporate <-
    sapply(seq(nrow(sth_data)), function(x)
      sample(corporate, 1, replace = TRUE, prob = c(.20, .80)))
  # Corporate accounts 95% full, Individuals 40% partial
  #------------------------------------------------------------------------------
  # Plan type
  #------------------------------------------------------------------------------
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
  #------------------------------------------------------------------------------
  # DISTANCE
  #------------------------------------------------------------------------------
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
  #------------------------------------------------------------------------------
  # TICKETS: Corporate > Individual | Full > Partial
  #------------------------------------------------------------------------------
  tickets <- c(10,8,6,5,4,3,2,1)
  set.seed(seed)

  sth_data[which(sth_data$corporate == "c"),]$tickets <-
    sapply(seq(nrow(sth_data[which(sth_data$corporate == "c"),])), function(x)
      sample(tickets, 1, replace = TRUE, prob = c(.02,.08,.10,.05,.50,.05,.20,0)))
  # Individuals
  sth_data[which(sth_data$corporate == "i"),]$tickets <-
    sapply(seq(nrow(sth_data[which(sth_data$corporate == "i"),])), function(x)
      sample(tickets, 1, replace = TRUE, prob = c(0,0,.10,.05,.40,.05,.30,.10)))
  #------------------------------------------------------------------------------
  # TENURE
  #------------------------------------------------------------------------------
  if(renew == T){
    avgDist <- mean(sth_data$distance)
    set.seed(seed)
    tenures <- with(sth_data,mapply(f_tenure,corporate,planType,distance,avgDist))
    sth_data$tenure <- as.vector(tenures)
  }else{sth_data$tenure = 0}
  #------------------------------------------------------------------------------
  # SPEND
  #------------------------------------------------------------------------------
  avgTenure <- mean(sth_data$tenure)
  set.seed(seed)
  spend <- with(sth_data,mapply(f_spend,corporate,planType,tenure,avgTenure))
  sth_data$spend <- as.vector(spend) * sth_data$tickets
  #------------------------------------------------------------------------------
  # USAGE: ticketUsage: Corporate > Individual | Full > Partial | Close > far
  #------------------------------------------------------------------------------
  avgDist <- mean(sth_data$distance)
  set.seed(seed)
  ticket_use <- with(sth_data,mapply(f_use,corporate,distance,avgDist))
  sth_data$ticketUsage <- as.vector(ticket_use)
  #------------------------------------------------------------------------------
  # Return proper data frame
  #------------------------------------------------------------------------------

  if(renew == T){
    sth_data_renew <-  f_renewal_assignment(seed,sth_data,f_assign_renewal)
    return(sth_data_renew)
  }else{ return(sth_data)}

} # End Function
