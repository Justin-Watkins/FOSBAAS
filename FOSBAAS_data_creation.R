#-----------------------------------------------------------------
# This file creates the data for the FOSBAAS book
# Justin Watkins
# Copyright 2022
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

#-----------------------------------------------------------------
# End Customer Renewals
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Begin aggregated CRM data
#-----------------------------------------------------------------

ag_sales_data <- as.data.frame(matrix(nrow = 5000,ncol = 3))
names(ag_sales_data) <- c('repID','call','revenue')

set.seed(755)

ag_sales_data$repID <-  rep(sapply(seq(10), function(x) paste(sample(c(0:9, LETTERS),
                                                                     12,
                                                                     replace=TRUE),
                                                                     collapse = "")),
                                                                     500)

calls <- sample(1:5,10000,prob = c(.20,.25,.30,.2,.05),replace = TRUE)

revenue <- c(rnorm(25000,3000,800),rnorm(500,10000,1000),
             rnorm(2000,500,60),rnorm(500,50000,8000),
             rep(0,30000))

ag_sales_data$revenue <- sample(revenue,nrow(ag_sales_data))
ag_sales_data$call    <- sample(calls,nrow(ag_sales_data))

#readr::write_csv(ag_sales_data,"aggregated_crm_data.csv")

#-----------------------------------------------------------------
# END aggregated CRM data
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Begin perceptual data
#-----------------------------------------------------------------
# Create our data set
perceptual_data <- as.data.frame(matrix(nrow=3,ncol=10))
names(perceptual_data)      <- c('Friendly','Exciting','Fresh','Inovative','Fun',
                                 'Old','Historic','Winners','Great','Expensive')
row.names(perceptual_data)  <- c('Chicken Hearts','Grizzlies','Predators')

set.seed(2632)
perceptual_data <- apply(perceptual_data,1:2,function(x) round(rnorm(1,3000,1000),0))

#write.csv(perceptual_data,"perceptual_data.csv",row.names=FALSE)

#-----------------------------------------------------------------
# END perceptual data
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# BEGIN factor analysis survey data
#-----------------------------------------------------------------

reasons <-
  c('Company outing',
    'Family outing, no children',
    'Family outing, with children',
    'I was given tickets',
    'No distinct reason',
    'Support the Team',
    'Special Game',
    'Other Promotion',
    'See a concert',
    'Celebrate an anniversary',
    'Support the opposing team',
    'Celebrate a birthday',
    'See my friends',
    'Visit the park')

survey_data_experiential <- data.frame(matrix(nrow = 10000,ncol = 25))

set.seed(755)
reas        <- as.data.frame(sample(reasons,10000,replace = T))
names(reas) <- 'ReasonForAttending'

names(survey_data_experiential) <-
  c('Socialize','Tailgate','TakeSelfies','PostToSocialMedia','SeeFriends',
    'VisitKidAttractions','MeetMascot','SnacksForKids','KidsRunBases','KidsSlide',
    'GetDinner', 'EatParkFood','SampleFood','GetDrinks','DrinkBeer',
    'BuyGear','TourThePark','VisitAttractions','WatchPregameShow','SeeTheChicken',
    'UpgradeSeats','GetAutographs','WatchGame','SeePractice','MeetPlayers')

set.seed(755)
survey_data_experiential[] <- apply(survey_data_experiential,2,function(x) round(runif(10000,0,10),0))

reasons <- cbind.data.frame(reas,survey_data_experiential)
#-----------------------------------------------------------------
# add noise
set.seed(1456)
sampattr <- sample(1:1000,1000,replace = F)
reasons[sampattr,17:21] <-
  apply(reasons[sampattr,17:21],1:2, function(x) ifelse(runif(1,0,1) > .5,x*.25,x))

set.seed(3000)
sampgame <- sample(1:1000,1000,replace = F)
reasons[sampgame,22:26] <-
  apply(reasons[sampgame,22:26],1:2, function(x) ifelse(runif(1,0,1) > .1,x*.20,x))

set.seed(755)
sampSocial <- sample(1:2000,2000,replace = F)
reasons[sampSocial,2:6] <-
  apply(reasons[sampSocial,2:6],1:2, function(x) ifelse(runif(1,0,1) > .2,x*.4,x))

set.seed(114)
sampkids <- sample(1:2000,2000,replace = F)
reasons[sampkids,7:11] <-
  apply(reasons[sampkids,7:11],1:2, function(x) ifelse(runif(1,0,1) > .4,x*.3,x))

set.seed(999)
sampfood <- sample(1:2500,2500,replace = F)
reasons[sampfood,12:16] <-
  apply(reasons[sampfood,12:16],1:2, function(x) ifelse(runif(1,0,1) > .2,x*.5,x))

reasons[,2:26] <- apply(reasons[,2:26],2,function(x) ifelse(x > 10,10,x))
reasons[,2:26] <- apply(reasons[,2:26],2,function(x) round(x,0))

FA   <- reasons[,c(1:26)]
Type <- FA[,1]
FA[,2:26] <- apply(FA[,2:26],2,function(x) as.numeric(x))

#write.csv(FA,'FA_survey_data.csv',row.names = F)

#-----------------------------------------------------------------
# END factor analysis survey data
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# BEGIN Manifest data
#-----------------------------------------------------------------

section_prices <- as.data.frame(matrix(nrow = 25, ncol = 7))
names(section_prices) <- c("section", "sectionName", "level", "seats",
                           "seasonPrice", "groupPrice", "singlePrice")
# Populate manifest summary
section_prices$section <- seq(1:25)
set.seed(106)
prices        <- as.data.frame(rnorm(5000, 40, 40))
names(prices) <- "ticketPrice"

samp_prices    <- prices %>% filter(ticketPrice > 10)  %>%
  mutate(roundPrices = round(ticketPrice))             %>%
  arrange(desc(ticketPrice))                           %>%
  distinct(roundPrices)
#-----------------------------------------------------------------
# Start with highest prices and get every n price
x        <- 1
y        <- 1
sections <- 25
while(x <= sections){
  section_prices[x,5] <- samp_prices[y,1]
  y <- y + ceiling(nrow(samp_prices)/sections)
  x <- x + 1
}
#-----------------------------------------------------------------
# Allocate seats based on exponential values
demand <- as.data.frame(rexp(45000, rate = 1))
names(demand) <- "expValues"

hist_values          <- hist(demand$expValues, breaks = 50)$counts
section_prices$seats <- hist_values[25:1]

section_prices$groupPrice  <- section_prices$seasonPrice * 1.05
section_prices$singlePrice <- section_prices$seasonPrice * 1.15

section_prices[23:25,]$level <- 300
section_prices[19:22,]$level <- 200
section_prices[1:18,]$level  <- 100

section_names <- c('Herty',  'Brown', 'Winston', 'Warner',
                   'MCarthy', 'Saussy', 'Jones', 'Reynolds',
                   'Dickinson', 'Barnard', 'Whitney',
                   'Bocock', 'Coulter', 'Dobson', 'Cunningham',
                   'Stegeman', 'Woodruff', 'Mehre',
                   'Hunt', 'Butts', 'Griffith', 'Dooley',
                   'Goff', 'Richt', 'Smart')
section_prices$sectionName <- section_names
#-----------------------------------------------------------------
# Constant model
#modExp <- nls(seats ~ a*seasonPrice^m, data = section_prices,
#              start = list(a = 8000,m=-.33))

a <- 136786.214
m <- -1.079

section_prices$modeledExpSeats <- sapply(section_prices$seasonPrice,
                                         function(x) a* x^m)
#-----------------------------------------------------------------
# Logistic Model

mod_log <- glm(seats ~ seasonPrice, data = section_prices, family = gaussian(link = "log"))

section_prices$modeledLogSeats <- predict(mod_log,section_prices,type = "response")

#-----------------------------------------------------------------
# Construct Manifest

manifest <- section_prices %>% select(section,sectionName,level,seats)
manifest_full <- as.data.frame(matrix(nrow=0,ncol = 7))
names(manifest_full) <- c('section','sectionName','level','seats',
                          'sectionNumber','rowNumber','seatNumber')

x <- 1
while(x <= nrow(manifest)){

  section <- manifest[x,]
  sectionRep <- section[rep(seq_len(nrow(section)),
                            each = section$seats), ]
#-----------------------------------------------------------------
# Create Sections
  if(section$seats/288 <= 1){numSections <- 1
  }else{numSections <- ceiling(section$seats/288)}

  sections <- rep(rep(1:numSections),(section$seats + 288)/numSections)
  sectionRep$sectionNumber <- sort(sections[1:section$seats])
#-----------------------------------------------------------------
# Create rows
  rows               <- as.vector(rep(rep(1:12),ceiling(section$seats/12)))
  sectionRep$rowNumber  <- rows[1:section$seats]
  sectionRep$seatNumber <- rows[1:section$seats]
  y <- 2
  while(y <= max(sectionRep$sectionNumber)){

    sectionRep[which(sectionRep$sectionNumber == y),]$rowNumber <-
      rows[1:nrow(sectionRep[which(sectionRep$sectionNumber == y),])]

    y <- y + 1
  }
  sectionRep <- sectionRep[order(sectionRep$sectionNumber,sectionRep$rowNum),]
#-----------------------------------------------------------------
# Create Seats
  y <- 1
  while(y <= max(sectionRep$sectionNumber)){

    z <- 1
    while(z <= max(sectionRep[which(sectionRep$sectionNumber == y),]$rowNumber)){

      sectionRep[which(sectionRep$sectionNumber == y & sectionRep$rowNumber == z ),]$seatNumber <-
        rep(1:nrow(sectionRep[which(sectionRep$sectionNumber == y & sectionRep$rowNumber == z ),]))

      z <- z + 1
    }
    y <- y + 1
  }
#-----------------------------------------------------------------
# bind to data frame
  manifest_full <- rbind(manifest_full,sectionRep)
  x <- x + 1
  print(x)

}

manifest_full_complete <- manifest_full %>% left_join(section_prices, by = 'section') %>%
  mutate(seatID = seq(1:nrow(manifest_full))) %>%
  select(seatID,section,sectionNumber,rowNumber,seatNumber,
         seasonPrice,groupPrice,singlePrice)

#readr::write_csv(manifest_full_complete,'manifest_data.csv')

#-----------------------------------------------------------------
# END Manifest data
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# BEGIN Secondary data
#-----------------------------------------------------------------




#-----------------------------------------------------------------
# END Secondary data
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# BEGIN Demographic data set
#-----------------------------------------------------------------

library(dplyr)
library(readr)
library(lubridate)


f_build_customer_ids_names <- function(seed,num_rows = 200000){
  set.seed(seed)
  # Build a data frame to hold the customer data
  customer_data <- data.frame(matrix(nrow=num_rows,ncol=4))
  # Build ids and append to customer data frame
  customer_data[,1] <- sapply(seq(nrow(customer_data)), function(x)
    paste(sample(c(0:9, LETTERS), 12, replace=TRUE),
          collapse = ""))

  m_names <- c("James","John","Robert","Michael","William","David","Richard","Joseph",
               "Thomas","Charles","Christopher","Daniel","Matthew","Anthony","Donald",
               "Mark","Paul","Steven","Andrew","Kenneth","Joshua","George","Kevin",
               "Brian","Edward","Ronald","Timothy","Jason","Jeffrey","Ryan","Jacob",
               "Gary","Nicholas","Eric","Stephen","Jonathan","Larry","Justin","Scott",
               "Brandon","Frank","Benjamin","Gregory","Samuel","Raymond","Patrick",
               "Alexander","Jack","Dennis","Jerry","Tyler","Aaron","Jose","Henry",
               "Douglas","Adam","Peter","Nathan","Zachary","Walter","Kyle","Harold",
               "Carl","Jeremy","Keith","Roger","Gerald","Ethan","Arthur","Terry",
               "Christian","Sean","Lawrence","Austin","Joe","Noah","Jesse","Albert",
               "Bryan","Billy","Bruce","Willie","Jordan","Dylan","Alan","Ralph",
               "Gabriel","Roy","Juan","Wayne","Eugene","Logan","Randy","Louis","Russell",
               "Vincent","Philip","Bobby","Johnny","Bradley")

  f_names <- c("Mary","Patricia","Jennifer","Linda","Elizabeth","Barbara","Susan","Jessica",
               "Sarah","Karen","Nancy","Margaret","Lisa","Betty","Dorothy","Sandra","Ashley",
               "Kimberly","Donna","Emily","Michelle","Carol","Amanda","Melissa","Deborah",
               "Stephanie","Rebecca","Laura","Sharon","Cynthia","Kathleen","Helen","Amy",
               "Shirley","Angela","Anna","Brenda","Pamela","Nicole","Ruth","Katherine",
               "Samantha","Christine","Emma","Catherine","Debra","Virginia","Rachel","Carolyn",
               "Janet","Maria","Heather","Diane","Julie","Joyce","Victoria","Kelly",
               "Christina","Joan","Evelyn","Lauren","Judith","Olivia","Frances","Martha",
               "Cheryl","Megan","Andrea","Hannah","Jacqueline","Ann","Jean","Alice",
               "Kathryn","Gloria","Teresa","Doris","Sara","Janice","Julia","Marie","Madison",
               "Grace","Judy","Theresa","Beverly","Denise","Marilyn","Amber","Danielle",
               "Abigail","Brittany","Rose","Diana","Natalie","Sophia","Alexis","Lori","Kayla",
               "Jane")
  a_names <- c(f_names,m_names)

  l_names <- c("Smith","Johnson","Williams","Brown","Jones","Miller","Davis","Garcia","Rodriguez",
               "Wilson","Martinez","Anderson","Taylor","Thomas","Hernandez","Moore","Martin","Jackson",
               "Thompson","White","Lopez","Lee","Gonzalez","Harris","Clark","Lewis","Robinson","Walker",
               "Perez","Hall","Young","Allen","Sanchez","Wright","King","Scott","Green","Baker","Adams",
               "Nelson","Hill","Ramirez","Campbell","Mitchell","Roberts","Carter","Phillips","Evans",
               "Turner","Torres","Parker","Collins","Edwards","Stewart","Flores","Morris","Nguyen",
               "Murphy","Rivera","Cook","Rogers","Morgan","Peterson","Cooper","Reed","Bailey","Bell",
               "Gomez","Kelly","Howard","Ward","Cox","Diaz","Richardson","Wood","Watson","Brooks",
               "Bennett","Gray","James","Reyes","Cruz","Hughes","Price","Myers","Long","Foster",
               "Sanders","Ross","Morales","Powell","Sullivan","Russell","Ortiz","Jenkins","Gutierrez",
               "Perry","Butler","Barnes","Fisher","Henderson","Coleman","Simmons","Patterson","Jordan",
               "Reynolds","Hamilton","Graham","Kim","Gonzales","Alexander","Ramos","Wallace","Griffin",
               "West","Cole","Hayes","Chavez","Gibson","Bryant","Ellis","Stevens","Murray","Ford",
               "Marshall","Owens","Mcdonald","Harrison","Ruiz","Kennedy","Wells","Alvarez","Woods",
               "Mendoza","Castillo","Olson","Webb","Washington","Tucker","Freeman","Burns","Henry",
               "Vasquez","Snyder","Simpson","Crawford","Jimenez","Porter","Mason","Shaw","Gordon",
               "Wagner","Hunter","Romero","Hicks","Dixon","Hunt","Palmer","Robertson","Black","Holmes",
               "Stone","Meyer","Boyd","Mills","Warren","Fox","Rose","Rice","Moreno","Schmidt","Patel",
               "Ferguson","Nichols","Herrera","Medina","Ryan","Fernandez","Weaver","Daniels","Stephens",
               "Gardner","Payne","Kelley","Dunn","Pierce","Arnold","Tran","Spencer","Peters","Hawkins",
               "Grant","Hansen","Castro","Hoffman","Hart","Elliott","Cunningham","Knight","Bradley",
               "Carroll","Hudson","Duncan","Armstrong","Berry","Andrews","Johnston","Ray","Lane",
               "Riley","Carpenter","Perkins","Aguilar","Silva","Richards","Willis","Matthews","Chapman",
               "Lawrence","Garza","Vargas","Watkins","Wheeler","Larson","Carlson","Harper","George",
               "Greene","Burke","Guzman","Morrison","Munoz","Jacobs","Obrien","Lawson","Franklin","Lynch",
               "Bishop","Carr","Salazar","Austin","Mendez","Gilbert","Jensen","Williamson","Montgomery",
               "Harvey","Oliver","Howell","Dean","Hanson","Weber","Garrett","Sims","Burton","Fuller",
               "Soto","Mccoy","Welch","Chen","Schultz","Walters","Reid","Fields","Walsh","Little",
               "Fowler","Bowman","Davidson","May","Day","Schneider","Newman","Brewer","Lucas","Holland",
               "Wong","Banks","Santos","Curtis","Pearson","Delgado","Valdez","Pena","Rios","Douglas",
               "Sandoval","Barrett","Hopkins","Keller","Guerrero","Stanley","Bates","Alvarado","Beck",
               "Ortega","Wade","Estrada","Contreras","Barnett","Caldwell","Santiago","Lambert","Powers",
               "Chambers","Nunez","Craig","Leonard","Lowe","Rhodes","Byrd","Gregory","Shelton","Frazier",
               "Becker","Maldonado","Fleming","Vega","Sutton","Cohen","Jennings","Parks","Mcdaniel",
               "Watts","Barker","Norris","Vaughn","Vazquez","Holt","Schwartz","Steele","Benson","Neal",
               "Dominguez","Horton","Terry","Wolfe","Hale","Lyons","Graves","Haynes","Miles","Park",
               "Warner","Padilla","Bush","Thornton","Mccarthy","Mann","Zimmerman","Erickson","Fletcher",
               "Mckinney","Page","Dawson","Joseph","Marquez","Reeves","Klein","Espinoza","Baldwin",
               "Moran","Love","Robbins","Higgins","Ball","Cortez","Le","Griffith","Bowen","Sharp",
               "Cummings","Ramsey","Hardy","Swanson","Barber","Acosta","Luna","Chandler","Blair","Daniel",
               "Cross","Simon","Dennis","Oconnor","Quinn","Gross","Navarro","Moss","Fitzgerald","Doyle",
               "Mclaughlin","Rojas","Rodgers","Stevenson","Singh","Yang","Figueroa","Harmon","Newton",
               "Paul","Manning","Garner","Mcgee","Reese","Francis","Burgess","Adkins","Goodman","Curry",
               "Brady","Christensen","Potter","Walton","Goodwin","Mullins","Molina","Webster","Fischer",
               "Campos","Avila","Sherman","Todd","Chang","Blake","Malone","Wolf","Hodges","Juarez","Gill",
               "Farmer","Hines","Gallagher","Duran","Hubbard","Cannon","Miranda","Wang","Saunders","Tate",
               "Mack","Hammond","Carrillo","Townsend","Wise","Ingram","Barton","Mejia","Ayala","Schroeder",
               "Hampton","Rowe","Parsons","Frank","Waters","Strickland","Osborne","Maxwell","Chan","Deleon",
               "Norman","Harrington","Casey","Patton","Logan","Bowers","Mueller","Glover","Floyd","Hartman",
               "Buchanan","Cobb","French","Kramer","Mccormick","Clarke","Tyler","Gibbs","Moody","Conner",
               "Sparks","Mcguire","Leon","Bauer","Norton","Pope","Flynn","Hogan","Robles","Salinas","Yates",
               "Lindsey","Lloyd","Marsh","Mcbride","Owen","Solis","Pham","Lang","Pratt","Lara","Brock",
               "Ballard","Trujillo","Shaffer","Drake","Roman","Aguirre","Morton","Stokes","Lamb","Pacheco",
               "Patrick","Cochran","Shepherd","Cain","Burnett","Hess","Li","Cervantes","Olsen","Briggs",
               "Ochoa","Cabrera","Velasquez","Montoya","Roth","Meyers","Cardenas","Fuentes","Weiss","Hoover",
               "Wilkins","Nicholson","Underwood","Short","Carson","Morrow","Colon","Holloway","Summers",
               "Bryan","Petersen","Mckenzie","Serrano","Wilcox","Carey","Clayton","Poole","Calderon",
               "Gallegos","Greer","Rivas","Guerra","Decker","Collier","Wall","Whitaker","Bass","Flowers",
               "Davenport","Conley","Houston","Huff","Copeland","Hood","Monroe","Massey","Roberson","Combs",
               "Franco","Larsen","Pittman","Randall","Skinner","Wilkinson","Kirby","Cameron","Bridges",
               "Anthony","Richard","Kirk","Bruce","Singleton","Mathis","Bradford","Boone","Abbott","Charles",
               "Allison","Sweeney","Atkinson","Horn","Jefferson","Rosales","York","Christian","Phelps",
               "Farrell","Castaneda","Nash","Dickerson","Bond","Wyatt","Foley","Chase","Gates","Vincent",
               "Mathews","Hodge","Garrison","Trevino","Villarreal","Heath","Dalton","Valencia","Callahan",
               "Hensley","Atkins","Huffman","Roy","Boyer","Shields","Lin","Hancock","Grimes","Glenn","Cline",
               "Delacruz","Camacho","Dillon","Parrish","Oneill","Melton","Booth","Kane","Berg","Harrell","Pitts",
               "Savage","Wiggins","Brennan","Salas","Marks","Russo","Sawyer","Baxter","Golden","Hutchinson",
               "Liu","Walter","Mcdowell","Wiley","Rich","Humphrey","Johns","Koch","Suarez","Hobbs","Beard",
               "Gilmore","Ibarra","Keith","Macias","Khan","Andrade","Ware","Stephenson","Henson","Wilkerson",
               "Dyer","Mcclure","Blackwell","Mercado","Tanner","Eaton","Clay","Barron","Beasley","Oneal",
               "Preston","Small","Wu","Zamora","Macdonald","Vance","Snow","Mcclain","Stafford","Orozco","Barry",
               "English","Shannon","Kline","Jacobson","Woodard","Huang","Kemp","Mosley","Prince","Merritt",
               "Hurst","Villanueva","Roach","Nolan","Lam","Yoder","Mccullough","Lester","Santana","Valenzuela",
               "Winters","Barrera","Leach","Orr","Berger","Mckee","Strong","Conway","Stein","Whitehead","Bullock",
               "Escobar","Knox","Meadows","Solomon","Velez","Odonnell","Kerr","Stout","Blankenship","Browning",
               "Kent","Lozano","Bartlett","Pruitt","Buck","Barr","Gaines","Durham","Gentry","Mcintyre","Sloan",
               "Melendez","Rocha","Herman","Sexton","Moon","Hendricks","Rangel","Stark","Lowery","Hardin","Hull",
               "Sellers","Ellison","Calhoun","Gillespie","Mora","Knapp","Mccall","Morse","Dorsey","Weeks",
               "Nielsen","Livingston","Leblanc","Mclean","Bradshaw","Glass","Middleton","Buckley","Schaefer",
               "Frost","Howe","House","Mcintosh","Ho","Pennington","Reilly","Hebert","Mcfarland","Hickman",
               "Noble","Spears","Conrad","Arias","Galvan","Velazquez","Huynh","Frederick","Randolph","Cantu",
               "Fitzpatrick","Mahoney","Peck","Villa","Michael","Donovan","Mcconnell","Walls","Boyle","Mayer",
               "Zuniga","Giles","Pineda","Pace","Hurley","Mays","Mcmillan","Crosby","Ayers","Case","Bentley",
               "Shepard","Everett","Pugh","David","Mcmahon","Dunlap","Bender","Hahn","Harding","Acevedo","Raymond",
               "Blackburn","Duffy","Landry","Dougherty","Bautista","Shah","Potts","Arroyo","Valentine","Meza","Gould",
               "Vaughan","Fry","Rush","Avery","Herring","Dodson","Clements","Sampson","Tapia","Bean","Lynn","Crane",
               "Farley","Cisneros","Benton","Ashley","Mckay","Finley","Best","Blevins","Friedman","Moses","Sosa",
               "Blanchard","Huber","Frye","Krueger","Bernard","Rosario","Rubio","Mullen","Benjamin","Haley","Chung",
               "Moyer","Choi","Horne","Yu","Woodward","Ali","Nixon","Hayden","Rivers","Estes","Mccarty","Richmond",
               "Stuart","Maynard","Brandt","Oconnell","Hanna","Sanford","Sheppard","Church","Burch","Levy","Rasmussen",
               "Coffey","Ponce","Faulkner","Donaldson","Schmitt","Novak","Costa","Montes","Booker","Cordova","Waller",
               "Arellano","Maddox","Mata","Bonilla","Stanton","Compton","Kaufman","Dudley","Mcpherson","Beltran",
               "Dickson","Mccann","Villegas","Proctor","Hester","Cantrell","Daugherty","Cherry","Bray","Davila",
               "Rowland","Levine","Madden","Spence","Good","Irwin","Werner","Krause","Petty","Whitney","Baird",
               "Hooper","Pollard","Zavala","Jarvis","Holden","Haas","Hendrix","Mcgrath","Bird","Lucero","Terrell",
               "Riggs","Joyce","Mercer","Rollins","Galloway","Duke","Odom","Andersen","Downs","Hatfield","Benitez",
               "Archer","Huerta","Travis","Mcneil","Hinton","Zhang","Hays","Mayo","Fritz","Branch","Mooney","Ewing",
               "Ritter","Esparza","Frey","Braun","Gay","Riddle","Haney","Kaiser","Holder","Chaney","Mcknight","Gamble",
               "Vang","Cooley","Carney","Cowan","Forbes","Ferrell","Davies","Barajas","Shea","Osborn","Bright",
               "Cuevas","Bolton","Murillo","Lutz","Duarte","Kidd","Key","Cooke")


  full_names   <- vector()
  first_names  <- vector()
  last_names   <- vector()

  # Combine names
  k <- 1
  for(i in a_names){
    for(j in l_names){
      #    full_names[k]  <- paste(i,j)
      first_names[k] <- i
      last_names[k]  <- j
      k <- 1 + k
    }
  }
  # Randomly apply a name to each row of the customer file
  set.seed(755)
  customer_data[,2] <- sapply(seq(nrow(customer_data)), function(x)
    sample(first_names,1, replace=TRUE))
  customer_data[,3] <- sapply(seq(nrow(customer_data)), function(x)
    sample(last_names,1, replace=TRUE))

  customer_data[,4] <- paste(customer_data[,2],customer_data[,3],sep=' ')

  # Rename columns
  names(customer_data) <- c("custID",'nameF','nameL',"nameFull")
  return(customer_data)

}

customer_data <- f_build_customer_ids_names(755)

#-----------------------------------------------------------------
# Determine Gender

f_determine_gender <- function(f_name){

  f_names <- c("Mary","Patricia","Jennifer","Linda","Elizabeth","Barbara","Susan","Jessica",
               "Sarah","Karen","Nancy","Margaret","Lisa","Betty","Dorothy","Sandra","Ashley",
               "Kimberly","Donna","Emily","Michelle","Carol","Amanda","Melissa","Deborah",
               "Stephanie","Rebecca","Laura","Sharon","Cynthia","Kathleen","Helen","Amy",
               "Shirley","Angela","Anna","Brenda","Pamela","Nicole","Ruth","Katherine",
               "Samantha","Christine","Emma","Catherine","Debra","Virginia","Rachel","Carolyn",
               "Janet","Maria","Heather","Diane","Julie","Joyce","Victoria","Kelly",
               "Christina","Joan","Evelyn","Lauren","Judith","Olivia","Frances","Martha",
               "Cheryl","Megan","Andrea","Hannah","Jacqueline","Ann","Jean","Alice",
               "Kathryn","Gloria","Teresa","Doris","Sara","Janice","Julia","Marie","Madison",
               "Grace","Judy","Theresa","Beverly","Denise","Marilyn","Amber","Danielle",
               "Abigail","Brittany","Rose","Diana","Natalie","Sophia","Alexis","Lori","Kayla",
               "Jane")


  if (f_name %in% f_names == TRUE){gender <- 'f'}
  else{gender <- 'm'}

  return(gender)

}

customer_data$gender <- sapply(customer_data$nameF,function(x) f_determine_gender(x))

#readr::write_csv(customer_data, "customer_data.csv")

#-----------------------------------------------------------------
# Demographics


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

#-----------------------------------------------------------------
# Begin Season data
#-----------------------------------------------------------------

f_simulate_sales <- function(t, dow, m, sio, dslg, od, bh){

  teamMod      <-
    if(t %in% c("BOS", "CHC", "NYY", "LAD", "STL")){round(rnorm(1, 42000, 3000), 0)}
      else if(t %in% c("LAA", "HOU", "NYM", "SF", "PHI")){round(rnorm(1, 30000, 3000), 0)}
        else if(t %in% c("BAl", "KAN", "SD", "TEX")){round(rnorm(1, 20000, 500), 0)}
          else{round(rnorm(1, 25000, 3000), 0)}
  dayMod       <-
    if(dow %in% c("Sat", "Fri")){round(rnorm(1, 60000, 2000), 0)}
      else{round(rnorm(1, 12000, 1000), 0)}
  monthMod     <-
    if(m %in% c("June", "July")){round(rnorm(1, 60000, 2000), 0)}
      else if(m %in% c("Apr", "Sep", "May")){round(rnorm(1, 20000, 3000), 0)}
        else{round(rnorm(1, 24000, 3000), 0)}
  schoolMod     <-
    if(sio == FALSE){round(rnorm(1, 40000, 2000), 0)}
      else{round(rnorm(1, 25000, 5000), 0)}
  lastGameMmod  <-
    if(dslg > 5){round(rnorm(1, 38000, 5000), 0)}
      else{round(rnorm(1, 29000, 5000), 0)}
  openingDayMod <-
    if(od == TRUE){round(rnorm(1, 65000, 2000), 0)}
      else{round(rnorm(1, 32000, 5000), 0)}
  bobbleHeadMod <-
    if(bh == TRUE){round(rnorm(1, 45000, 2000), 0)}
      else{round(rnorm(1, 32000, 3000), 0)}
  ticketSales   <- mean(teamMod, dayMod, monthMod, schoolMod,lastGameMod, openingDayMod, bobbleHeadMod)
  ticketSales   <- ifelse(ticketSales > 44500, 44500, ticketSales)

  return(ticketSales)
}

f_build_season <- function(seed1, seed2, seed3, seasonYear, salesFunction,modifier = 1, dayMod = 1, monthMod  = 1){

  seasonData <- data.frame(matrix(nrow = 81, ncol = 12))
  names(seasonData) <- c("gameNumber","team","date","dayOfWeek","month","weekEnd","schoolInOut","daysSinceLastGame","openingDay","bobbleHead","ticketSales","season")
# Simulate days in home stand
  set.seed(seed1)
  pattern  <- c(3, 6, 9)
  daysOff  <- c(3, 7, 10)
# Get a date
  dates <- seq(as.Date(paste(seasonYear,"-03-27",sep='')),
               as.Date(paste(seasonYear,"-10-08",sep='')),
               by="+1 day")
  seasonDates <- as.data.frame(matrix(nrow = 81, ncol = 1))
    colnames(seasonDates) <- "date"
  startDate  <- sample(dates[1:10], 1)

  x <- 1
  while(x <= 81){
    numGamesSeries <- sample(pattern, 1, prob = c(.25, .7, .05))
    dayOffSelect   <- sample(daysOff, 1, prob = c(.05, .45, .50))
    dateIndex      <- dayOffSelect + as.numeric(which(dates == seasonDates[x-1,]))

    if(x == 1){   seriesDates                   <- dates[x:numGamesSeries]
    seasonDates[x:(numGamesSeries), 1]          <- as.character(seriesDates)
    }else{        seriesDates                   <- dates[dateIndex:(numGamesSeries - 1 + dateIndex)]
    seasonDates[x:(numGamesSeries - 1 + x), 1]  <- as.character(seriesDates)
    }
    x <- 81 - (sum(is.na(seasonDates)) - 1)
  }
  seasonData$date <- seasonDates[1:81,]
  seasonData$date <- as.Date(seasonData$date)

# Create list of teams and apply to schedule
  mlbTeams <-  c("ARI", "ATL", "BAL", "BOS", "CHC", "CWS", "CIN",
                 "CLE", "COL", "DET", "FLA", "HOU", "KAN", "LAA",
                 "LAD", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI",
                 "PIT", "SD", "SF", "SEA", "STL", "TB", "TEX",
                 "TOR", "WAS")
  set.seed(seed2)
# Assign random team every three games
  x <- 1
  y <- 1
  while(x <= nrow(seasonData)){
    seriesOpponent <- sample(mlbTeams, 1, replace = TRUE)
    y <- 1
    while(y <= 3){
      seasonData[x,2] <- seriesOpponent
      x <- x + 1
      y <- y + 1
    }
  }
  # Get game number
  seasonData$gameNumber  <- seq(1,81)
  # Get day of week
  seasonData$dayOfWeek   <- lubridate::wday(seasonData$date,label=TRUE)
  # Get month
  seasonData$month       <- lubridate::month(seasonData$date,label=TRUE)
  # Weekend
  seasonData$weekEnd     <- ifelse(seasonData$dayOfWeek %in% c('Fri','Sat'), TRUE, FALSE)
  # School in/out
  seasonData$schoolInOut <- ifelse(seasonData$date >= as.Date(paste(seasonYear, '-05-25',sep='')) &
                                     seasonData$date <= as.Date(paste(seasonYear, '-08-05',sep='')),
                                   FALSE, TRUE)
  # Opening day
  seasonData$openingDay  <- ifelse(seasonData$gameNumber == 1,TRUE,FALSE)
  # bobble head
  seasonData$bobbleHead  <- ifelse(seasonData$date %in% sample(seasonData$date,5),TRUE,FALSE)
  # Days since last game
  x <- 1
  dateValues <- list()
  while(x <= nrow(seasonData)){
    dateValues[x] <- seasonData$date[x + 1] - seasonData$date[x]
    x <- x + 1
  }
  seasonData$daysSinceLastGame[2:81] <- as.vector(unlist(dateValues[1:80]))
  seasonData$daysSinceLastGame[1] <- 0
  # Simulate Sales
  set.seed(seed3)
  seasonData$ticketSales <- with(seasonData,mapply(f_simulate_sales,team,dayOfWeek,month,schoolInOut,daysSinceLastGame,openingDay,bobbleHead))
  seasonData$ticketSales <- seasonData$ticketSales * modifier
  seasonData[which(seasonData$month %in% c("Jun", "Jul")), ]$ticketSales     <- seasonData[which(seasonData$month %in% c("Jun", "Jul")), ]$ticketSales * monthMod
  seasonData[which(seasonData$dayOfWeek %in% c("Sat", "Sun")), ]$ticketSales <- seasonData[which(seasonData$dayOfWeek %in% c("Sat", "Sun")), ]$ticketSales * dayMod
  seasonData$ticketSales <- ifelse(seasonData$ticketSales > 44500, 44500, seasonData$ticketSales)
  #season name
  seasonData$season <- seasonYear
  return(seasonData)
}


seed1      <- 3371
seed2      <- 2726
seed3      <- 2726
modifier   <- 1.05
dayMod     <- 1.10
monthMod   <- 1.15
seasonYear <- '2022'
season22   <- f_build_season(seed1, seed2, seed3, seasonYear,f_simulate_sales, modifier, dayMod, monthMod)

seed1      <- 309
seed2      <- 755
seed3      <- 512
modifier   <- 1.00
dayMod     <- 1.10
monthMod   <- 1.15
seasonYear <- '2023'
season23   <- f_build_season(seed1, seed2, seed3, seasonYear,f_simulate_sales, modifier, dayMod, monthMod)

seed1      <- 2607
seed2      <- 2
seed3      <- 3084
modifier   <- .90
dayMod     <- 1.10
monthMod   <- 1.15
seasonYear <- '2024'
season24   <- f_build_season(seed1, seed2, seed3, seasonYear,f_simulate_sales, modifier, dayMod, monthMod)

season_data             <- rbind(season22, season23, season24)
season_data$gameID      <- seq(1:nrow(season_data))
season_data$gameKey     <- paste(substring(season_data$date, 1, 4),sep = "_", season_data$gameNumber)
season_data$ticketSales <- ceiling(season_data$ticketSales)

#readr::write_csv(season_data,'season_data.csv')
#-----------------------------------------------------------------
# End Season data set
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Begin Manifest data set
#-----------------------------------------------------------------

section_prices <- as.data.frame(matrix(nrow = 25, ncol = 7))
names(section_prices) <- c("section", "sectionName", "level", "seats","seasonPrice", "groupPrice", "singlePrice")
#------------------------------------------------------------------------------
# Populate manifest summary
section_prices$section <- seq(1:25)
set.seed(106)
prices        <- as.data.frame(rnorm(5000, 40, 40))
names(prices) <- "ticketPrice"

samp_prices    <- prices %>% filter(ticketPrice > 10)  %>%
  mutate(roundPrices = round(ticketPrice))             %>%
  arrange(desc(ticketPrice))                           %>%
  distinct(roundPrices)
#------------------------------------------------------------------------------
# Start with highest prices and get every n price
x        <- 1
y        <- 1
sections <- 25
while(x <= sections){
  section_prices[x,5] <- samp_prices[y,1]
  y <- y + ceiling(nrow(samp_prices)/sections)
  x <- x + 1
}
#------------------------------------------------------------------------------
# Allocate seats based on exponential values
demand <- as.data.frame(rexp(45000, rate = 1))
names(demand) <- "expValues"

hist_values          <- hist(demand$expValues, breaks = 50)$counts
section_prices$seats <- hist_values[25:1]

section_prices$groupPrice  <- section_prices$seasonPrice * 1.05
section_prices$singlePrice <- section_prices$seasonPrice * 1.15

section_prices[23:25,]$level <- 300
section_prices[19:22,]$level <- 200
section_prices[1:18,]$level  <- 100

section_names <- c('Herty',  'Brown', 'Winston', 'Warner',
                   'MCarthy', 'Saussy', 'Jones', 'Reynolds',
                   'Dickinson', 'Barnard', 'Whitney',
                   'Bocock', 'Coulter', 'Dobson', 'Cunningham',
                   'Stegeman', 'Woodruff', 'Mehre',
                   'Hunt', 'Butts', 'Griffith', 'Dooley',
                   'Goff', 'Richt', 'Smart')
section_prices$sectionName <- section_names
#------------------------------------------------------------------------------
# Constant model
modExp <- nls(seats ~ a*seasonPrice^m, data = section_prices,
              start = list(a = 8000,m=-.33))


a <- 136786.214
m <- -1.079

section_prices$modeledExpSeats <- sapply(section_prices$seasonPrice,function(x) a* x^m)
#------------------------------------------------------------------------------
# Logistic Model
mod_log <- glm(seats ~ seasonPrice, data = section_prices, family = gaussian(link = "log"))

section_prices$modeledLogSeats <- predict(mod_log,section_prices,type = "response")

#------------------------------------------------------------------------------
# Construct Manifest

manifest             <- section_prices %>% select(section,sectionName,level,seats)
manifest_full        <- as.data.frame(matrix(nrow=0,ncol = 7))
names(manifest_full) <- c('section','sectionName','level','seats','sectionNumber','rowNumber','seatNumber')

x <- 1
while(x <= nrow(manifest)){

  section <- manifest[x,]
  sectionRep <- section[rep(seq_len(nrow(section)),
                            each = section$seats), ]
#------------------------------------------------------------------------------
# Create Sections
  if(section$seats/288 <= 1){numSections <- 1
  }else{numSections <- ceiling(section$seats/288)}

  sections <- rep(rep(1:numSections),(section$seats + 288)/numSections)
  sectionRep$sectionNumber <- sort(sections[1:section$seats])
#------------------------------------------------------------------------------
# Create rows
  rows               <- as.vector(rep(rep(1:12),ceiling(section$seats/12)))
  sectionRep$rowNumber  <- rows[1:section$seats]
  sectionRep$seatNumber <- rows[1:section$seats]
  y <- 2
  while(y <= max(sectionRep$sectionNumber)){

    sectionRep[which(sectionRep$sectionNumber == y),]$rowNumber <-
      rows[1:nrow(sectionRep[which(sectionRep$sectionNumber == y),])]

    y <- y + 1
  }
  sectionRep <- sectionRep[order(sectionRep$sectionNumber,sectionRep$rowNum),]
#------------------------------------------------------------------------------
# Create Seats
  y <- 1
  while(y <= max(sectionRep$sectionNumber)){
    z <- 1
    while(z <= max(sectionRep[which(sectionRep$sectionNumber == y),]$rowNumber)){
      sectionRep[which(sectionRep$sectionNumber == y & sectionRep$rowNumber == z ),]$seatNumber <-
        rep(1:nrow(sectionRep[which(sectionRep$sectionNumber == y & sectionRep$rowNumber == z ),]))
      z <- z + 1
    }
    y <- y + 1
  }
#------------------------------------------------------------------------------
# bind to data frame
  manifest_full <- rbind(manifest_full,sectionRep)
  x <- x + 1

}

manifest_full_complete <- manifest_full %>% left_join(section_prices, by = 'section') %>%
  mutate(seatID = seq(1:nrow(manifest_full))) %>%
  select(seatID,section,sectionNumber,rowNumber,seatNumber,
         seasonPrice,groupPrice,singlePrice)

#readr::write_csv(manifest_full_complete,'manifest_data.csv')

#-----------------------------------------------------------------
# End create manifest data
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# Create Resale data
#-----------------------------------------------------------------

data$season_data$gameCluster <- kmeans(scale(data$season_data$ticketSales),centers = 10)$cluster


clusMatrix <- as.data.frame(as.table(by(data$season_data$ticketSales,
                                        data$season_data$gameCluster,
                                        function(x) mean(x))))

clusMatrixSort <- clusMatrix[order(clusMatrix[,2]),]
clusMatrixSort$orderedCluster <- seq(1:nrow(clusMatrixSort))
names(clusMatrixSort) <- c('gameCluster','avg','orderedCluster')
clusMatrixSort$gameCluster <- as.numeric(as.character(clusMatrixSort$gameCluster))


clusMatrixSort   <- dplyr::select(clusMatrixSort,gameCluster,orderedCluster)
data$season_data <- data$season_data %>% left_join(clusMatrixSort,by = 'gameCluster')

# Create data frame to aid in building pricing coefficient
# The coefficinet should work on a distribution related to how
# attractive the game is based on attendance (it really works in the converse)


# Sample 12% of tickets to be sold on the secondary market.


set.seed(6)
data$secondary_sales <- dplyr::sample_frac(data$ticket_sales_2022_2023_2024,.12)

# Join Game cluster
clusterKeys <- dplyr::select(data$season_data,gameID,orderedCluster)

data$secondary_sales <- data$secondary_sales %>% dplyr::left_join(clusterKeys,by='gameID')


coefficientMeans <- seq(from = .6, to = 1.5, by = .1)
sd <- .2

set.seed(2)
data$secondary_sales$secondayrPrice <- with(data$secondary_sales,
                                            mapply(function(od,pr) round(rnorm(1,coefficientMeans[od],sd) * pr,2)
                                                   ,orderedCluster,price))

readr::write_csv(data$secondary_sales,"secondary_sales_data.csv")










