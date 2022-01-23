#-----------------------------------------------------------------
# manifet_data data set
#
# This data set approximates a disaggregated seat manifest for a ballpark
#
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
