#-----------------------------------------------------------------
# season-data data set
#
# This data set approximates a processed season data set for modeling
#
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
