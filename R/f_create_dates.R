#' @title f_create_dates
#' @description Build a series of dates
#' @source \url{GIT}
#' @export

f_create_dates <- function(seed,season_year){
  set.seed(seed)

  pattern   <- c(3, 6, 9)
  days_off  <- c(3, 7, 10)
  # Get a date
  dates     <- seq(as.Date(paste(season_year,"-03-27", sep = '')),
                   as.Date(paste(season_year,"-10-08", sep = '')),
                   by = "+1 day")

  season_dates     <- list()

  start_date  <- sample(dates[1:10], 1)
  start_index <- which(dates == start_date)
  x <- 1

  while(x <= 81){
    num_games_series <- sample(pattern, 1, prob = c(.30, .60, .10))
    day_off_select   <- sample(days_off, 1, prob = c(.05, .35, .60))
    date_index       <- day_off_select + which(dates == as.character(season_dates[(x-1)]))[1]

    if(x == 1){   series_dates                   <- dates[x:num_games_series]
    season_dates[x:(num_games_series)]         <- as.character(series_dates)

    }else{        series_dates                   <- dates[date_index:(date_index+num_games_series-1)]
    season_dates[x:(x +num_games_series - 1)]    <- as.character(series_dates)
    }
    x <- x + num_games_series
  }
  return(season_dates[1:81])
}
