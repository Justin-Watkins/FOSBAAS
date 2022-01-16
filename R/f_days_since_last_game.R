#' @title f_days_since_last_game
#' @description Calculate the number of days since a game was played
#' @source \url{GIT}
#' @export

f_days_since_last_game <- function(dates){

  dates <- as.Date(dates)
  x <- 1
  date_values <- list()
  while(x <= length(dates)){

    if(x == 1){date_values[x] <- 50}
    else if(x == length(dates)){date_values[x] <- dates[x] - dates[x-1]}
    else{date_values[x] <-  dates[x] - dates[x - 1]}
    x <- x + 1

  }
  return(date_values)

}
