#' @title f_build_season
#' @description Build season data
#' @source \url{GIT}
#' @export

f_build_season <- function(seed1,season_year,seed2,num_games = 81,
                           seed3,num_bbh,num_con,num_oth,seed4,
                           seed5,mean_sales, sd_sales){

  require(FOSBAAS)

  season      <- f_create_season_frame()
  season$date <- unlist(f_create_dates(seed1,season_year))
  season$team <- unlist(f_select_team_for_series(seed2,num_games))

  fields      <- f_apply_static_fields(season$date)

  season$gameNumber  <- unlist(fields[1])
  season$dayOfWeek   <- unlist(fields[2])
  season$month       <- unlist(fields[3])
  season$weekEnd     <- unlist(fields[4])
  season$schoolInOut <- unlist(fields[5])
  season$openingDay  <- unlist(fields[6])
  season$season      <- unlist(fields[7])

  season$promotion <- unlist(f_assign_promotion(num_bbh,num_con,num_oth,num_games,seed3))

  season$daysSinceLastGame <- unlist(f_days_since_last_game(season$date))


  coefficient_list <- with(season, mapply(f_attend_coefficient,team,weekEnd,schoolInOut,
                                          daysSinceLastGame,openingDay,
                                          promotion,seed4))

  set.seed(seed5)
  season$ticketSales <- round(rnorm(nrow(season),mean = mean_sales, sd = sd_sales),0)

  season$ticketSales <- round(season$ticketSales * coefficient_list,0)

  season$ticketSales <- sapply(season$ticketSales,
                               function(x) f_constrain_sales(x))

  return(season)

}
