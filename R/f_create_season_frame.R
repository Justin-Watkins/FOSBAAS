#' @title f_create_season_frame
#' @description Build a season table
#' @source \url{GIT}
#' @export

f_create_season_frame <- function(){

  season_data        <- data.frame(matrix(nrow = 81, ncol = 12))
  names(season_data) <- c("gameNumber","team","date","dayOfWeek",
                          "month","weekEnd","schoolInOut",
                          "daysSinceLastGame","openingDay","promotion",
                          "ticketSales","season")
  return(season_data)
}
