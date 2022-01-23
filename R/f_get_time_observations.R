#' @title f_get_time_observations
#' @param start A number < 24
#' @param end A number < 24
#' @return Formatted military time values
#' @examples
#' f_get_time_observations(5,6)
#' @description Helper function to format time
#' @source \url{https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_time_observations.R}
#' @export

f_get_time_observations <- function(start,end){

  hours   <- sort(rep(seq(start,end, by = 1),60))
  minutes <- rep(seq(0,59, by = 1),4)
  minutes_format <- sapply(minutes, function(x) f_add_zero(x))
  hours_format   <- sapply(hours, function(x) f_add_zero(x))

  time <- paste(hours_format,":",
                minutes_format,
                sep = '')
  return(time)

}
