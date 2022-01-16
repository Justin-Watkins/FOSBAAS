#' @title f_get_time_observations
#' @description Create time sequence for observations
#' @source \url{GIT}
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
