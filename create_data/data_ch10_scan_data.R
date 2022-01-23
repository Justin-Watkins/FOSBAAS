#-----------------------------------------------------------------
# scan_data data set
#
# This data set approximates sans into a ballpark
# library(FOSBAAS)
# library(dplyr)
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# BEGIN Scan data
#-----------------------------------------------------------------


library(FOSBAAS)
library(dplyr)

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

f_calc_scans <- function(x,y,j){
  a <- y/(x^2 - 300*x + 300)
  z <- a*(j^2 - 301*j + 300)
  return(z)
}

f_get_scan_data <- function(x_value,y_value,seed,sd_mod){

  require(FOSBAAS)

  x_val <- x_value
  y_val <- y_value
  obs   <- seq(1,300, by = 1)
  set.seed(seed)

  scans     <- mapply(f_calc_scans,x_val,y_val,obs)
  scan_data <- data.frame(observations = obs,
                          scans        = scans)

  scan_data$scans <- round(sapply(scan_data$scans,
                                  function(x) abs(rnorm(1,x,x/sd_mod))),0)
  return(scan_data)
}

library(readr)
scans_a <- FOSBAAS::f_get_scan_data(x_value = 230,
                                    y_value = 110,
                                    seed    = 714,
                                    sd_mod  = 14)


scans_a$action_time <- f_get_time_observations(17,21)
scans_a$date <- "4/1/2024"


scans_b <- FOSBAAS::f_get_scan_data(x_value = 230,
                                    y_value = 123,
                                    seed    = 755,
                                    sd_mod  = 17)

scans_b$action_time <- f_get_time_observations(17,21)
scans_b$date <- "4/2/2024"

scans_c <- FOSBAAS::f_get_scan_data(x_value = 230,
                                    y_value = 133,
                                    seed    = 660,
                                    sd_mod  = 19)


scans_c$action_time <- f_get_time_observations(17,21)
scans_c$date <- "4/3/2024"

scans <- bind_rows(scans_a,scans_b,scans_c)

#write_csv(scans,"scan_data.csv")

#-----------------------------------------------------------------
# END Scan data
#-----------------------------------------------------------------
