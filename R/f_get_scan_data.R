#' @title f_get_scan_data
#' @description Build a parabolic function
#' @source \url{GIT}
#' @export

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
