#' @title f_get_scans
#' @param new_time y2 value for point slope formula
#' @param zero_1 starting point
#' @param zero_2 end point
#' @return Scan value
#' @examples
#' f_get_scans(2,0,300)
#' @description Return scans values. This function only works with fit values.
#' @source \url{https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_scans.R}
#' @export

f_get_scans <- function(new_time,zero_1,zero_2){

  if(new_time > zero_2){return(round(abs(rnorm(1,6,1)),0))}
  else if(new_time < zero_1){return(round(abs(rnorm(1,3,1)),0))}
  else if(new_time > 400){return(0)}
  else if(new_time < 0){return(0)}
  else{

    scans <- coef(fit)[1] + (coef(fit)[2]*new_time + (coef(fit)[3] * new_time^2))
    scans <- jitter(scans,5)
    return(abs(round(scans,0)))}
}


