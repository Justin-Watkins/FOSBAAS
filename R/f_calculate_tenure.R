#' @title f_calculate_tenure
#' @description Assign a tenure amount in years
#' @source \url{GIT}
#' @export
f_calculate_tenure <- function(corporate,planType,distance,avgDist){
  if(corporate == "c" & planType == "f" & distance <= avgDist){ten <-round(abs(rnorm(1,mean = 14,sd = 6)),0)}
  else if(corporate == "i" & planType == "f" & distance <= avgDist){ten <-round(abs(rnorm(1,mean = 10,sd = 6)),0)}
  else if(corporate == "c" & planType == "p" & distance <= avgDist){ten <-round(abs(rnorm(1,mean = 3,sd = 2)),0)}
  else if(corporate == "i" & planType == "p" & distance <= avgDist){ten <-round(abs(rnorm(1,mean = 3,sd = 2)),0)}
  else if(corporate == "c" & planType == "f" & distance >= avgDist){ten <-round(abs(rnorm(1,mean = 9,sd = 3)),0)}
  else if(corporate == "i" & planType == "f" & distance >= avgDist){ten <-round(abs(rnorm(1,mean = 7,sd = 3)),0)}
  else if(corporate == "c" & planType == "p" & distance >= avgDist){ten <-round(abs(rnorm(1,mean = 2,sd = 1)),0)}
  else if(corporate == "i" & planType == "p" & distance >= avgDist){ten <-round(abs(rnorm(1,mean = 2,sd = 1)),0)}
  else{ten <-round(abs(rnorm(1,mean = 8,sd = 3)),0)}
  return(ten)
}
