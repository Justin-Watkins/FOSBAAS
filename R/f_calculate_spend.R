#' @title f_calculate_spend
#' @description Assign a spend per ticket amount
#' @source \url{GIT}
#' @export
f_calculate_spend <- function(corporate,planType,tenure,avgTenure){
  #set.seed(seed)
  if(corporate == "c" & planType == "f" & tenure >= avgTenure){spend <-round(abs(rnorm(1,mean = 7500,sd = 800)),0)}
  else if(corporate == "i" & planType == "f" & tenure >= avgTenure){spend <-round(abs(rnorm(1,mean = 2100,sd = 500)),0)}
  else if(corporate == "c" & planType == "p" & tenure >= avgTenure){spend <-round(abs(rnorm(1,mean = 2000,sd = 300)),0)}
  else if(corporate == "i" & planType == "p" & tenure >= avgTenure){spend <-round(abs(rnorm(1,mean = 1200,sd = 200)),0)}
  else if(corporate == "c" & planType == "f" & tenure <= avgTenure){spend <-round(abs(rnorm(1,mean = 5000,sd = 500)),0)}
  else if(corporate == "i" & planType == "f" & tenure <= avgTenure){spend <-round(abs(rnorm(1,mean = 2000,sd = 300)),0)}
  else if(corporate == "c" & planType == "p" & tenure <= avgTenure){spend <-round(abs(rnorm(1,mean = 2000,sd = 400)),0)}
  else if(corporate == "i" & planType == "p" & tenure <= avgTenure){spend <-round(abs(rnorm(1,mean = 800,sd = 75)),0)}
  else{spend <-round(abs(rnorm(1,mean = 2500,sd = 300)),0)}
  return(spend)
}
