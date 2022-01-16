#' @title f_demographics_married
#' @description Determine if the customer is married
#' @source \url{GIT}
#' @export


f_demographics_married <- function(seed,gender,age){

  set.seed(seed)
  married <- c("m", "s")

  if(gender == 'm' & age <= 40){status <-sample(married, 1, replace = TRUE, prob = c(.35, .65))}
  else if(gender == 'm' & age > 40){status <-sample(married, 1, replace = TRUE, prob = c(.70, .30))}
  else if(gender == 'f' & age <= 40){status <-sample(married, 1, replace = TRUE, prob = c(.45, .55))}
  else if(gender == 'f' & age > 40){status <-sample(married, 1, replace = TRUE, prob = c(.85, .15))}
  else{status <- sample(married, 1, replace = TRUE, prob = c(.50, .50))}

  return(status)
}
