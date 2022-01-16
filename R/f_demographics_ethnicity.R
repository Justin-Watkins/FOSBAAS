#' @title f_demographics_ethnicity
#' @description Determine the ethnicity of the fan
#' @source \url{GIT}
#' @export


f_demographics_ethnicity <- function(seed,distance,age){

  ethnicity <- c("w", "aa", "h", "a")

  if(distance <= 35 & age <= 40){eth <-sample(ethnicity, 1, replace = TRUE, prob = c(.87, .02, .10, .01))}
  else if(distance > 35 & age > 40){eth <-sample(ethnicity, 1, replace = TRUE, prob = c(.80, .12, .08, .02))}
  else if(distance <= 35 & age <= 40){eth <-sample(ethnicity, 1, replace = TRUE, prob = c(.70, .15, .12, .03))}
  else if(distance > 35 & age > 40){eth <-sample(ethnicity, 1, replace = TRUE, prob = c(.95, .11, .06, .03))}
  else{eth <-sample(ethnicity, 1, replace = TRUE, prob = c(.80, .11, .06, .03))}

  return(eth)

}
