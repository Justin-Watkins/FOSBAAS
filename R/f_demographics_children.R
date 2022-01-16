#' @title f_demographics_children
#' @description Determine the ethnicity of the fan
#' @source \url{GIT}
#' @export



f_demographics_children <- function(seed,married,age){

  children <- c("y", "n")

  if(married == 'm' & age <= 40){child <-sample(children, 1, replace = TRUE, prob = c(.60, .40))}
  else if(married == 's' & age < 40){child <-sample(children, 1, replace = TRUE, prob = c(.15, .85))}
  else if(married == 'm' & age > 40){child <-sample(children, 1, replace = TRUE, prob = c(.70, .30))}
  else if(married =='s' & age > 40){child <-sample(children, 1, replace = TRUE, prob = c(.10, .90))}
  else{child <-sample(children, 1, replace = TRUE, prob = c(.50, .50))}

  return(child)

}
