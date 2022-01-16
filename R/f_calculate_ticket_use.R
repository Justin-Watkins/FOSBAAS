#' @title f_calculate_ticket_use
#' @description What percentage of tickets get used
#' @source \url{GIT}
#' @export
f_calculate_ticket_use <- function(corporate,distance,avgDist){
  if(corporate == "c" & distance <= avgDist){tu <- runif(1,min = .89, max = 1)}
  else if(corporate == "i" & distance <= avgDist){tu <- runif(1,min = .82, max = .94)}
  else if(corporate == "c" & distance >= avgDist){tu <- runif(1,min = .65, max = .9)}
  else if(corporate == "i" & distance >= avgDist){tu <- runif(1,min = .55, max = .85)}
  else{tu <- runif(1,min = .65, max = .95)}
  return(tu)
}
