#' @title f_add_zero
#' @description Add a zero to a number for time conversion
#' @source \url{GIT}
#' @export

f_add_zero <- function(number){
  if(number < 10){min <- paste('0', as.character(number), sep = '')}
  else{min <- as.character(number)}
  return(min)
}
