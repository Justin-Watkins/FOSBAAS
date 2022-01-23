#' @title f_add_zero
#' @param number A number
#' @return single character number with a zero prefix. It does nothing if the number is > 10
#' @examples
#' f_add_zero(2)
#' @description Helper function to organize numerical data on graphs
#' @source \url{https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_add_zero.R}
#' @export

f_add_zero <- function(number){
  if(number < 10){min <- paste('0', as.character(number), sep = '')}
  else{min <- as.character(number)}
  return(min)
}
