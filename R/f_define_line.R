#' @title f_define_line
#' @param y2 y2 value for point slope formula
#' @param y1 y1 value for point slope formula
#' @param x2 x2 value for point slope formula
#' @param x1 x1 value for point slope formula
#' @param x x value for line
#' @return Y value for defined line
#' @examples
#' f_define_line(0,10,0,10,2)
#' @description Illustrative point slope formula
#' @source \url{https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_define_line.R}
#' @export

f_define_line <- function(y2,y1,x2,x1,x){
  m <- (y2-y1)/(x2-x1)
  b <- y2 - (m*x2)
  y <- m*x + b
  return(y)
}
