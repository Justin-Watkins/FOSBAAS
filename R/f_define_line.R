#' @title f_define_line
#' @description Create time sequence for observations
#' @source \url{GIT}
#' @export


f_define_line <- function(y2,y1,x2,x1,x){
  m <- (y2-y1)/(x2-x1)
  b <- y2 - (m*x2)
  y <- m*x + b
  return(y)
}
