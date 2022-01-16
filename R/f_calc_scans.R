#' @title f_calc_scans
#' @description Build a parabolic function
#' @source \url{GIT}
#' @export


f_calc_scans <- function(x,y,j){

  a <- y/(x^2 - 300*x + 300)
  z <- a*(j^2 - 301*j + 300)

  return(z)

}
