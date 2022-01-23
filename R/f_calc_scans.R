#' @title f_calc_scans
#' @param x `a <- y/(x^2 - 300*x + 300)`
#' @param y `a <- y/(x^2 - 300*x + 300)`
#' @param j `z <- a*(j^2 - 301*j + 300)`
#' @return value
#' @examples
#' f_calc_scans(10,20,6)
#' @description Return values from parabolic function
#' @source \url{https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_calc_scans.R}
#' @export


f_calc_scans <- function(x,y,j){

  a <- y/(x^2 - 300*x + 300)
  z <- a*(j^2 - 301*j + 300)

  return(z)

}
