#' @title f_get_third_degree_fit
#' @param new_var
#' @param dist_fit
#' @return output of third degree polynomial function
#' @examples
#' f_get_third_degree_fit()
#' @description Outputs a value based on a third degree polynomial function
#' @source \url{https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_third_degree_fit.R}
#' @export


f_get_third_degree_fit <- function(new_var,dist_fit){
  var <- coef(dist_fit)[1] + (coef(dist_fit)[2]*new_var+ (coef(dist_fit)[3] * new_var^2) + (coef(dist_fit)[4] * new_var^3))
  return(var)
}
