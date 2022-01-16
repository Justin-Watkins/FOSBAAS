
#' @title f_get_third_degree_fit
#' @description Get third degree fit
#' @source \url{GIT}
#' @export




f_get_third_degree_fit <- function(new_var,dist_fit){
  var <- coef(dist_fit)[1] + (coef(dist_fit)[2]*new_var+ (coef(dist_fit)[3] * new_var^2) + (coef(dist_fit)[4] * new_var^3))
  return(var)
}
