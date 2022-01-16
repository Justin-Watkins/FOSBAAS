#' @title f_constrain_sales
#' @description Constrain demand to fit stadium size
#' @source \url{GIT}
#' @export


f_constrain_sales <- function(sales){

  if(sales > 45000){c_sales <- 45000
  }else{c_sales <- sales}

  return(c_sales)


}
