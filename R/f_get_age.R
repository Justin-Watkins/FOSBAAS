#' @title f_get_age
#' @description create a bimodal age distribution
#' @source \url{GIT}
#' @export

f_get_age <- function(seed,num_rows = 200000, g1_mean,g2_mean,g1_sd,g2_sd){

  group_1 <-  rnorm(num_rows, g1_mean, g1_sd)
  group_2 <-  rnorm(num_rows, g2_mean, g2_sd)
  age <- list()
  age <- sample(round(c(sample(group_1,num_rows *.4),
                        sample(group_2,num_rows*.6)),0),
                num_rows,replace = FALSE)
  return(age)
}
