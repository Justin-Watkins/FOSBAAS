#' @title f_assign_promotion
#' @description Assign promotions to random dates
#' @source \url{GIT}
#' @export

f_assign_promotion <- function(number_bbhead,number_concert,number_other,games,seed){

  set.seed <- seed

  bbh <- rep("bobblehead",number_bbhead)
  con <- rep("concert",number_concert)
  oth <- rep("other",number_other)
  gam <- rep("none",games - sum(number_bbhead,number_concert,number_other))

  sample_list <- c(bbh,con,oth,gam)

  promos <- sample(sample_list,length(sample_list), replace = FALSE)

  return(promos)
}
