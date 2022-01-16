#' @title f_assign_renwal
#' @description Assign a renewal probability
#' @source \url{GIT}
#' @export
f_assign_renewal <- function(x,renew){

  if(x == 10){sample(renew,1,prob = c(.99,.01))}
  else if(x == 9){sample(renew,1,prob = c(.98,.02))}
  else if(x == 8){sample(renew,1,prob = c(.95,.05))}
  else if(x == 7){sample(renew,1,prob = c(.95,.05))}
  else if(x == 6){sample(renew,1,prob = c(.92,.08))}
  else if(x == 5){sample(renew,1,prob = c(.90,.10))}
  else if(x == 4){sample(renew,1,prob = c(.85,.15))}
  else if(x == 3){sample(renew,1,prob = c(.80,.20))}
  else if(x == 2){sample(renew,1,prob = c(.30,.70))}
  else if(x == 1){sample(renew,1,prob = c(.25,.75))}
  else{sample(renew,1,prob = c(.5,.5))}
}
