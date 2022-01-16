#' @title f_demographics_income
#' @description Determine the household income of the fan
#' @source \url{GIT}
#' @export


f_demographics_income <- function(married,gender,age,age_low,age_high){


  means <- c(3000, 2500, 2000, 1500, 1000, 500)

  if(married == 'm' & gender == 'm' & age >= age_high){income <- sample(means, 1, prob = c(.5,.2,.1,.1,.1,.00))
  }else if(married == 'm' & gender == 'f' & age >= age_high){income <- sample(means, 1, prob = c(.5,.2,.1,.1,.05,.05))
  }else if(married == 'm' & gender == 'm' & age >= age_low & age < age_high){income <- sample(means, 1, prob = c(.5,.2,.1,.1,.1,.00))
  }else if(married == 'm' & gender == 'f' & age >= age_low & age < age_high){income <- sample(means, 1, prob = c(.5,.2,.1,.1,.05,.05))
  }else if(married == 'm' & gender == 'm' & age < age_low){income <- sample(means, 1, prob = c(.2,.2,.2,.2,.1,.1))
  }else if(married == 'm' & gender == 'f' & age < age_low){income <- sample(means, 1, prob = c(.1,.1,.2,.2,.2,.2))
  }else if(married == 's' & gender == 'm' & age >= age_high){income <- sample(means, 1, prob = c(.3,.2,.3,.1,.05,.05))
  }else if(married == 's' & gender == 'f' & age >= age_high){income <- sample(means, 1, prob = c(.1,.1,.2,.3,.2,.1))
  }else if(married == 's' & gender == 'm' & age >= age_low & age < age_high){income <- sample(means, 1, prob = c(.2,.2,.2,.2,.1,.1))
  }else if(married == 's' & gender == 'f' & age >= age_low & age < age_high){income <- sample(means, 1, prob = c(.1,.1,.2,.2,.2,.2))
  }else if(married == 's' & gender == 'm' & age < age_low){income <- sample(means, 1, prob = c(.1,.1,.1,.2,.2,.3))
  }else if(married == 's' & gender == 'f' & age < age_low){income <- sample(means, 1, prob = c(.0,.1,.1,.2,.3,.3))
  }else{income <- sample(means, 1, prob = c(.2,.2,.2,.2,.1,.1))}

  income <- rnorm(1, income, income/10)
  return(round(abs(income),0))

}
