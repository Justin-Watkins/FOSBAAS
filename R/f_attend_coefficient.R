#' @title f_attend_coefficient
#' @description Buikd a term to modify ticket sales
#' @source \url{GIT}
#' @export

f_attend_coefficient <- function(opponent,week_end,school_out,lag,opening_day,promotion,seed){

  mod <- 1
  set.seed(seed)

  if(opponent %in% c("BOS", "CHC", "NYY", "LAD", "STL")){mod <- mod + rnorm(1,.5,.05)}
  else if(opponent %in% c("LAA", "HOU", "NYM", "SF", "PHI")){mod <- mod + rnorm(1, .25, .05)}
  else if(opponent %in% c("BAl", "KAN", "SD", "TEX")){mod <- mod + rnorm(1, 0, .05)}
  else{mod <- mod + 0}

  if(week_end == TRUE){mod <- mod + rnorm(1,.4,.05)}
  else{mod == mod + 0}

  if(school_out == TRUE){mod <- mod + rnorm(1,.2,.05)}
  else{mod <- mod + 0}

  if(lag > 1){mod <- mod + rnorm(1,.1,.02)}
  else{mod <- mod + 0}

  if(opening_day == TRUE){mod <- mod + rnorm(1,.7,.02)}
  else{mod <- mod + 0}

  if(promotion == "concert"){mod <- mod + rnorm(1,.15,.02)}
  else if(promotion == "bobblehead"){mod <- mod + rnorm(1,.25,.02)}
  else if(promotion == "other"){mod <- mod + rnorm(1,.05,.02)}
  else{mod <- mod + 0}

  return(mod)
}
