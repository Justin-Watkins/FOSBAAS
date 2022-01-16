#' @title f_apply_static_fields
#' @description Determine which team is played
#' @source \url{GIT}
#' @export

f_apply_static_fields <- function(date){

  require(lubridate)

  gam <- seq(1,81)
  dow <- lubridate::wday(date,label=TRUE)
  mon <- lubridate::month(date,label=TRUE)
  we  <- ifelse(dow %in% c('Fri','Sat'), TRUE, FALSE)
  sch <- ifelse(mon %in% c('Jun',"Jul"), TRUE,FALSE)
  opd <- ifelse(gam == 1,TRUE,FALSE)
  yer <- lubridate::year(date[1])

  fields <- list()
  fields[[1]] <- gam
  fields[[2]] <- dow
  fields[[3]] <- mon
  fields[[4]] <- we
  fields[[5]] <- sch
  fields[[6]] <- opd
  fields[[7]] <- yer
  return(fields)
}
