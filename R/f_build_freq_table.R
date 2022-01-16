
#' @title f_build_freq_table
#' @description Get a frequency table
#' @source \url{GIT}
#' @export



f_build_freq_table <- function(variable){

  pr          <- as.data.frame(table(variable))
  pr$prob     <- pr$Freq/sum(pr$Freq)
  pr$variable <- as.numeric(as.character(pr$variable))

  return(pr)

}
