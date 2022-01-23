#' @title f_build_freq_table
#' @param variable A vector of factors
#' @return creates a frequency table
#' @examples
#' f_build_freq_table()
#' @description Create a data frame of a frequency table
#' @source \url{https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_build_freq_table.R}
#' @export

f_build_freq_table <- function(variable){

  pr          <- as.data.frame(table(variable))
  pr$prob     <- pr$Freq/sum(pr$Freq)
  pr$variable <- as.numeric(as.character(pr$variable))

  return(pr)

}
