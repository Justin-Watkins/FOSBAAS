#' @title f_get_line_length
#' @param seed A number
#' @param n A number representing the number of returned values
#' @param u1 A number mean value
#' @param sd1 A number standard deviation
#' @param u2 A number mean value
#' @param sd2 A number standard deviation
#' @return creates a bimodal distribution of line length
#' @examples
#' f_get_line_length(755,300,20,10,30,5)
#' @description Create a data frame to simulate line length
#' @source \url{https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_line_length.R}
#' @export


f_get_line_length <- function(seed,n = 300,u1,sd1,u2,sd2){
  ## Create a bi-modal distribution
  set.seed(seed)
  group_1 <-  abs(rnorm(n, u1, sd1))
  group_2 <-  abs(rnorm(n, u2, sd2))
  line <- sample(round(c(sample(group_1,n *.6),
                         sample(group_2,n*.4)),0),n,replace = FALSE)
  ## Create ordinal data
  x    <- 1
  i    <- 1
  samples <- c(seq(from = 1, to = 30, by = 2),
               rev(seq(from = 1, to = 30, by = 2)),
               seq(from = 1, to = 15, by = 1),
               rev(seq(from = 1, to = 15, by = 1)))

  lineLength <- list()

  while (x <= length(samples)) {
    alter <- samples[x]
    y <- 1
    while(y <= 5){
      figure <- rnorm(1, alter, sd(samples))
      lineLength[i] <- round(ifelse(figure < 0,0,figure),0)
      y <- y + 1
      i <- i + 1
    }
    x <- x + 1
  }

  lines <- data.frame(observation = seq(1:n),
                      lineLength  = unlist(lineLength))
  names(lines) <- c('observation','lineLength')
  return(lines)
}
