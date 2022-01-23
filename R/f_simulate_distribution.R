#' @title f_simulate_distribution
#' @param observation
#' @param rand
#' @param seeds
#' @return Gets Wait time
#' @examples
#' f_simulate_distribution()
#' @description Simulates wait time distribution
#' @source \url{https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_simulate_distribution.R}
#' @export


f_simulate_distribution <- function(observation,rand,seeds = 1){
  require(FOSBAAS)
  if(seeds == 1){set.seed(round(runif(1)*100,0))
  }else{set.seed(seeds)}

  dist      <- f_build_freq_table(observation)
  min_val   <- 0 #min(dist$Freq)
  max_val   <- max(dist$Freq)
  fit       <- lm(dist$variable~poly(dist$Freq,3,raw=TRUE))
  dist$pred <- predict(fit)
  rand_seq  <- runif(rand, min=min_val, max=max_val)
  wait      <- sapply(rand_seq ,function(x) f_get_third_degree_fit(x,fit))
  return(wait)
}
