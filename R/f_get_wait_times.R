#' @title f_get_wait_times
#' @description Create a data frame to simulate concession waits
#' @source \url{GIT}
#' @export


f_get_wait_times <- function(seed,n = 300,time,rate1,rate2,rate3){
  set.seed(seed)

  order_times       <- rexp(n, rate = rate1)
  payment_times     <- rexp(n, rate = rate2)
  fulfillment_times <- rexp(n, rate = rate3)
  total_time        <- order_times + payment_times + fulfillment_times

  wait_times <- data.frame(transaction  = seq(1,n, by = 1),
                           orderTimes   = order_times,
                           paymentTimes = payment_times,
                           fulfillTimes = fulfillment_times,
                           totalTime    = total_time)
  wait_times[] <- apply(wait_times,2,function(x) round(x,0))
  return(wait_times)
}
