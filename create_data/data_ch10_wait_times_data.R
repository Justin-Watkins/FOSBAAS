#-----------------------------------------------------------------
# wait_times data set
#
# This data set approximates wait times at a concession stans
#
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# BEGIN Wait Times
#-----------------------------------------------------------------

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

wait_times_a <- f_get_wait_times(seed  = 755,
                                 n     = 300,
                                 rate1 = .03,
                                 rate2 = .06,
                                 rate3 = .15)

wait_times_b <- f_get_wait_times(seed  = 714,
                                 n     = 300,
                                 rate1 = .02,
                                 rate2 = .05,
                                 rate3 = .13)

wait_times_c <- f_get_wait_times(seed  = 60,
                                 n     = 300,
                                 rate1 = .02,
                                 rate2 = .05,
                                 rate3 = .13)

wait_times <- bind_rows(wait_times_a,wait_times_b,wait_times_c)
#write_csv(wait_times,'wait_times_data.csv')
