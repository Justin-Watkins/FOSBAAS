#' Simulate concession-stand wait times
#'
#' Builds a table of transaction wait times by decomposing each transaction into
#' three exponentially distributed stages -- ordering, paying and fulfilment --
#' and summing them. This is the building block behind [wait_times_data].
#'
#' @param seed Integer seed; makes the wait times reproducible.
#' @param n Number of transactions to simulate; defaults to `300`.
#' @param time Reserved for backward compatibility; currently ignored.
#' @param rate1 Rate of the exponential order-time distribution.
#' @param rate2 Rate of the exponential payment-time distribution.
#' @param rate3 Rate of the exponential fulfilment-time distribution.
#'
#' @return A data frame with `n` rows and the columns `transaction`,
#'   `orderTimes`, `paymentTimes`, `fulfillTimes` and `totalTime`, each rounded
#'   to whole seconds.
#'
#' @details
#' A smaller `rate` means a longer average time for that stage (the mean of an
#' exponential is `1 / rate`).
#'
#' @examples
#' wt <- f_get_wait_times(seed = 755, n = 300, rate1 = .03,
#'                        rate2 = .06, rate3 = .15)
#' head(wt)
#'
#' @family operations
#' @seealso [wait_times_data]
#' @importFrom stats rexp
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_wait_times.R>
#' @export
f_get_wait_times <- function(seed, n = 300, time, rate1, rate2, rate3) {
  set.seed(seed)

  order_times       <- rexp(n, rate = rate1)
  payment_times     <- rexp(n, rate = rate2)
  fulfillment_times <- rexp(n, rate = rate3)

  wait_times <- data.frame(
    transaction  = seq_len(n),
    orderTimes   = order_times,
    paymentTimes = payment_times,
    fulfillTimes = fulfillment_times,
    totalTime    = order_times + payment_times + fulfillment_times
  )
  wait_times[] <- lapply(wait_times, round, 0)
  wait_times
}
