#' Solve an M/M/k/N queue
#'
#' Computes the standard performance measures of an M/M/k/N queue: `k` parallel
#' servers, Poisson arrivals, exponential service times and a finite system
#' capacity of `N` customers. The book uses it to reason about staffing at a
#' concession stand or gate.
#'
#' @param k Number of servers.
#' @param N Maximum number of customers allowed in the system (capacity).
#' @param ta Mean time between arrivals (same time unit as `ts`).
#' @param ts Mean service time.
#'
#' @return A data frame with columns `Metric` and `Value` and one row for each
#'   of: servers, system capacity, time between arrivals, average service time,
#'   minutes in service, minutes in queue and minutes in the system.
#'
#' @details
#' Arrival rate is `lambda = 1/ta`, service rate `mu = 1/ts`, and utilisation
#' `rho = lambda/mu`. The state probabilities give the effective arrival rate and
#' the expected number in service and in queue, from which the waiting times
#' follow by Little's law.
#'
#' @examples
#' f_get_MMKN(k = 2, N = 5, ta = 10, ts = 8)
#'
#' @family operations
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_mmkn.R>
#' @export
f_get_MMKN <- function(k, N, ta, ts) {

  lambda <- 1 / ta # arrivals per unit time
  mu     <- 1 / ts # service completions per unit time
  rho    <- lambda / mu # utilisation ratio

  # Probability of an empty system, P0.
  n  <- seq(0, N - 1, by = 1)
  P0 <- 1 / sum(((rho^n) / factorial(n)) +
                  ((rho^k) / (factorial(k) * ((k^(N - k + 1)) -
                  (rho^(N - k + 1)) / ((k - rho) * (k^(N - k)))))))

  # State probabilities for n <= k and n > k.
  n   <- seq(0, k, by = 1)
  Pn0 <- rho^n / factorial(n) * P0
  n   <- seq(k + 1, N, by = 1)
  Pn1 <- rho^n / (factorial(k) * k^(n - k)) * P0
  Pn  <- c(Pn0, Pn1)

  len      <- length(Pn)
  lambda_e <- lambda * (1 - Pn[len]) # effective arrival rate
  rho_e    <- lambda_e / mu

  Ls <- rho_e # expected number in service

  n  <- seq(k + 2, N + 1, by = 1)
  Lq <- sum((n - (k + 1)) * Pn[n]) # expected number in queue

  Ws <- Ls / lambda_e # time in service
  Wq <- Lq / lambda_e # time in queue
  W  <- Wq + Ws       # time in system

  data.frame(
    Metric = c("Servers:", "System Capacity:", "Time between arrivals:",
               "Average service time:", "Minutes in service:",
               "Minutes in queue:", "Minutes in system:"),
    Value  = c(k, N, ta, ts, Ws, Wq, W),
    stringsAsFactors = FALSE
  )
}
