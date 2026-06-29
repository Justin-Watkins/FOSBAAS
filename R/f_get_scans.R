#' Predict a scan count from a fitted curve
#'
#' Returns a noisy scan count at a given time using a fitted quadratic model,
#' with flat low-level counts outside the main arrival window. It is a teaching
#' helper that shows how a fitted model can drive a simulation.
#'
#' @param new_time The time (minute index) to predict a scan count for.
#' @param zero_1 Start of the main arrival window; before it, counts are low.
#' @param zero_2 End of the main arrival window; after it, counts are low.
#' @param fit A fitted quadratic model (e.g. from [stats::lm()]) whose first
#'   three coefficients are intercept, linear and quadratic terms.
#'
#' @return A single non-negative whole-number scan count.
#'
#' @section Bug fix:
#' The model is now passed in as the `fit` argument. The previous version
#' referred to a global variable named `fit`, so it only worked if such an
#' object happened to exist in the caller's environment.
#'
#' @examples
#' obs <- 1:300
#' shape <- data.frame(t = obs, scans = -0.01 * (obs - 150)^2 + 200)
#' fit <- lm(scans ~ t + I(t^2), data = shape)
#' set.seed(1)
#' f_get_scans(new_time = 150, zero_1 = 0, zero_2 = 300, fit = fit)
#'
#' @family operations
#' @importFrom stats coef rnorm
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_scans.R>
#' @export
f_get_scans <- function(new_time, zero_1, zero_2, fit) {
  if (new_time > zero_2) {
    round(abs(rnorm(1, 6, 1)), 0)
  } else if (new_time < zero_1) {
    round(abs(rnorm(1, 3, 1)), 0)
  } else if (new_time > 400 || new_time < 0) {
    0
  } else {
    co    <- coef(fit)
    scans <- co[1] + co[2] * new_time + co[3] * new_time^2
    abs(round(jitter(scans, 5), 0))
  }
}
