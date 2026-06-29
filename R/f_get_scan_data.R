#' Simulate a session of gate scans
#'
#' Builds 300 minutes of gate-scan counts by evaluating the [f_calc_scans()]
#' parabola at each minute and adding multiplicative noise, producing the
#' rise-and-fall pattern of fans entering a ballpark. This is the building block
#' behind the bundled [scan_data].
#'
#' @param x_value Width parameter passed to [f_calc_scans()] as `x`.
#' @param y_value Peak-height parameter passed to [f_calc_scans()] as `y`.
#' @param seed Integer seed; makes the noisy counts reproducible.
#' @param sd_mod Noise control: larger values give smoother (less noisy) counts
#'   (the per-point standard deviation is the count divided by `sd_mod`).
#'
#' @return A data frame with 300 rows and the columns `observations` (`1:300`)
#'   and `scans` (non-negative whole-number counts).
#'
#' @examples
#' scans <- f_get_scan_data(x_value = 230, y_value = 110, seed = 714, sd_mod = 14)
#' head(scans)
#'
#' @family operations
#' @seealso [f_calc_scans()], [scan_data]
#' @importFrom stats rnorm
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_scan_data.R>
#' @export
f_get_scan_data <- function(x_value, y_value, seed, sd_mod) {
  obs <- seq_len(300)
  set.seed(seed)

  scans <- mapply(f_calc_scans, x_value, y_value, obs)
  scan_data <- data.frame(observations = obs, scans = scans)
  scan_data$scans <- round(vapply(scan_data$scans,
                                  function(x) abs(rnorm(1, x, x / sd_mod)),
                                  numeric(1)), 0)
  scan_data
}
