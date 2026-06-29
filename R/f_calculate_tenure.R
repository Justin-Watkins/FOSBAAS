#' Simulate account tenure in years
#'
#' Draws how many years an account has held season tickets. Corporate accounts,
#' full-season plans and customers who live close to the venue tend to have
#' longer tenures. Designed to be called row-wise when building lead-scoring
#' data with [f_create_lead_scoring_data()].
#'
#' @param corporate Account type: `"c"` (corporate) or `"i"` (individual).
#' @param planType Plan type: `"f"` (full season) or `"p"` (partial).
#' @param distance Distance from the venue, in miles.
#' @param avgDist Average distance across all accounts, used as the near/far
#'   threshold.
#'
#' @return A single non-negative whole number of years.
#'
#' @details
#' Set the seed once in the caller for reproducibility.
#'
#' @examples
#' set.seed(1)
#' f_calculate_tenure(corporate = "c", planType = "f",
#'                    distance = 10, avgDist = 25)
#'
#' @family lead_scoring
#' @importFrom stats rnorm
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_calculate_tenure.R>
#' @export
f_calculate_tenure <- function(corporate, planType, distance, avgDist) {
  if (corporate == "c" && planType == "f" && distance <= avgDist) {
    round(abs(rnorm(1, mean = 14, sd = 6)), 0)
  } else if (corporate == "i" && planType == "f" && distance <= avgDist) {
    round(abs(rnorm(1, mean = 10, sd = 6)), 0)
  } else if (corporate == "c" && planType == "p" && distance <= avgDist) {
    round(abs(rnorm(1, mean = 3, sd = 2)), 0)
  } else if (corporate == "i" && planType == "p" && distance <= avgDist) {
    round(abs(rnorm(1, mean = 3, sd = 2)), 0)
  } else if (corporate == "c" && planType == "f" && distance >= avgDist) {
    round(abs(rnorm(1, mean = 9, sd = 3)), 0)
  } else if (corporate == "i" && planType == "f" && distance >= avgDist) {
    round(abs(rnorm(1, mean = 7, sd = 3)), 0)
  } else if (corporate == "c" && planType == "p" && distance >= avgDist) {
    round(abs(rnorm(1, mean = 2, sd = 1)), 0)
  } else if (corporate == "i" && planType == "p" && distance >= avgDist) {
    round(abs(rnorm(1, mean = 2, sd = 1)), 0)
  } else {
    round(abs(rnorm(1, mean = 8, sd = 3)), 0)
  }
}
