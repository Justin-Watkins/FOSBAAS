#' Simulate per-ticket spend
#'
#' Draws an average spend-per-ticket figure for an account. Corporate accounts,
#' full-season plans and longer-tenured customers spend more. Designed to be
#' called row-wise when building lead-scoring data with
#' [f_create_lead_scoring_data()].
#'
#' @param corporate Account type: `"c"` (corporate) or `"i"` (individual).
#' @param planType Plan type: `"f"` (full season) or `"p"` (partial).
#' @param tenure Account tenure in years.
#' @param avgTenure Average tenure across all accounts, used as the
#'   high/low threshold.
#'
#' @return A single non-negative rounded spend figure (per ticket).
#'
#' @details
#' Set the seed once in the caller for reproducibility.
#'
#' @examples
#' set.seed(1)
#' f_calculate_spend(corporate = "c", planType = "f",
#'                   tenure = 12, avgTenure = 8)
#'
#' @family lead_scoring
#' @importFrom stats rnorm
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_calculate_spend.R>
#' @export
f_calculate_spend <- function(corporate, planType, tenure, avgTenure) {
  if (corporate == "c" && planType == "f" && tenure >= avgTenure) {
    round(abs(rnorm(1, mean = 7500, sd = 800)), 0)
  } else if (corporate == "i" && planType == "f" && tenure >= avgTenure) {
    round(abs(rnorm(1, mean = 2100, sd = 500)), 0)
  } else if (corporate == "c" && planType == "p" && tenure >= avgTenure) {
    round(abs(rnorm(1, mean = 2000, sd = 300)), 0)
  } else if (corporate == "i" && planType == "p" && tenure >= avgTenure) {
    round(abs(rnorm(1, mean = 1200, sd = 200)), 0)
  } else if (corporate == "c" && planType == "f" && tenure <= avgTenure) {
    round(abs(rnorm(1, mean = 5000, sd = 500)), 0)
  } else if (corporate == "i" && planType == "f" && tenure <= avgTenure) {
    round(abs(rnorm(1, mean = 2000, sd = 300)), 0)
  } else if (corporate == "c" && planType == "p" && tenure <= avgTenure) {
    round(abs(rnorm(1, mean = 2000, sd = 400)), 0)
  } else if (corporate == "i" && planType == "p" && tenure <= avgTenure) {
    round(abs(rnorm(1, mean = 800, sd = 75)), 0)
  } else {
    round(abs(rnorm(1, mean = 2500, sd = 300)), 0)
  }
}
