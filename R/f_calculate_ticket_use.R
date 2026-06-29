#' Simulate the share of tickets used
#'
#' Draws the fraction of an account's tickets that are actually used (scanned at
#' the gate). Corporate accounts and customers close to the venue use a higher
#' share. Designed to be called row-wise when building lead-scoring data with
#' [f_create_lead_scoring_data()].
#'
#' @param corporate Account type: `"c"` (corporate) or `"i"` (individual).
#' @param distance Distance from the venue, in miles.
#' @param avgDist Average distance across all accounts, used as the near/far
#'   threshold.
#'
#' @return A single proportion between 0 and 1.
#'
#' @details
#' Set the seed once in the caller for reproducibility.
#'
#' @examples
#' set.seed(1)
#' f_calculate_ticket_use(corporate = "c", distance = 10, avgDist = 25)
#'
#' @family lead_scoring
#' @importFrom stats runif
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_calculate_ticket_use.R>
#' @export
f_calculate_ticket_use <- function(corporate, distance, avgDist) {
  if (corporate == "c" && distance <= avgDist) {
    runif(1, min = .89, max = 1)
  } else if (corporate == "i" && distance <= avgDist) {
    runif(1, min = .82, max = .94)
  } else if (corporate == "c" && distance >= avgDist) {
    runif(1, min = .65, max = .90)
  } else if (corporate == "i" && distance >= avgDist) {
    runif(1, min = .55, max = .85)
  } else {
    runif(1, min = .65, max = .95)
  }
}
