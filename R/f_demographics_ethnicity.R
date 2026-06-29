#' Simulate ethnicity for one customer
#'
#' Draws an ethnicity code for a single customer, with probabilities that depend
#' on how far they live from the venue and their age. Designed to be called
#' row-wise when building [demographic_data].
#'
#' @param distance Distance from the venue, in miles.
#' @param age Customer age in years.
#'
#' @return A single character code: `"w"`, `"aa"`, `"h"` or `"a"`.
#'
#' @section Bug fixes:
#' The first and third branches (and the second and fourth) previously had
#' identical conditions, so two branches were dead code and one probability
#' vector summed to more than 1. The four distance-by-age cells are now distinct
#' and each probability vector sums to 1. The unused `seed` argument was removed;
#' set the seed once in the caller.
#'
#' @examples
#' set.seed(500)
#' f_demographics_ethnicity(distance = 20, age = 35)
#'
#' @family demographic_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_demographics_ethnicity.R>
#' @export
f_demographics_ethnicity <- function(distance, age) {
  ethnicity <- c("w", "aa", "h", "a")

  if (distance <= 35 && age <= 40) {
    sample(ethnicity, 1, prob = c(.70, .15, .12, .03))
  } else if (distance <= 35 && age > 40) {
    sample(ethnicity, 1, prob = c(.80, .10, .07, .03))
  } else if (distance > 35 && age <= 40) {
    sample(ethnicity, 1, prob = c(.85, .06, .06, .03))
  } else if (distance > 35 && age > 40) {
    sample(ethnicity, 1, prob = c(.90, .05, .03, .02))
  } else {
    sample(ethnicity, 1, prob = c(.80, .11, .06, .03))
  }
}
