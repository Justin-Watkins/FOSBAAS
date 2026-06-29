#' Simulate marital status for one customer
#'
#' Draws a marital status ("married" or "single") for a single customer, with
#' probabilities that depend on gender and age. Designed to be called row-wise
#' (e.g. via `mapply()`) when building [demographic_data].
#'
#' @param gender Customer gender, `"m"` or `"f"`.
#' @param age Customer age in years.
#'
#' @return A single character value: `"m"` (married) or `"s"` (single).
#'
#' @section Bug fix:
#' The old version called `set.seed(seed)` *inside* the function. Because it was
#' applied row-by-row with one shared seed, every call reset the generator and
#' the draws were not independent. The `seed` argument has been removed; the
#' caller should set the seed once before iterating.
#'
#' @examples
#' set.seed(400)
#' f_demographics_married(gender = "f", age = 52)
#'
#' @family demographic_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_demographics_married.R>
#' @export
f_demographics_married <- function(gender, age) {
  married <- c("m", "s")

  if (gender == "m" && age <= 40) {
    sample(married, 1, prob = c(.35, .65))
  } else if (gender == "m" && age > 40) {
    sample(married, 1, prob = c(.70, .30))
  } else if (gender == "f" && age <= 40) {
    sample(married, 1, prob = c(.45, .55))
  } else if (gender == "f" && age > 40) {
    sample(married, 1, prob = c(.85, .15))
  } else {
    sample(married, 1, prob = c(.50, .50))
  }
}
