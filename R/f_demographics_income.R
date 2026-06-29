#' Simulate household income for one customer
#'
#' Draws a (scaled) household-income figure for a single customer. A base income
#' level is chosen from a discrete set with probabilities that depend on marital
#' status, gender and which age band the customer falls into, then jittered with
#' a small normal draw. Designed to be called row-wise when building
#' [demographic_data].
#'
#' @param married Marital status, `"m"` or `"s"`.
#' @param gender Gender, `"m"` or `"f"`.
#' @param age Customer age in years.
#' @param age_low Lower age-band cut point (e.g. the 30th percentile of age).
#' @param age_high Upper age-band cut point (e.g. the 60th percentile of age).
#'
#' @return A single non-negative rounded numeric value: the (scaled) household
#'   income. Married men and older customers skew toward the higher levels.
#'
#' @details
#' The values are deliberately on an arbitrary scaled axis rather than dollars.
#' Set the seed once in the caller for reproducibility.
#'
#' @examples
#' set.seed(700)
#' f_demographics_income(married = "m", gender = "m", age = 50,
#'                       age_low = 35, age_high = 52)
#'
#' @family demographic_simulation
#' @importFrom stats rnorm
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_demographics_income.R>
#' @export
f_demographics_income <- function(married, gender, age, age_low, age_high) {
  means <- c(3000, 2500, 2000, 1500, 1000, 500)

  base <- if (married == "m" && gender == "m" && age >= age_high) {
    sample(means, 1, prob = c(.5, .2, .1, .1, .1, .00))
  } else if (married == "m" && gender == "f" && age >= age_high) {
    sample(means, 1, prob = c(.5, .2, .1, .1, .05, .05))
  } else if (married == "m" && gender == "m" && age >= age_low && age < age_high) {
    sample(means, 1, prob = c(.5, .2, .1, .1, .1, .00))
  } else if (married == "m" && gender == "f" && age >= age_low && age < age_high) {
    sample(means, 1, prob = c(.5, .2, .1, .1, .05, .05))
  } else if (married == "m" && gender == "m" && age < age_low) {
    sample(means, 1, prob = c(.2, .2, .2, .2, .1, .1))
  } else if (married == "m" && gender == "f" && age < age_low) {
    sample(means, 1, prob = c(.1, .1, .2, .2, .2, .2))
  } else if (married == "s" && gender == "m" && age >= age_high) {
    sample(means, 1, prob = c(.3, .2, .3, .1, .05, .05))
  } else if (married == "s" && gender == "f" && age >= age_high) {
    sample(means, 1, prob = c(.1, .1, .2, .3, .2, .1))
  } else if (married == "s" && gender == "m" && age >= age_low && age < age_high) {
    sample(means, 1, prob = c(.2, .2, .2, .2, .1, .1))
  } else if (married == "s" && gender == "f" && age >= age_low && age < age_high) {
    sample(means, 1, prob = c(.1, .1, .2, .2, .2, .2))
  } else if (married == "s" && gender == "m" && age < age_low) {
    sample(means, 1, prob = c(.1, .1, .1, .2, .2, .3))
  } else if (married == "s" && gender == "f" && age < age_low) {
    sample(means, 1, prob = c(.0, .1, .1, .2, .3, .3))
  } else {
    sample(means, 1, prob = c(.2, .2, .2, .2, .1, .1))
  }

  round(abs(rnorm(1, base, base / 10)), 0)
}
