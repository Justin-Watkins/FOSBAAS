#' Simulate whether one customer has children
#'
#' Draws a yes/no children flag for a single customer, with probabilities that
#' depend on marital status and age. Designed to be called row-wise when
#' building [demographic_data].
#'
#' @param married Marital status, `"m"` (married) or `"s"` (single).
#' @param age Customer age in years.
#'
#' @return A single character value: `"y"` (has children) or `"n"`.
#'
#' @section Bug fix:
#' The unused `seed` argument was removed; set the seed once in the caller.
#'
#' @examples
#' set.seed(600)
#' f_demographics_children(married = "m", age = 38)
#'
#' @family demographic_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_demographics_children.R>
#' @export
f_demographics_children <- function(married, age) {
  children <- c("y", "n")

  if (married == "m" && age <= 40) {
    sample(children, 1, prob = c(.60, .40))
  } else if (married == "s" && age < 40) {
    sample(children, 1, prob = c(.15, .85))
  } else if (married == "m" && age > 40) {
    sample(children, 1, prob = c(.70, .30))
  } else if (married == "s" && age > 40) {
    sample(children, 1, prob = c(.10, .90))
  } else {
    sample(children, 1, prob = c(.50, .50))
  }
}
