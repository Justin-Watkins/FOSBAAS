#' Evaluate a fitted cubic polynomial
#'
#' Evaluates a third-degree polynomial at a new point using the first four
#' coefficients of a fitted model. It is used to read values off a cubic curve
#' fitted to an empirical distribution (see [f_simulate_distribution()]).
#'
#' @param new_var The value at which to evaluate the polynomial.
#' @param dist_fit A fitted model (e.g. from [stats::lm()]) whose first four
#'   coefficients are the intercept and the linear, quadratic and cubic terms.
#'
#' @return The numeric value of the cubic at `new_var`.
#'
#' @examples
#' x <- 1:50
#' y <- 2 + 0.5 * x - 0.01 * x^2 + 0.0001 * x^3
#' fit <- lm(y ~ x + I(x^2) + I(x^3))
#' f_get_third_degree_fit(new_var = 25, dist_fit = fit)
#'
#' @family operations
#' @importFrom stats coef
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_third_degree_fit.R>
#' @export
f_get_third_degree_fit <- function(new_var, dist_fit) {
  co <- coef(dist_fit)
  co[1] + co[2] * new_var + co[3] * new_var^2 + co[4] * new_var^3
}
