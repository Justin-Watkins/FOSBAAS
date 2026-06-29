#' Simulate a bimodal age distribution
#'
#' Draws customer ages from a mixture of two normal distributions, producing the
#' twin-peaked age profile typical of a sports fan base (a younger group and an
#' older group).
#'
#' @param seed Integer seed; makes the ages reproducible.
#' @param num_rows Number of ages to return; defaults to `200000`.
#' @param g1_mean,g1_sd Mean and standard deviation of the first (younger) group.
#' @param g2_mean,g2_sd Mean and standard deviation of the second (older) group.
#'
#' @return A numeric vector of `num_rows` whole-number ages, shuffled so the two
#'   groups are interleaved. The mixture is roughly 40% from group 1 and 60%
#'   from group 2.
#'
#' @section Bug fix:
#' The `seed` argument is now actually used (`set.seed(seed)`); previously it was
#' ignored, so results were not reproducible.
#'
#' @examples
#' ages <- f_get_age(seed = 200, num_rows = 1000,
#'                   g1_mean = 30, g2_mean = 55, g1_sd = 8, g2_sd = 10)
#' summary(ages)
#'
#' @family demographic_simulation
#' @importFrom stats rnorm
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_age.R>
#' @export
f_get_age <- function(seed, num_rows = 200000, g1_mean, g2_mean, g1_sd, g2_sd) {
  set.seed(seed)
  group_1 <- rnorm(num_rows, g1_mean, g1_sd)
  group_2 <- rnorm(num_rows, g2_mean, g2_sd)
  pooled  <- round(c(sample(group_1, num_rows * .4),
                     sample(group_2, num_rows * .6)), 0)
  sample(pooled, num_rows, replace = FALSE)
}
