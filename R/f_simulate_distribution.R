#' Simulate values from an empirical distribution
#'
#' Fits a cubic curve to the empirical frequency distribution of an observed
#' variable and uses it to draw new simulated values. The book uses it to grow a
#' small set of observed wait times into a larger simulated sample.
#'
#' @param observation A numeric vector of observed values to learn the
#'   distribution from.
#' @param rand Number of simulated values to draw.
#' @param seeds Integer seed. The default of `1` is a sentinel meaning "pick a
#'   random seed"; pass any other value for reproducible output.
#'
#' @return A numeric vector of `rand` simulated values.
#'
#' @details
#' The empirical distribution is built with [f_build_freq_table()], a cubic is
#' fitted to it, and values are read off the curve with
#' [f_get_third_degree_fit()] at uniformly random frequencies.
#'
#' @examples
#' set.seed(42)
#' obs <- sample(0:50, size = 500, replace = TRUE)
#' sim <- f_simulate_distribution(observation = obs, rand = 100, seeds = 42)
#' summary(sim)
#'
#' @family operations
#' @seealso [f_build_freq_table()], [f_get_third_degree_fit()]
#' @importFrom stats lm poly predict runif
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_simulate_distribution.R>
#' @export
f_simulate_distribution <- function(observation, rand, seeds = 1) {
  if (seeds == 1) {
    set.seed(round(runif(1) * 100, 0))
  } else {
    set.seed(seeds)
  }

  dist    <- f_build_freq_table(observation)
  max_val <- max(dist$Freq)
  fit     <- lm(dist$variable ~ poly(dist$Freq, 3, raw = TRUE))

  rand_seq <- runif(rand, min = 0, max = max_val)
  vapply(rand_seq, function(x) f_get_third_degree_fit(x, fit), numeric(1))
}
