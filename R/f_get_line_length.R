#' Simulate concession-line lengths
#'
#' Draws a bimodal distribution of line lengths -- a busy mode and a quiet mode
#' -- of the kind observed at a concession stand over the course of a game.
#'
#' @param seed Integer seed; makes the lengths reproducible.
#' @param n Number of observations to return; defaults to `300`.
#' @param u1,sd1 Mean and standard deviation of the busy (longer-line) mode.
#' @param u2,sd2 Mean and standard deviation of the quiet (shorter-line) mode.
#'
#' @return A data frame with `n` rows and the columns `observation` (`1:n`) and
#'   `lineLength` (non-negative whole numbers).
#'
#' @section Bug fix:
#' The function now returns the bimodal sample it draws. The previous version
#' computed that sample, discarded it, and returned an unrelated fixed pattern,
#' leaving the `u1`/`sd1`/`u2`/`sd2` arguments with no effect.
#'
#' @details
#' About 60% of observations come from the first mode and 40% from the second.
#'
#' @examples
#' lines <- f_get_line_length(seed = 755, n = 300, u1 = 20, sd1 = 10,
#'                            u2 = 30, sd2 = 5)
#' head(lines)
#'
#' @family operations
#' @importFrom stats rnorm
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_line_length.R>
#' @export
f_get_line_length <- function(seed, n = 300, u1, sd1, u2, sd2) {
  set.seed(seed)
  group_1 <- abs(rnorm(n, u1, sd1))
  group_2 <- abs(rnorm(n, u2, sd2))
  line <- sample(round(c(sample(group_1, n * .6),
                         sample(group_2, n * .4)), 0), n, replace = FALSE)
  data.frame(observation = seq_len(n), lineLength = line)
}
