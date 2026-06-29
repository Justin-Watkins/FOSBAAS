#' Randomly assign promotions across a season
#'
#' Spreads a fixed number of bobblehead, concert and "other" promotional games
#' across a season at random, leaving the remaining games with no promotion.
#'
#' @param number_bbhead Number of bobblehead-giveaway games.
#' @param number_concert Number of post-game concert games.
#' @param number_other Number of other promotional games.
#' @param games Total number of games in the season.
#' @param seed Integer seed; makes the assignment reproducible.
#'
#' @return A character vector of length `games`, each element one of
#'   `"bobblehead"`, `"concert"`, `"other"` or `"none"`, in game order.
#'
#' @details
#' The promotion labels are built in the requested counts, padded with `"none"`
#' up to `games`, then shuffled.
#'
#' @section Bug fix:
#' Earlier versions wrote `set.seed <- seed`, which created a local variable
#' instead of seeding the generator, so results were not reproducible. This
#' version calls `set.seed(seed)`.
#'
#' @examples
#' table(f_assign_promotion(number_bbhead = 5, number_concert = 3,
#'                           number_other = 5, games = 81, seed = 366))
#'
#' @family season_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_assign_promotion.R>
#' @export
f_assign_promotion <- function(number_bbhead, number_concert, number_other,
                               games, seed) {
  set.seed(seed)

  bbh <- rep("bobblehead", number_bbhead)
  con <- rep("concert",    number_concert)
  oth <- rep("other",      number_other)
  non <- rep("none", games - sum(number_bbhead, number_concert, number_other))

  sample_list <- c(bbh, con, oth, non)
  sample(sample_list, length(sample_list), replace = FALSE)
}
