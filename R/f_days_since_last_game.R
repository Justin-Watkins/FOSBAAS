#' Days since the previous home game
#'
#' For an ordered vector of game dates, returns the gap in days between each game
#' and the one before it. This "rest" gap is one of the drivers of attendance in
#' [f_attend_coefficient()].
#'
#' @param dates An ordered vector of dates (or ISO date strings).
#'
#' @return An integer vector the same length as `dates`. The first element is
#'   `50` (a sentinel standing in for the long off-season before opening day);
#'   each later element is the number of days since the previous game.
#'
#' @examples
#' dates <- unlist(f_create_dates(seed = 309, season_year = 2024))
#' head(f_days_since_last_game(dates))
#'
#' @family season_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_days_since_last_game.R>
#' @export
f_days_since_last_game <- function(dates) {
  dates <- as.Date(dates)
  gaps <- c(50L, as.integer(diff(dates)))
  gaps
}
