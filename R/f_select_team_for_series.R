#' Assign opponents to a season's home games
#'
#' Walks through the games of a season and assigns the same randomly chosen MLB
#' opponent to each game of a home stand (3 or 6 consecutive games), so the
#' resulting schedule looks like a real one in which a visiting team plays a
#' short series.
#'
#' @param seed Integer seed; makes the opponent assignment reproducible.
#' @param number Number of games to assign (81 for a half-season).
#'
#' @return A list of length `number` of three-letter opponent abbreviations.
#'
#' @details
#' Series lengths are sampled from `c(3, 6)` with probabilities `c(.7, .3)`.
#' Opponents are drawn (with replacement across series) from the 30 MLB club
#' abbreviations, so the set of teams that appears depends on `seed`.
#'
#' @examples
#' teams <- f_select_team_for_series(seed = 755, number = 81)
#' length(teams)
#' table(unlist(teams))
#'
#' @family season_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_select_team_for_series.R>
#' @export
f_select_team_for_series <- function(seed, number) {
  mlb_teams <- c("ARI", "ATL", "BAL", "BOS", "CHC", "CWS", "CIN",
                 "CLE", "COL", "DET", "FLA", "HOU", "KAN", "LAA",
                 "LAD", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI",
                 "PIT", "SD", "SF", "SEA", "STL", "TB", "TEX",
                 "TOR", "WAS")
  set.seed(seed)

  opponents <- list()
  series_lengths <- c(3, 6)

  x <- 1
  while (x <= number) {
    series_opponent <- sample(mlb_teams, 1)
    games <- sample(series_lengths, 1, prob = c(.7, .3))
    opponents[x:(x + games - 1)] <- series_opponent
    x <- x + games
  }
  opponents[1:number]
}
