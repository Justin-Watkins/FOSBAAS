#' Simulate a home-game schedule of dates
#'
#' Builds a plausible sequence of 81 home-game dates for a single season by
#' stringing together home stands (series of 3, 6 or 9 games) separated by road
#' trips (gaps of 3, 7 or 10 days), all drawn at random between late March and
#' early October.
#'
#' @param seed Integer seed; makes the schedule reproducible.
#' @param season_year Four-digit year (numeric or character) used to anchor the
#'   calendar, e.g. `2024`.
#'
#' @return A list of 81 character dates (`"YYYY-MM-DD"`), in chronological order.
#'
#' @details
#' Home-stand lengths are sampled from `c(3, 6, 9)` and off-day gaps from
#' `c(3, 7, 10)`, each with fixed probabilities. The season always opens on the
#' first eligible date (March 27 of `season_year`).
#'
#' @examples
#' dates <- f_create_dates(seed = 309, season_year = 2024)
#' length(dates)
#' unlist(dates[1:5])
#'
#' @family season_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_create_dates.R>
#' @export
f_create_dates <- function(seed, season_year) {
  set.seed(seed)

  pattern  <- c(3, 6, 9)
  days_off <- c(3, 7, 10)

  dates <- seq(as.Date(paste0(season_year, "-03-27")),
               as.Date(paste0(season_year, "-10-08")),
               by = "+1 day")

  season_dates <- list()
  x <- 1
  while (x <= 81) {
    num_games_series <- sample(pattern, 1, prob = c(.30, .60, .10))

    if (x == 1) {
      series_dates <- dates[x:num_games_series]
      season_dates[x:num_games_series] <- as.character(series_dates)
    } else {
      day_off_select <- sample(days_off, 1, prob = c(.05, .35, .60))
      date_index     <- day_off_select +
        which(dates == as.character(season_dates[(x - 1)]))[1]
      series_dates   <- dates[date_index:(date_index + num_games_series - 1)]
      season_dates[x:(x + num_games_series - 1)] <- as.character(series_dates)
    }
    x <- x + num_games_series
  }
  season_dates[1:81]
}
