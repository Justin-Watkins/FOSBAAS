#' Simulate a full season of home games
#'
#' Assembles a complete season schedule with simulated ticket sales by combining
#' the season-simulation helpers: it draws dates, assigns opponents, derives the
#' calendar fields, scatters promotions, computes a per-game attendance
#' multiplier and applies it to a baseline drawn from a normal distribution. The
#' result has the same shape as the bundled [season_data].
#'
#' @param seed1 Seed for the schedule of dates ([f_create_dates()]).
#' @param season_year Four-digit season year, e.g. `2024`.
#' @param seed2 Seed for opponent assignment ([f_select_team_for_series()]).
#'   Note that this seed determines *which* teams appear in the season.
#' @param num_games Number of home games; defaults to `81` (the season frame is
#'   built for 81 games).
#' @param seed3 Seed for promotion assignment ([f_assign_promotion()]).
#' @param num_bbh Number of bobblehead-giveaway games.
#' @param num_con Number of concert games.
#' @param num_oth Number of other promotional games.
#' @param seed4 Seed for the attendance multipliers ([f_attend_coefficient()]).
#' @param seed5 Seed for the baseline ticket-sales draw.
#' @param mean_sales Mean of the baseline per-game ticket-sales distribution.
#' @param sd_sales Standard deviation of that distribution.
#'
#' @return A data frame of `num_games` rows with the columns documented in
#'   [season_data]: `gameNumber`, `team`, `date` (Date), `dayOfWeek`, `month`,
#'   `weekEnd`, `schoolInOut`, `daysSinceLastGame`, `openingDay`, `promotion`,
#'   `ticketSales` (capped at 45,000 by [f_constrain_sales()]) and `season`.
#'
#' @section Bug fixes:
#' `dayOfWeek` and `month` are now returned as readable labels (e.g. `"Sun"`,
#' `"Mar"`) instead of the integer factor codes the old `unlist()` produced, and
#' `date` is a proper `Date` column.
#'
#' @examples
#' season <- f_build_season(seed1 = 3000, season_year = 2024, seed2 = 714,
#'                          num_games = 81, seed3 = 366, num_bbh = 5,
#'                          num_con = 3, num_oth = 5, seed4 = 309, seed5 = 25,
#'                          mean_sales = 29000, sd_sales = 3500)
#' head(season)
#'
#' @family season_simulation
#' @importFrom stats rnorm
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_build_season.R>
#' @export
f_build_season <- function(seed1, season_year, seed2, num_games = 81,
                           seed3, num_bbh, num_con, num_oth, seed4,
                           seed5, mean_sales, sd_sales) {

  season      <- f_create_season_frame()
  season$date <- as.Date(unlist(f_create_dates(seed1, season_year)))
  season$team <- unlist(f_select_team_for_series(seed2, num_games))

  fields <- f_apply_static_fields(season$date)
  season$gameNumber  <- fields$gameNumber
  season$dayOfWeek   <- fields$dayOfWeek
  season$month       <- fields$month
  season$weekEnd     <- fields$weekEnd
  season$schoolInOut <- fields$schoolInOut
  season$openingDay  <- fields$openingDay
  season$season      <- fields$season

  season$promotion <- f_assign_promotion(num_bbh, num_con, num_oth,
                                          num_games, seed3)
  season$daysSinceLastGame <- f_days_since_last_game(season$date)

  set.seed(seed4)
  coefficient_list <- with(season,
                           mapply(f_attend_coefficient, team, weekEnd,
                                  schoolInOut, daysSinceLastGame, openingDay,
                                  promotion))

  set.seed(seed5)
  season$ticketSales <- round(rnorm(nrow(season), mean = mean_sales,
                                    sd = sd_sales), 0)
  season$ticketSales <- round(season$ticketSales * coefficient_list, 0)
  season$ticketSales <- vapply(season$ticketSales, f_constrain_sales, numeric(1))

  season
}
