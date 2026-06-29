#' Build an attendance multiplier for a single game
#'
#' Returns a multiplier, centred near 1, that scales baseline ticket sales up or
#' down for one game according to how attractive it is: marquee opponents,
#' weekends, summer (school out), long rest, opening day and promotions all add
#' demand. [f_build_season()] applies this multiplier to every game.
#'
#' @param opponent Three-letter opponent abbreviation (see
#'   [f_select_team_for_series()]).
#' @param week_end Logical; is the game on a Friday or Saturday?
#' @param school_out Logical; is school out (June/July)?
#' @param lag Days since the previous home game (see [f_days_since_last_game()]).
#' @param opening_day Logical; is this opening day?
#' @param promotion Promotion label: `"concert"`, `"bobblehead"`, `"other"` or
#'   `"none"`.
#'
#' @return A single numeric multiplier (1 plus the sum of the demand boosts).
#'
#' @details
#' Each contribution is a small normal draw, so the caller must set the seed
#' before invoking this function (for example inside a `mapply()` over a
#' season). Opponents are tiered: `BOS/CHC/NYY/LAD/STL` add the most, then
#' `LAA/HOU/NYM/SF/PHI`, while all other clubs add nothing.
#'
#' @section Bug fixes:
#' Removed the per-row `set.seed()` (seeding inside a row-wise call collapsed the
#' randomness), the no-op `mod == mod + 0` branch, and the misspelt `"BAl"`
#' opponent code.
#'
#' @examples
#' set.seed(309)
#' f_attend_coefficient("NYY", week_end = TRUE, school_out = TRUE,
#'                      lag = 3, opening_day = FALSE, promotion = "bobblehead")
#'
#' @family season_simulation
#' @importFrom stats rnorm
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_attend_coefficient.R>
#' @export
f_attend_coefficient <- function(opponent, week_end, school_out, lag,
                                 opening_day, promotion) {
  mod <- 1

  if (opponent %in% c("BOS", "CHC", "NYY", "LAD", "STL")) {
    mod <- mod + rnorm(1, .5, .05)
  } else if (opponent %in% c("LAA", "HOU", "NYM", "SF", "PHI")) {
    mod <- mod + rnorm(1, .25, .05)
  }

  if (isTRUE(week_end))    mod <- mod + rnorm(1, .4, .05)
  if (isTRUE(school_out))  mod <- mod + rnorm(1, .2, .05)
  if (lag > 1)             mod <- mod + rnorm(1, .1, .02)
  if (isTRUE(opening_day)) mod <- mod + rnorm(1, .7, .02)

  if (promotion == "concert") {
    mod <- mod + rnorm(1, .15, .02)
  } else if (promotion == "bobblehead") {
    mod <- mod + rnorm(1, .25, .02)
  } else if (promotion == "other") {
    mod <- mod + rnorm(1, .05, .02)
  }

  mod
}
