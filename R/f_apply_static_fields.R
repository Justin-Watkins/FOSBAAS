#' Derive calendar-based schedule fields
#'
#' From a vector of game dates, derives the deterministic ("static") schedule
#' columns that depend only on the calendar: game number, day of week, month,
#' weekend flag, school-in-session flag, opening-day flag and season year.
#'
#' @param date A vector of dates (or ISO date strings) for the season's games.
#'
#' @return A named list with seven elements, suitable for assigning straight
#'   into a season frame:
#'   \describe{
#'     \item{`gameNumber`}{integer game order, `1:81`;}
#'     \item{`dayOfWeek`}{abbreviated weekday name, e.g. `"Sun"`;}
#'     \item{`month`}{abbreviated month name, e.g. `"Mar"`;}
#'     \item{`weekEnd`}{`TRUE` on Friday/Saturday;}
#'     \item{`schoolInOut`}{`TRUE` in June/July (school out);}
#'     \item{`openingDay`}{`TRUE` for game 1;}
#'     \item{`season`}{the four-digit season year.}
#'   }
#'
#' @examples
#' dates <- unlist(f_create_dates(seed = 309, season_year = 2024))
#' fields <- f_apply_static_fields(dates)
#' fields$dayOfWeek[1:5]
#'
#' @family season_simulation
#' @importFrom lubridate wday month year
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_apply_static_fields.R>
#' @export
f_apply_static_fields <- function(date) {
  date <- as.Date(date)

  game_number <- seq_len(81)
  day_of_week <- as.character(lubridate::wday(date, label = TRUE))
  month_name  <- as.character(lubridate::month(date, label = TRUE))

  list(
    gameNumber  = game_number,
    dayOfWeek   = day_of_week,
    month       = month_name,
    weekEnd     = day_of_week %in% c("Fri", "Sat"),
    schoolInOut = month_name %in% c("Jun", "Jul"),
    openingDay  = game_number == 1,
    season      = lubridate::year(date[1])
  )
}
