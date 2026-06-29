#' Create an empty season schedule frame
#'
#' Returns an empty 81-row data frame with the columns [f_build_season()] fills
#' in, one row per home game in a half-season. It exists so the column set and
#' order live in a single place.
#'
#' @return A data frame with 81 rows and 12 `NA`-filled columns: `gameNumber`,
#'   `team`, `date`, `dayOfWeek`, `month`, `weekEnd`, `schoolInOut`,
#'   `daysSinceLastGame`, `openingDay`, `promotion`, `ticketSales`, `season`.
#'
#' @examples
#' str(f_create_season_frame())
#'
#' @family season_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_create_season_frame.R>
#' @export
f_create_season_frame <- function() {
  season_data <- data.frame(matrix(nrow = 81, ncol = 12))
  names(season_data) <- c("gameNumber", "team", "date", "dayOfWeek",
                          "month", "weekEnd", "schoolInOut",
                          "daysSinceLastGame", "openingDay", "promotion",
                          "ticketSales", "season")
  season_data
}
