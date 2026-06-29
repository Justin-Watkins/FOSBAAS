#' Generate a sequence of clock times by the minute
#'
#' Produces every minute between two whole hours as zero-padded `"HH:MM"`
#' strings, e.g. `"17:00"`, `"17:01"`, ... It is used to give the simulated gate
#' [scan_data] a realistic, evenly spaced time axis.
#'
#' @param start First hour to include, an integer in `0:23`.
#' @param end Last hour to include, an integer in `0:23` and `>= start`.
#'
#' @return A character vector of `(end - start + 1) * 60` time labels in
#'   `"HH:MM"` format, ordered from `start:00` to `end:59`.
#'
#' @details
#' Hours and minutes are zero-padded with [f_add_zero()] so labels sort
#' correctly as text.
#'
#' @examples
#' head(f_get_time_observations(17, 21))
#' length(f_get_time_observations(17, 21))
#'
#' @family utilities
#' @seealso [f_add_zero()]
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_time_observations.R>
#' @export
f_get_time_observations <- function(start, end) {
  n_hours <- end - start + 1
  hours   <- sort(rep(seq(start, end), 60))
  minutes <- rep(seq(0, 59), n_hours)

  hours_format   <- vapply(hours, f_add_zero, character(1))
  minutes_format <- vapply(minutes, f_add_zero, character(1))

  paste(hours_format, minutes_format, sep = ":")
}
