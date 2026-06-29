#' Left-pad a single-digit number with a zero
#'
#' Formats a number as a two-character string, prefixing a leading `"0"` when
#' the value is below 10. Numbers of 10 or greater are returned unchanged. This
#' is a small presentation helper used to keep clock-style labels (hours,
#' minutes) aligned on plots and in tables.
#'
#' @param number A single numeric value.
#'
#' @return A length-one character string. Values below 10 gain a leading zero
#'   (e.g. `2` becomes `"02"`); values of 10 or more are returned as-is
#'   (e.g. `11` becomes `"11"`).
#'
#' @examples
#' f_add_zero(2)
#' f_add_zero(11)
#'
#' @family utilities
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_add_zero.R>
#' @export
f_add_zero <- function(number) {
  if (number < 10) {
    paste0("0", as.character(number))
  } else {
    as.character(number)
  }
}
