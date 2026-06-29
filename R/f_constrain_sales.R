#' Cap ticket sales at stadium capacity
#'
#' Clamps a ticket-sales figure to the venue's capacity of 45,000 seats, so
#' simulated demand never exceeds the number of seats available.
#'
#' @param sales A numeric ticket-sales figure.
#'
#' @return `sales` unchanged when it is at or below 45,000, otherwise 45,000.
#'
#' @examples
#' f_constrain_sales(38000)
#' f_constrain_sales(51000)
#'
#' @family season_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_constrain_sales.R>
#' @export
f_constrain_sales <- function(sales) {
  if (sales > 45000) 45000 else sales
}
