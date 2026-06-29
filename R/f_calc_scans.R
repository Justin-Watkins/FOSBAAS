#' Parabolic scan-count curve
#'
#' Evaluates the parabola used to shape the gate [scan_data]: a curve that rises
#' to a peak and falls again, modelling the rush of fans arriving before a game.
#'
#' @param x Location of the parabola's far root (controls its width); the curve
#'   is scaled so that it has roots near `0` and near `x`.
#' @param y Peak height of the curve.
#' @param j The point (e.g. minute index) at which to evaluate the curve.
#'
#' @return A numeric scan count at point `j`.
#'
#' @details
#' Internally `a = y / (x^2 - 300x + 300)` scales the parabola
#' `a * (j^2 - 301j + 300)`.
#'
#' @examples
#' f_calc_scans(x = 230, y = 110, j = 150)
#'
#' @family operations
#' @seealso [f_get_scan_data()]
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_calc_scans.R>
#' @export
f_calc_scans <- function(x, y, j) {
  a <- y / (x^2 - 300 * x + 300)
  a * (j^2 - 301 * j + 300)
}
