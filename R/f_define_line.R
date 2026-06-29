#' Evaluate a straight line defined by two points
#'
#' Given two points `(x1, y1)` and `(x2, y2)`, this computes the slope and
#' intercept of the line through them and returns the height of that line at an
#' arbitrary `x`. It is used in the book to illustrate the point-slope form of a
#' linear equation.
#'
#' @param y2 The `y` coordinate of the second point.
#' @param y1 The `y` coordinate of the first point.
#' @param x2 The `x` coordinate of the second point.
#' @param x1 The `x` coordinate of the first point.
#' @param x The `x` value at which to evaluate the line.
#'
#' @return A numeric value: the height of the line at `x`.
#'
#' @details
#' The slope is `m = (y2 - y1) / (x2 - x1)`, the intercept is `b = y2 - m * x2`,
#' and the returned value is `m * x + b`.
#'
#' @examples
#' f_define_line(y2 = 10, y1 = 0, x2 = 10, x1 = 0, x = 2)
#'
#' @family utilities
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_define_line.R>
#' @export
f_define_line <- function(y2, y1, x2, x1, x) {
  m <- (y2 - y1) / (x2 - x1)
  b <- y2 - (m * x2)
  m * x + b
}
