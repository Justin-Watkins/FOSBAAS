#' Build a frequency table with probabilities
#'
#' Tabulates a numeric vector and returns the count and relative frequency
#' (probability) of each distinct value. It is used in the operations chapter to
#' turn raw observation counts into an empirical probability distribution.
#'
#' @param variable A vector whose unique values can be coerced to numbers
#'   (numeric, or a factor/character of numeric labels).
#'
#' @return A data frame with one row per distinct value and the columns:
#'   \describe{
#'     \item{`variable`}{the distinct value, as a number;}
#'     \item{`Freq`}{the number of times it occurs;}
#'     \item{`prob`}{its relative frequency (`Freq` divided by the total),
#'       so the column sums to 1.}
#'   }
#'
#' @examples
#' set.seed(1)
#' f_build_freq_table(sample(0:5, size = 100, replace = TRUE))
#'
#' @family utilities
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_build_freq_table.R>
#' @export
f_build_freq_table <- function(variable) {
  pr          <- as.data.frame(table(variable), stringsAsFactors = FALSE)
  pr$prob     <- pr$Freq / sum(pr$Freq)
  pr$variable <- as.numeric(as.character(pr$variable))
  pr
}
