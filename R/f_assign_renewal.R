#' Draw a renewal outcome from a loyalty score
#'
#' Maps an integer loyalty score to a renew/not-renew outcome: the higher the
#' score, the more likely the account renews. Used inside
#' [f_renewal_assignment()] to turn clustering output into a renewal label.
#'
#' @param x Integer loyalty score (typically the sum of two 1-5 cluster
#'   memberships, so 2-10). Values outside 1-10 fall back to a coin flip.
#' @param renew Length-two vector of outcome labels, `c(renew, not_renew)` (the
#'   book uses `c("r", "nr")`).
#'
#' @return One element of `renew`, drawn with a probability that increases with
#'   `x`.
#'
#' @details
#' Set the seed once in the caller for reproducibility. The renewal probability
#' rises from about 1% at `x = 2` to roughly 99% at `x = 10`.
#'
#' @examples
#' set.seed(1)
#' f_assign_renewal(10, c("r", "nr"))
#' f_assign_renewal(2, c("r", "nr"))
#'
#' @family lead_scoring
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_assign_renewal.R>
#' @export
f_assign_renewal <- function(x, renew) {
  prob <- switch(
    as.character(x),
    "10" = c(.99, .01),
    "9"  = c(.98, .02),
    "8"  = c(.95, .05),
    "7"  = c(.95, .05),
    "6"  = c(.92, .08),
    "5"  = c(.90, .10),
    "4"  = c(.85, .15),
    "3"  = c(.80, .20),
    "2"  = c(.30, .70),
    "1"  = c(.25, .75),
    c(.5, .5)
  )
  sample(renew, 1, prob = prob)
}
