#' Build a season-ticket-holder lead-scoring data set
#'
#' Generates a synthetic table of season-ticket accounts with the features used
#' for renewal/lead-scoring models: account type, plan type, distance, tickets,
#' tenure, spend and ticket usage, optionally with a simulated renewal outcome.
#' It mirrors the bundled [customer_renewals] data.
#'
#' @param seed Integer seed; makes the whole table reproducible.
#' @param num_purchasers Number of accounts to generate.
#' @param season Season year to stamp on every row, e.g. `2023`.
#' @param f_tenure Function that draws tenure; normally [f_calculate_tenure()].
#' @param f_spend Function that draws spend; normally [f_calculate_spend()].
#' @param f_use Function that draws ticket usage; normally
#'   [f_calculate_ticket_use()].
#' @param f_renewal_assignment Function that assigns renewals; normally
#'   [f_renewal_assignment()].
#' @param f_assign_renewal Function that draws a single renewal outcome; normally
#'   [f_assign_renewal()].
#' @param renew Logical; if `TRUE` (default) tenure is simulated and a `renewed`
#'   column is added, otherwise tenure is `0` and no renewal is assigned.
#'
#' @return A data frame of `num_purchasers` accounts. With `renew = TRUE` it has
#'   the ten columns of [customer_renewals]; with `renew = FALSE` it returns the
#'   nine feature columns without `renewed`.
#'
#' @section Bug fix:
#' The corporate plan-type probabilities were `c(.95, .5)` (summing to 1.45);
#' they are now `c(.95, .05)`, matching the "95% full-season" intent in the
#' original comment.
#'
#' @details
#' The simulation injects its helper functions as arguments so the book can swap
#' in alternative behaviours; pass the package functions of the same name for
#' the standard data set.
#'
#' @examples
#' lead <- f_create_lead_scoring_data(
#'   seed = 434, num_purchasers = 250, season = 2023,
#'   f_tenure = f_calculate_tenure, f_spend = f_calculate_spend,
#'   f_use = f_calculate_ticket_use,
#'   f_renewal_assignment = f_renewal_assignment,
#'   f_assign_renewal = f_assign_renewal)
#' head(lead)
#'
#' @family lead_scoring
#' @importFrom stats rexp
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_create_lead_scoring_data.R>
#' @export
f_create_lead_scoring_data <- function(seed, num_purchasers, season,
                                       f_tenure, f_spend, f_use,
                                       f_renewal_assignment, f_assign_renewal,
                                       renew = TRUE) {

  sth_data <- data.frame(matrix(nrow = num_purchasers, ncol = 9))
  names(sth_data) <- c("accountID", "corporate", "season", "planType",
                       "ticketUsage", "tenure", "spend", "tickets", "distance")

  # Account IDs
  set.seed(seed)
  sth_data$accountID <- vapply(seq_len(num_purchasers), function(i)
    paste(sample(c(0:9, LETTERS), 12, replace = TRUE), collapse = ""),
    character(1))

  sth_data$season <- season

  # Account type: ~20% corporate, ~80% individual
  set.seed(seed)
  sth_data$corporate <- sample(c("c", "i"), num_purchasers, replace = TRUE,
                               prob = c(.20, .80))

  is_corp <- sth_data$corporate == "c"
  is_indv <- !is_corp

  # Plan type: corporate skews to full season, individuals more mixed
  set.seed(seed)
  sth_data$planType[is_corp] <- sample(c("f", "p"), sum(is_corp),
                                       replace = TRUE, prob = c(.95, .05))
  sth_data$planType[is_indv] <- sample(c("f", "p"), sum(is_indv),
                                       replace = TRUE, prob = c(.60, .40))

  # Distance: corporate accounts cluster closer than individuals
  set.seed(seed)
  distances_corp <- rexp(num_purchasers) * 12
  distances_indv <- rexp(num_purchasers) * 30
  set.seed(seed)
  sth_data$distance[is_corp] <- sample(distances_corp, sum(is_corp), replace = TRUE)
  sth_data$distance[is_indv] <- sample(distances_indv, sum(is_indv), replace = TRUE)

  # Tickets per account
  tickets <- c(10, 8, 6, 5, 4, 3, 2, 1)
  set.seed(seed)
  sth_data$tickets[is_corp] <- sample(tickets, sum(is_corp), replace = TRUE,
                                      prob = c(.02, .08, .10, .05, .50, .05, .20, 0))
  sth_data$tickets[is_indv] <- sample(tickets, sum(is_indv), replace = TRUE,
                                      prob = c(0, 0, .10, .05, .40, .05, .30, .10))

  # Tenure
  if (renew) {
    avg_dist <- mean(sth_data$distance)
    set.seed(seed)
    sth_data$tenure <- as.vector(with(sth_data,
      mapply(f_tenure, corporate, planType, distance, avg_dist)))
  } else {
    sth_data$tenure <- 0
  }

  # Spend (per ticket, scaled by ticket count)
  avg_tenure <- mean(sth_data$tenure)
  set.seed(seed)
  spend <- as.vector(with(sth_data,
    mapply(f_spend, corporate, planType, tenure, avg_tenure)))
  sth_data$spend <- spend * sth_data$tickets

  # Ticket usage
  avg_dist <- mean(sth_data$distance)
  set.seed(seed)
  sth_data$ticketUsage <- as.vector(with(sth_data,
    mapply(f_use, corporate, distance, avg_dist)))

  if (renew) {
    f_renewal_assignment(seed, sth_data, f_assign_renewal)
  } else {
    sth_data
  }
}
