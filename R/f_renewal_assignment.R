#' Assign renewal outcomes to season-ticket accounts
#'
#' Adds a simulated `renewed` flag to a season-ticket-holder table. Accounts are
#' clustered on ticket usage and distance with k-means, the two cluster
#' memberships are combined into a loyalty score, and that score drives a
#' renew/not-renew draw via [f_assign_renewal()].
#'
#' @param seed Integer seed; makes the clustering and renewal draws reproducible.
#' @param sth_data A data frame of season-ticket holders containing at least
#'   `accountID`, `ticketUsage` and `distance` (as produced inside
#'   [f_create_lead_scoring_data()]).
#' @param f_assign_renewal The renewal-draw function to apply to each loyalty
#'   score; normally [f_assign_renewal()].
#'
#' @return `sth_data` with a `renewed` column added (`"r"`/`"nr"`), returned with
#'   the columns `accountID`, `corporate`, `season`, `planType`, `ticketUsage`,
#'   `tenure`, `spend`, `tickets`, `distance`, `renewed`.
#'
#' @section Bug fix:
#' The renewal score now uses the combined loyalty score (usage cluster plus
#' distance cluster, range 2-10). The old code indexed a single cluster column
#' (range 1-5), so the renewal probabilities for scores 6-10 were never reached.
#'
#' @details
#' Usage clusters are ordered low-to-high and distance clusters high-to-low, so a
#' high combined score means a heavy user who lives close to the venue -- the
#' profile most likely to renew.
#'
#' @family lead_scoring
#' @importFrom stats kmeans
#' @importFrom dplyr left_join select
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_renewal_assignment.R>
#' @export
f_renewal_assignment <- function(seed, sth_data, f_assign_renewal) {

  ids <- data.frame(accountID = sth_data$accountID, stringsAsFactors = FALSE)

  set.seed(seed)
  centers1 <- sort(kmeans(sth_data$ticketUsage, centers = 5)$centers)
  ids$clusterTU <- kmeans(sth_data$ticketUsage, centers = centers1)$cluster

  set.seed(seed)
  centers2 <- rev(sort(kmeans(sth_data$distance, centers = 5)$centers))
  ids$clusterDI <- kmeans(sth_data$distance, centers = centers2)$cluster

  ids$clustSum <- ids$clusterTU + ids$clusterDI
  sth_data_renew <- dplyr::left_join(ids, sth_data, by = "accountID")

  renew <- c("r", "nr")
  set.seed(seed)
  sth_data_renew$renewed <- vapply(sth_data_renew$clustSum,
                                   function(score) f_assign_renewal(score, renew),
                                   character(1))

  dplyr::select(sth_data_renew, "accountID", "corporate", "season", "planType",
                "ticketUsage", "tenure", "spend", "tickets", "distance",
                "renewed")
}
