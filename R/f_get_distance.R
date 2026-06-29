#' Simulate customer locations and distance from the venue
#'
#' Places synthetic customers around six population centres (one of them the
#' home city), then computes each customer's great-circle distance from the
#' ballpark. The result feeds the `latitude`, `longitude` and `distance` columns
#' of [demographic_data].
#'
#' @param seed Integer seed; makes the locations reproducible.
#' @param n Number of customers to place; defaults to `200000`.
#' @param miles_fn Function used to convert a latitude/longitude to miles from
#'   the venue; defaults to [f_get_miles_NV()].
#'
#' @return A list of three numeric vectors, each of length `n`: latitudes,
#'   longitudes and distances (miles) from the venue.
#'
#' @details
#' Roughly 55% of customers cluster tightly around the home city and the rest
#' are spread across five regional centres, giving a realistic mix of local and
#' out-of-town fans.
#'
#' @section Bug fix:
#' The `seed` argument is now used (`set.seed(seed)`); previously the seeding
#' line was commented out, so locations were not reproducible.
#'
#' @examples
#' loc <- f_get_distance(seed = 305, n = 1000)
#' summary(loc[[3]])
#'
#' @family demographic_simulation
#' @seealso [f_get_miles_NV()]
#' @importFrom stats rnorm
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_distance.R>
#' @export
f_get_distance <- function(seed, n = 200000, miles_fn = f_get_miles_NV) {
  set.seed(seed)

  perc <- c(.55, .05, .05, .05, .10, .20)
  sd   <- c(.5,  1,   1,   1,   1.2, .5)
  lats <- c(36.1612, 33.7490, 34.7304, 35.5951, 35.0456, 35.9606)
  lngs <- c(-86.7785, -84.3880, -86.5861, -82.5515, -85.3097, -83.9207)
  draws <- n * perc

  latitude  <- vector("list", length(perc))
  longitude <- vector("list", length(perc))
  for (x in seq_along(perc)) {
    latitude[[x]]  <- rnorm(draws[x], lats[x], sd[x])
    longitude[[x]] <- rnorm(draws[x], lngs[x], sd[x])
  }

  list(
    unlist(latitude),
    unlist(longitude),
    unlist(mapply(miles_fn, latitude, longitude))
  )
}
