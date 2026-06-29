#' Great-circle distance from the ballpark
#'
#' Computes the approximate distance, in miles, from a fixed reference point
#' (the franchise's ballpark, near Nashville at 36.1613 N, 86.7786 W) to a
#' supplied latitude/longitude, using the haversine formula. It is the building
#' block [f_get_distance()] uses to turn simulated customer coordinates into a
#' distance-from-venue feature.
#'
#' @param newLat Latitude of the point of interest, in decimal degrees.
#' @param newLon Longitude of the point of interest, in decimal degrees.
#'
#' @return A numeric distance in miles, rounded to two decimals. Returns `0`
#'   when the point coincides with the reference location.
#'
#' @details
#' The earth's radius is approximated as 3958 miles. The `NV` suffix refers to
#' the reference city (Nashville); it is a teaching approximation rather than a
#' geodesy-grade calculation.
#'
#' @examples
#' # The reference point itself is zero miles away.
#' f_get_miles_NV(36.1613, -86.7786)
#' # A point roughly 70 miles south-east.
#' f_get_miles_NV(35.5, -86.0)
#'
#' @family utilities
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_get_miles_NV.R>
#' @export
f_get_miles_NV <- function(newLat, newLon) {
  deg2rad <- pi / 180
  radius  <- 3958 # rough radius of the earth in miles

  lat1 <- 36.1613  * deg2rad
  lon1 <- -86.7786 * deg2rad
  lat2 <- newLat * deg2rad
  lon2 <- newLon * deg2rad

  d_lat <- lat2 - lat1
  d_lon <- lon2 - lon1

  a <- sin(d_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(d_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))

  round(radius * c, 2)
}
