

#' @title f_get_miles_NV
#' @description Get miles based on latitude and longitude
#' @source \url{GIT}
#' @export

f_get_miles_NV <- function(newLat,newLon){
  # Convert to Radians
  lat2 <- newLat * (pi/180)
  lon2 <- newLon * (pi/180)
  # Rough radius of the earth at our latitude
  R = 3958
  # convert starting points to radians
  startLat <- 36.1613  * (pi/180)
  startLon <- -86.7786 * (pi/180)
  # Get the distance between points in radians
  dLon = lat2 - startLat
  dLat = lon2 - startLon
  # Apply Haversine formula
  a <- (sin(dLat/2))^2 + cos(startLat) * (sin(dLon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R*c
  return(round(d,2))
}
