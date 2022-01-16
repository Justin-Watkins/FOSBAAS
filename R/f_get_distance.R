#' @title f_get_distance
#' @description create locations for imaginary customers
#' @source \url{GIT}
#' @export



f_get_distance <- function(seed,n = 200000,f_get_miles_NV){

  # set.seed(305)
  # n     <- 200000
  perc  <- c(.55,.05,.05,.05,.1,.2)
  sd    <- c(.5,1,1,1,1.2,.5)
  draws <- n * perc
  lats  <- c(36.1612,33.7490,34.7304,35.5951,35.0456,35.9606 )
  lngs  <- c(-86.7785,-84.3880,-86.5861,-82.5515,-85.3097,-83.9207 )

  latitude  <- list()
  longitude <- list()

  x <- 1
  while(x <= length(perc)){

    latitude[[x]]  <- rnorm(draws[x], lats[x], sd[x])
    longitude[[x]] <- rnorm(draws[x], lngs[x], sd[x])

    x <- x + 1
  }

  distances <- list()

  distances[[1]] <- unlist(latitude)
  distances[[2]] <- unlist(longitude)
  distances[[3]] <- unlist(mapply(f_get_miles_NV,latitude,longitude))

  return(distances)

}
