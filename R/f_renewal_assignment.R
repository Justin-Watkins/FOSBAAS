#' @title f_renewal_assignment
#' @description Determine whether the tickets will be renewed
#' @source \url{GIT}
#' @export
f_renewal_assignment <- function(seed,sth_data,f_assign_renewal){

  require(dplyr)

  ids <- as.data.frame(sth_data$accountID)
  names(ids) <- "accountID"

  set.seed(seed)
  centers1 <- kmeans(sth_data$ticketUsage, centers = 5)$centers
  centers1 <- sort(centers1)
  ids$clusterTU <- kmeans(sth_data$ticketUsage, centers = centers1)$cluster

  set.seed(seed)
  centers2 <- kmeans(sth_data$distance, centers = 5)$centers
  centers2 <- rev(sort(centers2))
  ids$clusterDI <- kmeans(sth_data$distance, centers = centers2)$cluster

  ids$clustSum <- ids$clusterTU + ids$clusterDI
  sth_data_renew <- dplyr::left_join(ids,sth_data, by = "accountID")

  x <- 1
  renew <- c("r","nr")
  a_renew <- list()
  while(x <= nrow(sth_data_renew)){
    clust <- sth_data_renew[x,3]
    a_renew[x] <- f_assign_renewal(clust,renew)
    x <- x + 1
  }

  sth_data_renew$renewed <- unlist(a_renew)
  sth_data_renew <- dplyr::select(sth_data_renew,accountID,corporate,season,planType,
                                  ticketUsage,tenure,spend,tickets,distance,
                                  renewed)
  return(sth_data_renew)

} # End
