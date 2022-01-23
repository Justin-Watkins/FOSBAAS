#-----------------------------------------------------------------
# secondary_data data set
#
# This data set approximates sales on a secondary market
# You need the ticket activity data to create the sales file
#-----------------------------------------------------------------
setwd("C:/package/FOSBAAS/ticket_activity")
library(readr)
#-----------------------------------------------------------------
# Read and write to file
#-----------------------------------------------------------------
## Not run:
if (FALSE){
  files          <- list.files(path = "C:/package/FOSBAAS/ticket_activity",
                               pattern = "\\.csv$")

  ticketActivity <- do.call("rbind", lapply(files, function(x){
    readr::read_csv(x)
  }))

 # readr::write_csv(ticketActivity,'ticket_sales_2022_2023_2024.csv')

}
## End(**Not run**)
ticket_sales_2022_2023_2024 <- ticketActivity
#-----------------------------------------------------------------
# Read Data
#-----------------------------------------------------------------
setwd("C:/package/FOSBAAS/data-raw")
dataPaths   <- list.files(path       = "C:/package/FOSBAAS/data-raw",
                          pattern    = "\\.csv$",
                          full.names = TRUE)
data        <- lapply(dataPaths,readr::read_csv)
names(data) <- gsub(".csv","",
                    list.files("C:/package/FOSBAAS/data-raw",
                               full.names = FALSE),
                    fixed      = TRUE)

data$ticket_sales_2022_2023_2024 <- ticket_sales_2022_2023_2024

#-----------------------------------------------------------------
# Append tickets and pricing to ticket sales
library(dplyr)
data$ticket_sales_2022_2023_2024 <-
  data$ticket_sales_2022_2023_2024 %>%
  mutate(tickets = 1,priceKey = paste(seatID,ticketType,sep = "_"))

price_data_se <- data$manifest_data %>% select(seatID,seasonPrice) %>%
  tidyr::gather(key = seatID, value = seasonPrice) %>%
  mutate(priceKey = paste(seatID,'se',sep="_")) %>%
  rename(price = seasonPrice)

price_data_si <- data$manifest_data %>% select(seatID,singlePrice) %>%
  tidyr::gather(key = seatID, value = singlePrice) %>%
  mutate(priceKey = paste(seatID,'si',sep="_")) %>%
  rename(price = singlePrice)

price_data_gr <- data$manifest_data %>% select(seatID,groupPrice) %>%
  tidyr::gather(key = seatID, value = groupPrice) %>%
  mutate(priceKey = paste(seatID,'gr',sep="_")) %>%
  rename(price = groupPrice)

price_data    <- bind_rows(price_data_se,price_data_si,price_data_gr)

price_data <- price_data %>% select(priceKey,price)
#-----------------------------------------------------------------
# Append tickets and pricing to ticket sales

data$ticket_sales_2022_2023_2024 <- data$ticket_sales_2022_2023_2024 %>%
  left_join(price_data,by='priceKey')

#-----------------------------------------------------------------
# Create Resale data
#-----------------------------------------------------------------

data$season_data$gameCluster <- kmeans(scale(data$season_data$ticketSales),centers = 10)$cluster


clusMatrix <- as.data.frame(as.table(by(data$season_data$ticketSales,
                                        data$season_data$gameCluster,
                                        function(x) mean(x))))

clusMatrixSort <- clusMatrix[order(clusMatrix[,2]),]

clusMatrixSort$orderedCluster <- seq(1:nrow(clusMatrixSort))
names(clusMatrixSort)         <- c('gameCluster','avg','orderedCluster')
clusMatrixSort$gameCluster    <- as.numeric(as.character(clusMatrixSort$gameCluster))


clusMatrixSort   <- dplyr::select(clusMatrixSort,gameCluster,orderedCluster)
data$season_data <- data$season_data %>% left_join(clusMatrixSort,by = 'gameCluster')
View(data$season_data)

# Create data frame to aid in building pricing coefficient
# The coefficient should work on a distribution related to how
# attractive the game is based on attendance (it really works in the converse)

# Sample 12% of tickets to be sold on the secondary market.
set.seed(6)
data$secondary_sales    <- dplyr::sample_frac(data$ticket_sales_2022_2023_2024,.12)

# Join Game cluster
data$season_data$gameID <- data$season_data$gameNumber
clusterKeys             <- dplyr::select(data$season_data,gameID,orderedCluster)

data$secondary_sales    <- data$secondary_sales %>% dplyr::left_join(clusterKeys,by='gameID')
coefficientMeans        <- seq(from = .6, to = 1.5, by = .1)
sd                      <- .2

set.seed(2)
data$secondary_sales$secondayrPrice <-
  with(data$secondary_sales,
       mapply(function(od,pr) round(rnorm(1,coefficientMeans[od],sd) * pr,2), orderedCluster,price))

#readr::write_csv(data$secondary_sales,"secondary_sales_data.csv")
