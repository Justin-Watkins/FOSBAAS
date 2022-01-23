#-----------------------------------------------------------------
# purchase data set
#
# This data set approximates a purchases by class and is used to create other data sets
#
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# BEGIN simulate purchases
#-----------------------------------------------------------------
if (FALSE){
  seasonData           <- read.csv('data-raw/season_data.csv')
  sths                 <- read.csv('data-raw/season_ticket_holders.csv')
  manifestFullComplete <- read.csv('data-raw/manifest_data.csv')
  custData             <- read.csv('data-raw/customer_data.csv')

  sths$ticketType <- 'se'

  set.seed(755)
  seeds <- round(runif(81*3) * 100000,0)

  z <- 1
  while(z <= max(seasonData$gameID)){

    n <- (seasonData[z,]$ticketSales - nrow(sths)) + 125
    set.seed(seeds[z])

    seatsList <- list()
    custList  <- list()
    ttList    <- list()

    x <- 1
    i <- 1

    while(x <= n){
      seatSelect  <- c(1,2,3,4,5,6,7,8,9,10,20,25,30,100)
      blockStart  <- sample(seatSelect,size = 1,
                            prob = c(.1,.25,.05,.39,.03,.02,.01,.01,.01,.01,.08,.01,.02,.01))
      firstSeat   <- sample(manifestFullComplete[which(manifestFullComplete$seatID %in%
                                                         unlist(seatsList) == FALSE &
                                                         manifestFullComplete$seatID %in%
                                                         sths$seatID == FALSE),]$seatID,1)
      cust        <- sample(custData[which(custData$custID %in% unlist(custList) == FALSE & custData$custID %in% sths$custID == FALSE),]$custID,1)
      lastSeat    <- (firstSeat + (blockStart - 1))
      seats       <- as.vector(unlist(mapply(seq,firstSeat,lastSeat)))
      seatsReduce <- subset(seats,seats %in% unlist(seatsList) == FALSE & seats %in% sths$seatID == FALSE)
      seatsList[[i]] <- seatsReduce
      custList[[i]]  <- rep(cust,length(seatsList[[i]]))
      ticType        <- ifelse(length(seatsList[[i]]) >= 20, 'gr','si')
      ttList[[i]]    <- rep(ticType,length(seatsList[[i]]))

      x <- x + length(seatsList[[i]])
      i <- i + 1
      # print(x)
    }

    sths$gameID          <- seasonData[z,]$gameID
    salesActivity        <- as.data.frame(matrix(nrow= length(unlist(seatsList)), ncol=4))
    names(salesActivity) <- c('seatID','custID','ticketType','gameID')
    salesActivity[,1]    <- unlist(seatsList)
    salesActivity[,2]    <- unlist(custList)
    salesActivity[,3]    <- unlist(ttList)
    salesActivity[,4]    <- seasonData[z,]$gameID
    # append to sth file
    salesActivity        <- salesActivity[1:(seasonData[z,]$ticketSales - nrow(sths)),]
    salesActivity        <- bind_rows(salesActivity,sths)
    salesActivity        <- salesActivity[order(salesActivity$seatID),]
    # write to file
    name <- paste(seasonData[z,]$season,seasonData[z,]$gameID,
                  seasonData[z,]$gameNumber,'activity_sales.csv',sep='_')
    write.csv(salesActivity,paste('ticket_activity/',name,sep=''),row.names = FALSE)

    z <- z + 1
    print(z)
  }

    ## Not run:
#    if (FALSE){
# Get list of season ticket holders
#      set.seed(755)
#      sths <- sample(unique(salesActivity$custID),2800)
#      seasonTickets <- subset(salesActivity,salesActivity$custID %in% sths)
#      write.csv(seasonTickets,'ticket_activity/season_ticket_holders.csv',row.names = FALSE)
#  } ## End(**Not run**)
}


#-----------------------------------------------------------------
# End simulate purchases
#-----------------------------------------------------------------
