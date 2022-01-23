#-----------------------------------------------------------------
# Read all csv files into one data set
#
#
#
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# BEGIN Read existing data sets
#-----------------------------------------------------------------
setwd("C:/package/FOSBAAS/data")
dataPaths   <- list.files(path       = "C:/package/FOSBAAS/data-raw",
                          pattern    = "\\.csv$",
                          full.names = TRUE)
data        <- lapply(dataPaths,readr::read_csv)
names(data) <- gsub(".csv","",
                    list.files("C:/package/FOSBAAS/data-raw",
                               full.names = FALSE),
                    fixed      = TRUE)

#-----------------------------------------------------------------
# END Read existing data sets
#-----------------------------------------------------------------
