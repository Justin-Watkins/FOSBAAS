#-----------------------------------------------------------------
# combine_data_into_one_object
#
# Utility used while assembling the package data. It reads every CSV in
# data-raw/ into a single named list, so each raw data set can be referenced as
# `data$<name>` from one object. Run from the package root.
#-----------------------------------------------------------------

library(readr)

raw_dir <- "data-raw"

data_paths  <- list.files(path = raw_dir, pattern = "\\.csv$", full.names = TRUE)
data        <- lapply(data_paths, readr::read_csv)
names(data) <- gsub(".csv", "",
                    list.files(raw_dir, full.names = FALSE),
                    fixed = TRUE)

# `data` now holds every raw data set, e.g. data$season_data, data$manifest_data.
