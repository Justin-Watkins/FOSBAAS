#-----------------------------------------------------------------
# season_data  (used in Chapters 3, 6 and 8)
#
# Builds three seasons of home games with simulated ticket sales and stacks
# them, producing `FOSBAAS::season_data`. The schedule/sales simulation now
# lives in the package, so this script just calls f_build_season() once per
# season with the seeds and demand parameters used for the book.
#
# Output schema (12 columns): gameNumber, team, date, dayOfWeek, month, weekEnd,
# schoolInOut, daysSinceLastGame, openingDay, promotion, ticketSales, season.
#-----------------------------------------------------------------

library(FOSBAAS)
library(dplyr)

season_2022 <- f_build_season(seed1 = 3000, season_year = 2022, seed2 = 714,
                              num_games = 81, seed3 = 366, num_bbh = 5,
                              num_con = 3, num_oth = 5, seed4 = 309, seed5 = 25,
                              mean_sales = 29000, sd_sales = 3500)

season_2023 <- f_build_season(seed1 = 755, season_year = 2023, seed2 = 4256,
                              num_games = 81, seed3 = 54, num_bbh = 6,
                              num_con = 4, num_oth = 7, seed4 = 309, seed5 = 25,
                              mean_sales = 30500, sd_sales = 3000)

season_2024 <- f_build_season(seed1 = 2892, season_year = 2024, seed2 = 714,
                              num_games = 81, seed3 = 366, num_bbh = 9,
                              num_con = 2, num_oth = 6, seed4 = 6856, seed5 = 2892,
                              mean_sales = 32300, sd_sales = 2900)

season_data <- dplyr::bind_rows(season_2022, season_2023, season_2024)

# readr::write_csv(season_data, "data-raw/season_data.csv")
