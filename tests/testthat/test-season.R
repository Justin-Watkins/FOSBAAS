test_that("f_create_season_frame has the documented columns", {
  frame <- f_create_season_frame()
  expect_equal(dim(frame), c(81L, 12L))
  expect_named(frame, c("gameNumber", "team", "date", "dayOfWeek", "month",
                        "weekEnd", "schoolInOut", "daysSinceLastGame",
                        "openingDay", "promotion", "ticketSales", "season"))
})

test_that("f_constrain_sales caps at venue capacity", {
  expect_equal(f_constrain_sales(38000), 38000)
  expect_equal(f_constrain_sales(45000), 45000)
  expect_equal(f_constrain_sales(51000), 45000)
})

test_that("f_assign_promotion returns one label per game", {
  promos <- f_assign_promotion(number_bbhead = 5, number_concert = 3,
                               number_other = 5, games = 81, seed = 366)
  expect_length(promos, 81)
  expect_setequal(unique(promos), c("bobblehead", "concert", "other", "none"))
  expect_equal(sum(promos == "bobblehead"), 5)
  expect_equal(sum(promos == "concert"), 3)
  expect_equal(sum(promos == "other"), 5)
})

test_that("f_assign_promotion is reproducible (set.seed bug fix)", {
  a <- f_assign_promotion(5, 3, 5, 81, seed = 366)
  b <- f_assign_promotion(5, 3, 5, 81, seed = 366)
  expect_identical(a, b)
})

test_that("f_days_since_last_game uses 50 for the opener then real gaps", {
  dates <- as.Date(c("2024-03-27", "2024-03-28", "2024-03-31"))
  expect_equal(f_days_since_last_game(dates), c(50L, 1L, 3L))
})

test_that("f_select_team_for_series returns the requested number of teams", {
  mlb <- c("ARI", "ATL", "BAL", "BOS", "CHC", "CWS", "CIN", "CLE", "COL",
           "DET", "FLA", "HOU", "KAN", "LAA", "LAD", "MIL", "MIN", "NYM",
           "NYY", "OAK", "PHI", "PIT", "SD", "SF", "SEA", "STL", "TB",
           "TEX", "TOR", "WAS")
  teams <- f_select_team_for_series(seed = 755, number = 81)
  expect_length(teams, 81)
  expect_true(all(unlist(teams) %in% mlb))
})

test_that("f_build_season returns the documented shape and types", {
  season <- f_build_season(seed1 = 3000, season_year = 2024, seed2 = 714,
                           num_games = 81, seed3 = 366, num_bbh = 5,
                           num_con = 3, num_oth = 5, seed4 = 309, seed5 = 25,
                           mean_sales = 29000, sd_sales = 3500)
  expect_equal(dim(season), c(81L, 12L))
  expect_s3_class(season$date, "Date")
  expect_type(season$dayOfWeek, "character")
  expect_true(all(season$dayOfWeek %in%
                    c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))
  expect_type(season$weekEnd, "logical")
  expect_lte(max(season$ticketSales), 45000)
})
