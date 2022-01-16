#' @title f_select_team_for_series
#' @description Determine which team is played
#' @source \url{GIT}
#' @export


f_select_team_for_series <- function(seed,number){

  # Create list of teams and apply to schedule
  mlb_teams <-  c("ARI", "ATL", "BAL", "BOS", "CHC", "CWS", "CIN",
                  "CLE", "COL", "DET", "FLA", "HOU", "KAN", "LAA",
                  "LAD", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI",
                  "PIT", "SD", "SF", "SEA", "STL", "TB", "TEX",
                  "TOR", "WAS")
  set.seed(seed)
  # Assign random team every three games
  x <- 1
  opponents <- list()
  chance <- c(3,6)

  while(x <= number){

    series_opponent <- sample(mlb_teams, 1, replace = FALSE)
    games <- sample(chance,1,prob = c(.7,.3))
    opponents[x:(x+games)] <- series_opponent

    x <- x + games
  }
  return(opponents[1:number])
}
