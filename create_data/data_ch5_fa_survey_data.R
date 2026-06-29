#-----------------------------------------------------------------
# fa_survey_data  (used in Chapter 5)
#
# Builds survey responses designed for a factor-analysis example, producing
# `FOSBAAS::fa_survey_data`. Each respondent states a reason for attending and
# rates 25 game-day activities from 0-10. Structured noise is then applied to
# groups of related activities (social, kids, food, attractions, on-field) so
# that latent factors emerge. Plain base-R simulation (no package functions).
#
# Output schema: ReasonForAttending plus 25 numeric activity ratings.
#-----------------------------------------------------------------

reasons_list <- c("Company outing", "Family outing, no children",
                  "Family outing, with children", "I was given tickets",
                  "No distinct reason", "Support the Team", "Special Game",
                  "Other Promotion", "See a concert", "Celebrate an anniversary",
                  "Support the opposing team", "Celebrate a birthday",
                  "See my friends", "Visit the park")

activities <- c("Socialize", "Tailgate", "TakeSelfies", "PostToSocialMedia", "SeeFriends",
                "VisitKidAttractions", "MeetMascot", "SnacksForKids", "KidsRunBases", "KidsSlide",
                "GetDinner", "EatParkFood", "SampleFood", "GetDrinks", "DrinkBeer",
                "BuyGear", "TourThePark", "VisitAttractions", "WatchPregameShow", "SeeTheChicken",
                "UpgradeSeats", "GetAutographs", "WatchGame", "SeePractice", "MeetPlayers")

# Base ratings: uniform 0-10 for every activity.
ratings <- data.frame(matrix(nrow = 10000, ncol = 25))
names(ratings) <- activities

set.seed(755)
reason <- data.frame(ReasonForAttending = sample(reasons_list, 10000, replace = TRUE),
                     stringsAsFactors = FALSE)

set.seed(755)
ratings[] <- apply(ratings, 2, function(x) round(runif(10000, 0, 10), 0))

fa_survey_data <- cbind(reason, ratings)

#-----------------------------------------------------------------
# Apply structured noise to related blocks of activities so that
# correlated factors emerge (columns 2:26 are the 25 activities).
#-----------------------------------------------------------------
set.seed(1456) # attractions block
idx <- sample(1:1000, 1000)
fa_survey_data[idx, 17:21] <-
  apply(fa_survey_data[idx, 17:21], c(1, 2), function(x) ifelse(runif(1) > .5, x * .25, x))

set.seed(3000) # on-field block
idx <- sample(1:1000, 1000)
fa_survey_data[idx, 22:26] <-
  apply(fa_survey_data[idx, 22:26], c(1, 2), function(x) ifelse(runif(1) > .1, x * .20, x))

set.seed(755) # social block
idx <- sample(1:2000, 2000)
fa_survey_data[idx, 2:6] <-
  apply(fa_survey_data[idx, 2:6], c(1, 2), function(x) ifelse(runif(1) > .2, x * .40, x))

set.seed(114) # kids block
idx <- sample(1:2000, 2000)
fa_survey_data[idx, 7:11] <-
  apply(fa_survey_data[idx, 7:11], c(1, 2), function(x) ifelse(runif(1) > .4, x * .30, x))

set.seed(999) # food block
idx <- sample(1:2500, 2500)
fa_survey_data[idx, 12:16] <-
  apply(fa_survey_data[idx, 12:16], c(1, 2), function(x) ifelse(runif(1) > .2, x * .50, x))

# Clamp to 0-10 and round.
fa_survey_data[, 2:26] <- apply(fa_survey_data[, 2:26], 2,
                                function(x) round(pmin(x, 10), 0))

# readr::write_csv(fa_survey_data, "data-raw/fa_survey_data.csv")
