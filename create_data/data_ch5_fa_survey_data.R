#-----------------------------------------------------------------
# fa_survey_data data set
#
# This data set approximates the results of a survey question designed for a factor analysis
#
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# BEGIN factor analysis survey data
#-----------------------------------------------------------------

reasons <-
  c('Company outing',
    'Family outing, no children',
    'Family outing, with children',
    'I was given tickets',
    'No distinct reason',
    'Support the Team',
    'Special Game',
    'Other Promotion',
    'See a concert',
    'Celebrate an anniversary',
    'Support the opposing team',
    'Celebrate a birthday',
    'See my friends',
    'Visit the park')

survey_data_experiential <- data.frame(matrix(nrow = 10000,ncol = 25))

set.seed(755)
reas        <- as.data.frame(sample(reasons,10000,replace = T))
names(reas) <- 'ReasonForAttending'

names(survey_data_experiential) <-
  c('Socialize','Tailgate','TakeSelfies','PostToSocialMedia','SeeFriends',
    'VisitKidAttractions','MeetMascot','SnacksForKids','KidsRunBases','KidsSlide',
    'GetDinner', 'EatParkFood','SampleFood','GetDrinks','DrinkBeer',
    'BuyGear','TourThePark','VisitAttractions','WatchPregameShow','SeeTheChicken',
    'UpgradeSeats','GetAutographs','WatchGame','SeePractice','MeetPlayers')

set.seed(755)
survey_data_experiential[] <- apply(survey_data_experiential,2,function(x) round(runif(10000,0,10),0))

reasons <- cbind.data.frame(reas,survey_data_experiential)
#-----------------------------------------------------------------
# add noise
set.seed(1456)
sampattr <- sample(1:1000,1000,replace = F)
reasons[sampattr,17:21] <-
  apply(reasons[sampattr,17:21],1:2, function(x) ifelse(runif(1,0,1) > .5,x*.25,x))

set.seed(3000)
sampgame <- sample(1:1000,1000,replace = F)
reasons[sampgame,22:26] <-
  apply(reasons[sampgame,22:26],1:2, function(x) ifelse(runif(1,0,1) > .1,x*.20,x))

set.seed(755)
sampSocial <- sample(1:2000,2000,replace = F)
reasons[sampSocial,2:6] <-
  apply(reasons[sampSocial,2:6],1:2, function(x) ifelse(runif(1,0,1) > .2,x*.4,x))

set.seed(114)
sampkids <- sample(1:2000,2000,replace = F)
reasons[sampkids,7:11] <-
  apply(reasons[sampkids,7:11],1:2, function(x) ifelse(runif(1,0,1) > .4,x*.3,x))

set.seed(999)
sampfood <- sample(1:2500,2500,replace = F)
reasons[sampfood,12:16] <-
  apply(reasons[sampfood,12:16],1:2, function(x) ifelse(runif(1,0,1) > .2,x*.5,x))

reasons[,2:26] <- apply(reasons[,2:26],2,function(x) ifelse(x > 10,10,x))
reasons[,2:26] <- apply(reasons[,2:26],2,function(x) round(x,0))

FA   <- reasons[,c(1:26)]
Type <- FA[,1]
FA[,2:26] <- apply(FA[,2:26],2,function(x) as.numeric(x))

#write.csv(FA,'FA_survey_data.csv',row.names = F)

#-----------------------------------------------------------------
# END factor analysis survey data
#-----------------------------------------------------------------
