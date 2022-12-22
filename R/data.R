#' @title aggregated_crm_data
#' @description Replicates aggregated call data
#' @format A data frame with 5000 rows and 3 variables:
#' \describe{
#'   \item{\code{repID}}{character rep responsible for calls}
#'   \item{\code{call}}{double number of calls}
#'   \item{\code{revenue}}{double revenue generated}
#'}
#' @details This data would be consolidated from raw CRM data
"aggregated_crm_data"

#' @title scan_data
#' @description Replicates data periodically captured at and entry gate
#' @format A data frame with 900 rows and 4 variables:
#' \describe{
#'   \item{\code{observations}}{double ordered observations}
#'   \item{\code{scans}}{double count of customer scans}
#'   \item{\code{action_time}}{double time of observation}
#'   \item{\code{date}}{character date of observation}
#'}
#' @details This data replicates data captured by hand and a stopwatch at a gate
"scan_data"

#' @title wait_times_data
#' @description Decomposed data captured at a concession stands
#' @format A data frame with 900 rows and 5 variables:
#' \describe{
#'   \item{\code{transaction}}{double Count of transactions}
#'   \item{\code{orderTimes}}{double order time in seconds}
#'   \item{\code{paymentTimes}}{double payment time in seconds}
#'   \item{\code{fulfillTimes}}{double fulfillment time in secongs}
#'   \item{\code{totalTime}}{double totla time in seconds}
#'}
#' @details Replicates data gathered through direct observation
"wait_times_data"

#' @title customer_data
#' @description database of customer id and names
#' @format A data frame with 200000 rows and 2 variables:
#' \describe{
#'   \item{\code{custID}}{character customer id}
#'   \item{\code{name}}{character customner name}
#'}
#' @details Replicates customer data found in a ticketing database
"customer_data"

#' @title demographic_data
#' @description Replicates demographic information purchased on customers
#' @format A data frame with 200000 rows and 13 variables:
#' \describe{
#'   \item{\code{custID}}{character Id from ticketing database}
#'   \item{\code{nameF}}{character First name}
#'   \item{\code{nameL}}{character Last name}
#'   \item{\code{nameFull}}{character Full name}
#'   \item{\code{gender}}{character gender}
#'   \item{\code{age}}{double age}
#'   \item{\code{latitude}}{double geographic latitude}
#'   \item{\code{longitude}}{double geographic longitude}
#'   \item{\code{distance}}{double distance from arena}
#'   \item{\code{maritalStatus}}{character married}
#'   \item{\code{ethnicity}}{character ethnicity}
#'   \item{\code{children}}{character children}
#'   \item{\code{hhIncome}}{double household income scaled}
#'   \item{\code{county}}{character state,county}
#'}
#' @details Demographic data purchased from third parties takes this form
"demographic_data"

#' @title manifest_data
#' @description Data to simulate manifest data from a ticketing system
#' @format A data frame with 44705 rows and 8 variables:
#' \describe{
#'   \item{\code{seatID}}{double unique id of seat}
#'   \item{\code{section}}{double section id}
#'   \item{\code{sectionNumber}}{double section number}
#'   \item{\code{rowNumber}}{double row number}
#'   \item{\code{seatNumber}}{double seat number}
#'   \item{\code{seasonPrice}}{double current season price}
#'   \item{\code{groupPrice}}{double current group price}
#'   \item{\code{singlePrice}}{double single game price}
#'}
#' @details This data has been transformed and won't typically look like this
"manifest_data"

#' @title perceptual_data
#' @description Simulates Consolidated data useful for building perceptual maps
#' @format A data frame with 3 rows and 10 variables:
#' \describe{
#'   \item{\code{Friendly}}{double count of repsonses}
#'   \item{\code{Exciting}}{double count of repsonses}
#'   \item{\code{Fresh}}{double double count of repsonses}
#'   \item{\code{Inovative}}{double double count of repsonses}
#'   \item{\code{Fun}}{double double count of repsonses}
#'   \item{\code{Old}}{double double count of repsonses}
#'   \item{\code{Historic}}{double double count of repsonses}
#'   \item{\code{Winners}}{double double count of repsonses}
#'   \item{\code{Great}}{double double count of repsonses}
#'   \item{\code{Expensive}}{double double count of repsonses}
#'}
#' @details Consolidated data take from a survey
"perceptual_data"

#' @title season_data
#' @description Simulates data aggregated for schedule comparisons
#' @format A data frame with 243 rows and 12 variables:
#' \describe{
#'   \item{\code{gameNumber}}{double game order}
#'   \item{\code{team}}{character away team}
#'   \item{\code{date}}{double game date}
#'   \item{\code{dayOfWeek}}{character day of the week}
#'   \item{\code{month}}{character month of event}
#'   \item{\code{weekEnd}}{logical was it a weekend}
#'   \item{\code{schoolInOut}}{logical were children in school}
#'   \item{\code{daysSinceLastGame}}{double days since last game was played}
#'   \item{\code{openingDay}}{logical is it opening day}
#'   \item{\code{promotion}}{character was there a promotion}
#'   \item{\code{ticketSales}}{double total tickets sold}
#'   \item{\code{season}}{double year of season}
#'}
#' @details This data would be aggregated to perform regression analysis on schedules
"season_data"

#' @title secondary_data
#' @description Simulates processed data from secondary market sales
#' @format A data frame with 853444 rows and 9 variables:
#' \describe{
#'   \item{\code{seatID}}{double manifest id of the seat}
#'   \item{\code{custID}}{character customer who purchased the tickets}
#'   \item{\code{ticketType}}{character single, season, group}
#'   \item{\code{gameID}}{double id of game purchased}
#'   \item{\code{tickets}}{double number of tickets purchased}
#'   \item{\code{priceKey}}{character seat and ticket type to match price}
#'   \item{\code{price}}{double original price of ticket}
#'   \item{\code{orderedCluster}}{double clustered ticket spend}
#'   \item{\code{secondayrPrice}}{double Price paid on secondary market}
#'}
#' @details It may be impossible to obtain this data
"secondary_data"

#' @title customer_renewals
#' @description Simulates data for calculating renewal likelihood
#' @format A data frame with 13706 rows and 10 variables:
#' \describe{
#'   \item{\code{accountID}}{character customer account id}
#'   \item{\code{corporate}}{character is it a corporation}
#'   \item{\code{season}}{double season id}
#'   \item{\code{planType}}{character type of ticket purchased}
#'   \item{\code{ticketUsage}}{double percentage of tickets used}
#'   \item{\code{tenure}}{double years as a customer}
#'   \item{\code{spend}}{double amount spend to tickets}
#'   \item{\code{tickets}}{double number of tickets purchased}
#'   \item{\code{distance}}{double distance from arena}
#'   \item{\code{renewed}}{character did the customer renew their seats}
#'}
#' @details This data would be transformed from many sources
"customer_renewals"

#' @title fa_survey_data
#' @description Simulated survey response data to perform factor analysis
#' @format A data frame with 10000 rows and 26 variables:
#' \describe{
#'   \item{\code{ReasonForAttending}}{character element rank}
#'   \item{\code{Socialize}}{double element rank}
#'   \item{\code{Tailgate}}{double element rank}
#'   \item{\code{TakeSelfies}}{double element rank}
#'   \item{\code{PostToSocialMedia}}{double element rank}
#'   \item{\code{SeeFriends}}{double element rank}
#'   \item{\code{VisitKidAttractions}}{double element rank}
#'   \item{\code{MeetMascot}}{double element rank}
#'   \item{\code{SnacksForKids}}{double element rank}
#'   \item{\code{KidsRunBases}}{double element rank}
#'   \item{\code{KidsSlide}}{double element rank}
#'   \item{\code{GetDinner}}{double element rank}
#'   \item{\code{EatParkFood}}{double element rank}
#'   \item{\code{SampleFood}}{double element rank}
#'   \item{\code{GetDrinks}}{double element rank}
#'   \item{\code{DrinkBeer}}{double element rank}
#'   \item{\code{BuyGear}}{double element rank}
#'   \item{\code{TourThePark}}{double element rank}
#'   \item{\code{VisitAttractions}}{double element rank}
#'   \item{\code{WatchPregameShow}}{double element rank}
#'   \item{\code{SeeTheChicken}}{double element rank}
#'   \item{\code{UpgradeSeats}}{double element rank}
#'   \item{\code{GetAutographs}}{double element rank}
#'   \item{\code{WatchGame}}{double element rank}
#'   \item{\code{SeePractice}}{double element rank}
#'   \item{\code{MeetPlayers}}{double element rank}
#'}
#' @details Fan asked to rank elements 0-10
"fa_survey_data"
