#' Aggregated CRM call and revenue data
#'
#' A small, pre-aggregated CRM extract summarising sales-rep call activity and
#' the revenue it generated. Used in the project-framing chapter.
#'
#' @format A data frame with 5,000 rows and 3 variables:
#' \describe{
#'   \item{repID}{character; the sales rep responsible for the calls.}
#'   \item{call}{numeric; number of calls.}
#'   \item{revenue}{numeric; revenue generated.}
#' }
#' @details This is the kind of table you would build by rolling up raw CRM
#'   activity to the rep level.
#' @source Simulated for the book. See `create_data/data_ch4_aggregated_crm_data.R`.
#' @family datasets
"aggregated_crm_data"

#' Customer identity table
#'
#' A minimal ticketing-database customer list: an account ID and a name.
#'
#' @format A data frame with 200,000 rows and 2 variables:
#' \describe{
#'   \item{custID}{character; customer ID.}
#'   \item{name}{character; customer full name.}
#' }
#' @details Mirrors the customer table you would find in a ticketing system.
#' @source Simulated with [f_build_customer_ids_names()].
#' @family datasets
"customer_data"

#' Customer demographic data
#'
#' Third-party demographic attributes appended to the customer base, of the kind
#' a club might purchase to enrich its ticketing records.
#'
#' @format A data frame with 200,000 rows and 14 variables:
#' \describe{
#'   \item{custID}{character; ID from the ticketing database.}
#'   \item{nameF}{character; first name.}
#'   \item{nameL}{character; last name.}
#'   \item{nameFull}{character; full name.}
#'   \item{gender}{character; `"m"` or `"f"`.}
#'   \item{age}{numeric; age in years.}
#'   \item{latitude}{numeric; geographic latitude.}
#'   \item{longitude}{numeric; geographic longitude.}
#'   \item{distance}{numeric; distance from the venue, in miles.}
#'   \item{maritalStatus}{character; `"m"` (married) or `"s"` (single).}
#'   \item{ethnicity}{character; ethnicity code (`"w"`, `"aa"`, `"h"`, `"a"`).}
#'   \item{children}{character; `"y"` or `"n"`.}
#'   \item{hhIncome}{numeric; household income on a scaled axis.}
#'   \item{county}{character; `"state,county"` derived from the coordinates.}
#' }
#' @details The simulation columns are produced by [f_build_demographic_data()];
#'   the `county` column is appended from the coordinates during data assembly.
#' @source Simulated for the book. See `create_data/data_ch5_ch7_demographic_data.R`.
#' @family datasets
"demographic_data"

#' Factor-analysis survey responses
#'
#' Simulated survey data designed for a factor-analysis example: respondents
#' rate how important a list of game-day activities is to them.
#'
#' @format A data frame with 10,000 rows and 26 variables: `ReasonForAttending`
#'   (character) plus 25 numeric activity ratings, each scored 0-10:
#' \describe{
#'   \item{ReasonForAttending}{character; the respondent's stated reason for attending.}
#'   \item{Socialize, Tailgate, TakeSelfies, PostToSocialMedia, SeeFriends}{numeric ratings.}
#'   \item{VisitKidAttractions, MeetMascot, SnacksForKids, KidsRunBases, KidsSlide}{numeric ratings.}
#'   \item{GetDinner, EatParkFood, SampleFood, GetDrinks, DrinkBeer}{numeric ratings.}
#'   \item{BuyGear, TourThePark, VisitAttractions, WatchPregameShow, SeeTheChicken}{numeric ratings.}
#'   \item{UpgradeSeats, GetAutographs, WatchGame, SeePractice, MeetPlayers}{numeric ratings.}
#' }
#' @details Respondents were asked to rate each activity from 0 to 10.
#' @source Simulated for the book. See `create_data/data_ch5_fa_survey_data.R`.
#' @family datasets
"fa_survey_data"

#' Frequency table of gate-scan observations
#'
#' An empirical frequency distribution of per-minute gate-scan counts, used in
#' the operations chapter to illustrate building a probability distribution.
#'
#' @format A data frame with 161 rows and 3 variables:
#' \describe{
#'   \item{variable}{numeric; the observed scan count.}
#'   \item{Freq}{numeric; how often that count occurred.}
#'   \item{prob}{numeric; relative frequency (sums to 1).}
#' }
#' @details The shape produced by [f_build_freq_table()] applied to scan counts.
#' @source Simulated for the book. See `create_data/data_ch10_scan_data.R`.
#' @family datasets
"freq_table_data"

#' Binned frequency table of service times
#'
#' A binned empirical distribution of concession service times, used alongside
#' [freq_table_data] in the operations chapter.
#'
#' @format A data frame with 23 rows and 3 variables:
#' \describe{
#'   \item{bin}{numeric; bin index.}
#'   \item{mean_seconds}{numeric; mean service time (seconds) in the bin.}
#'   \item{prob}{numeric; relative frequency of the bin (sums to 1).}
#' }
#' @source Simulated for the book. See `create_data/data_ch10_wait_times_data.R`.
#' @family datasets
"freq_table_data_bin"

#' Seating-manifest and pricing data
#'
#' A seating manifest for the venue with the price of each seat under three
#' selling models.
#'
#' @format A data frame with 44,705 rows and 8 variables:
#' \describe{
#'   \item{seatID}{numeric; unique seat ID.}
#'   \item{section}{numeric; section ID.}
#'   \item{sectionNumber}{numeric; section number.}
#'   \item{rowNumber}{numeric; row number.}
#'   \item{seatNumber}{numeric; seat number.}
#'   \item{seasonPrice}{numeric; current season-ticket price.}
#'   \item{groupPrice}{numeric; current group price.}
#'   \item{singlePrice}{numeric; single-game price.}
#' }
#' @details A transformed view of seating data; real manifests rarely look this
#'   tidy.
#' @source Simulated for the book. See `create_data/data_ch7_manifest_data.R`.
#' @family datasets
"manifest_data"

#' Perceptual-map survey counts
#'
#' Consolidated survey counts describing how respondents associate each of three
#' teams with a set of brand attributes; the input to a perceptual map.
#'
#' @format A data frame with 3 rows (one per team) and 10 numeric attribute
#'   columns: `Friendly`, `Exciting`, `Fresh`, `Inovative`, `Fun`, `Old`,
#'   `Historic`, `Winners`, `Great`, `Expensive`. Each cell is a count of
#'   responses. (`Inovative` retains the original spelling used in the data.)
#' @source Simulated for the book. See `create_data/data_ch9_perceptual_data.R`.
#' @family datasets
"perceptual_data"

#' Gate-scan observations
#'
#' Per-minute counts of fans scanning in at an entry gate over three evenings,
#' as if recorded by hand with a stopwatch.
#'
#' @format A data frame with 900 rows and 4 variables:
#' \describe{
#'   \item{observations}{numeric; ordered observation index.}
#'   \item{scans}{numeric; number of customer scans in that minute.}
#'   \item{action_time}{`hms`; time of the observation.}
#'   \item{date}{character; date of the observation.}
#' }
#' @details Three 300-minute sessions stacked together; see [f_get_scan_data()].
#' @source Simulated for the book. See `create_data/data_ch10_scan_data.R`.
#' @family datasets
"scan_data"

#' Simulated season schedule and sales
#'
#' Three seasons of home games with schedule attributes and simulated ticket
#' sales, aggregated for schedule and promotion analysis.
#'
#' @format A data frame with 243 rows and 12 variables:
#' \describe{
#'   \item{gameNumber}{numeric; game order within the season.}
#'   \item{team}{character; opponent abbreviation.}
#'   \item{date}{Date; game date.}
#'   \item{dayOfWeek}{character; abbreviated weekday, e.g. `"Sun"`.}
#'   \item{month}{character; abbreviated month, e.g. `"Mar"`.}
#'   \item{weekEnd}{logical; was it a weekend game?}
#'   \item{schoolInOut}{logical; were children out of school?}
#'   \item{daysSinceLastGame}{numeric; days since the previous home game.}
#'   \item{openingDay}{logical; was it opening day?}
#'   \item{promotion}{character; promotion type or `"none"`.}
#'   \item{ticketSales}{numeric; total tickets sold (capped at venue capacity).}
#'   \item{season}{numeric; season year.}
#' }
#' @details The kind of table you would aggregate to run a regression on a
#'   schedule. See [f_build_season()].
#' @source Simulated for the book. See `create_data/data_ch3_ch6_ch8_season_data.R`.
#' @family datasets
"season_data"

#' Secondary-market ticket sales
#'
#' Processed secondary-market (resale) transactions, joining each sold seat to
#' its original price and the price paid on the secondary market.
#'
#' @format A data frame with 853,444 rows and 9 variables:
#' \describe{
#'   \item{seatID}{numeric; manifest ID of the seat.}
#'   \item{custID}{character; purchasing customer.}
#'   \item{ticketType}{character; `"si"` (single), `"se"` (season) or `"gr"` (group).}
#'   \item{gameID}{numeric; ID of the game.}
#'   \item{tickets}{numeric; number of tickets in the transaction.}
#'   \item{priceKey}{character; seat-and-type key used to look up price.}
#'   \item{price}{numeric; original (primary) ticket price.}
#'   \item{orderedCluster}{numeric; clustered spend band.}
#'   \item{secondayrPrice}{numeric; price paid on the secondary market. (The
#'     column name retains its original spelling.)}
#' }
#' @details In practice this data can be hard or impossible to obtain.
#' @source Simulated for the book. See `create_data/data_ch6_secondary_data.R`.
#' @family datasets
"secondary_data"

#' Season-ticket renewal data
#'
#' Account-level features for season-ticket holders together with whether they
#' renewed, used for lead-scoring and renewal-likelihood models.
#'
#' @format A data frame with 13,706 rows and 10 variables:
#' \describe{
#'   \item{accountID}{character; customer account ID.}
#'   \item{corporate}{character; `"c"` (corporate) or `"i"` (individual).}
#'   \item{season}{numeric; season year.}
#'   \item{planType}{character; `"f"` (full) or `"p"` (partial).}
#'   \item{ticketUsage}{numeric; share of tickets used (0-1).}
#'   \item{tenure}{numeric; years as a customer.}
#'   \item{spend}{numeric; amount spent on tickets.}
#'   \item{tickets}{numeric; number of tickets held.}
#'   \item{distance}{numeric; distance from the venue, in miles.}
#'   \item{renewed}{character; `"r"` (renewed) or `"nr"` (not renewed).}
#' }
#' @details Built by transforming several sources; see
#'   [f_create_lead_scoring_data()].
#' @source Simulated for the book. See `create_data/data_ch7_customer_renewal.R`.
#' @family datasets
"customer_renewals"

#' Concession-stand wait times
#'
#' Transaction-level wait times at a concession stand, decomposed into ordering,
#' payment and fulfilment stages.
#'
#' @format A data frame with 900 rows and 5 variables:
#' \describe{
#'   \item{transaction}{numeric; transaction index.}
#'   \item{orderTimes}{numeric; ordering time, in seconds.}
#'   \item{paymentTimes}{numeric; payment time, in seconds.}
#'   \item{fulfillTimes}{numeric; fulfilment time, in seconds.}
#'   \item{totalTime}{numeric; total time, in seconds.}
#' }
#' @details Three 300-transaction sessions stacked together; see
#'   [f_get_wait_times()].
#' @source Simulated for the book. See `create_data/data_ch10_wait_times_data.R`.
#' @family datasets
"wait_times_data"

#' Wait-time distribution sample
#'
#' A larger sample of concession wait times (same columns as [wait_times_data])
#' used to illustrate fitting and simulating from a distribution.
#'
#' @format A data frame with 3,000 rows and 5 variables:
#' \describe{
#'   \item{transaction}{numeric; transaction index.}
#'   \item{orderTimes}{numeric; ordering time, in seconds.}
#'   \item{paymentTimes}{numeric; payment time, in seconds.}
#'   \item{fulfillTimes}{numeric; fulfilment time, in seconds.}
#'   \item{totalTime}{numeric; total time, in seconds.}
#' }
#' @source Simulated for the book. See `create_data/data_ch10_wait_times_data.R`.
#' @family datasets
"wait_times_distribution_data"
