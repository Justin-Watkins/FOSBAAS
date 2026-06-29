#' FOSBAAS: Functions and Data for Sports Business Analytics
#'
#' The FOSBAAS package is the companion to the book *Fundamentals of Sports
#' Business Analytics and Strategy*. It provides the example data sets used in
#' the book along with the simulation and helper functions that created them.
#'
#' @section Data sets:
#' The package ships pre-built data describing a fictional sports franchise,
#' including [customer_data], [demographic_data], [season_data],
#' [manifest_data], [secondary_data], [customer_renewals], [aggregated_crm_data],
#' [fa_survey_data], [perceptual_data], [scan_data], [wait_times_data],
#' [wait_times_distribution_data], [freq_table_data] and [freq_table_data_bin].
#' Because `LazyData` is enabled, each is available by name once the package is
#' attached.
#'
#' @section Simulation helpers:
#' Functions are grouped into families that mirror the book's chapters:
#' \itemize{
#'   \item Season simulation -- [f_build_season()] and its helpers.
#'   \item Customer & demographic simulation -- [f_build_demographic_data()] and helpers.
#'   \item Lead scoring -- [f_create_lead_scoring_data()] and helpers.
#'   \item Operations & queueing -- [f_get_MMKN()], [f_get_wait_times()], [f_get_scan_data()].
#'   \item Small utilities -- [f_add_zero()], [f_define_line()], [f_build_freq_table()].
#' }
#'
#' @details
#' The functions favour clarity over robustness: they have minimal error
#' checking and are meant to illustrate a single idea from the book rather than
#' to be used in production.
#'
#' @keywords internal
"_PACKAGE"
