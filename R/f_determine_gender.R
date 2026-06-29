#' Infer gender from a first name
#'
#' Classifies a first name as female (`"f"`) or male (`"m"`) by checking it
#' against a list of common female names. Anything not on the list is treated as
#' male. This is a deliberately crude rule used only to attach a gender field to
#' the simulated customers.
#'
#' @param f_name A single first name (character string).
#'
#' @return `"f"` if the name is in the female-name list, otherwise `"m"`.
#'
#' @examples
#' f_determine_gender("Mary")
#' f_determine_gender("Robert")
#'
#' @family demographic_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_determine_gender.R>
#' @export
f_determine_gender <- function(f_name) {
  f_names <- c("Mary","Patricia","Jennifer","Linda","Elizabeth","Barbara","Susan","Jessica",
               "Sarah","Karen","Nancy","Margaret","Lisa","Betty","Dorothy","Sandra","Ashley",
               "Kimberly","Donna","Emily","Michelle","Carol","Amanda","Melissa","Deborah",
               "Stephanie","Rebecca","Laura","Sharon","Cynthia","Kathleen","Helen","Amy",
               "Shirley","Angela","Anna","Brenda","Pamela","Nicole","Ruth","Katherine",
               "Samantha","Christine","Emma","Catherine","Debra","Virginia","Rachel","Carolyn",
               "Janet","Maria","Heather","Diane","Julie","Joyce","Victoria","Kelly",
               "Christina","Joan","Evelyn","Lauren","Judith","Olivia","Frances","Martha",
               "Cheryl","Megan","Andrea","Hannah","Jacqueline","Ann","Jean","Alice",
               "Kathryn","Gloria","Teresa","Doris","Sara","Janice","Julia","Marie","Madison",
               "Grace","Judy","Theresa","Beverly","Denise","Marilyn","Amber","Danielle",
               "Abigail","Brittany","Rose","Diana","Natalie","Sophia","Alexis","Lori","Kayla",
               "Jane")

  if (f_name %in% f_names) "f" else "m"
}
