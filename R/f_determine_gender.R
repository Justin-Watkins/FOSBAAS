#' @title f_determine_gender
#' @description Determine gender by using first names
#' @source \url{GIT}
#' @export


f_determine_gender <- function(f_name){

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


  if (f_name %in% f_names == TRUE){gender <- 'f'}
  else{gender <- 'm'}

  return(gender)


}
