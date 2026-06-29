#' Build a table of customer IDs and names
#'
#' Creates a table of synthetic customers, each with a random 12-character
#' alphanumeric account ID and a randomly assigned first and last name drawn
#' from common US name lists. It is the starting point for
#' [f_build_demographic_data()].
#'
#' @param seed Integer seed; makes the IDs and names reproducible.
#' @param num_rows Number of customers to generate; defaults to `200000`.
#'
#' @return A data frame with `num_rows` rows and the columns `custID`, `nameF`
#'   (first name), `nameL` (last name) and `nameFull` (`"first last"`).
#'
#' @details
#' First names are sampled uniformly from a combined list of common male and
#' female given names and last names from a list of common surnames. Earlier
#' versions built the full cartesian product of names before sampling; this
#' version samples the two name parts independently, which gives the same
#' uniform distribution far more cheaply.
#'
#' @examples
#' head(f_build_customer_ids_names(seed = 755, num_rows = 10))
#'
#' @family demographic_simulation
#' @source <https://github.com/Justin-Watkins/FOSBAAS/blob/master/R/f_build_customer_ids_names.R>
#' @export
f_build_customer_ids_names <- function(seed, num_rows = 200000) {

  m_names <- c("James","John","Robert","Michael","William","David","Richard","Joseph",
               "Thomas","Charles","Christopher","Daniel","Matthew","Anthony","Donald",
               "Mark","Paul","Steven","Andrew","Kenneth","Joshua","George","Kevin",
               "Brian","Edward","Ronald","Timothy","Jason","Jeffrey","Ryan","Jacob",
               "Gary","Nicholas","Eric","Stephen","Jonathan","Larry","Justin","Scott",
               "Brandon","Frank","Benjamin","Gregory","Samuel","Raymond","Patrick",
               "Alexander","Jack","Dennis","Jerry","Tyler","Aaron","Jose","Henry",
               "Douglas","Adam","Peter","Nathan","Zachary","Walter","Kyle","Harold",
               "Carl","Jeremy","Keith","Roger","Gerald","Ethan","Arthur","Terry",
               "Christian","Sean","Lawrence","Austin","Joe","Noah","Jesse","Albert",
               "Bryan","Billy","Bruce","Willie","Jordan","Dylan","Alan","Ralph",
               "Gabriel","Roy","Juan","Wayne","Eugene","Logan","Randy","Louis","Russell",
               "Vincent","Philip","Bobby","Johnny","Bradley")

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

  l_names <- c("Smith","Johnson","Williams","Brown","Jones","Miller","Davis","Garcia","Rodriguez",
               "Wilson","Martinez","Anderson","Taylor","Thomas","Hernandez","Moore","Martin","Jackson",
               "Thompson","White","Lopez","Lee","Gonzalez","Harris","Clark","Lewis","Robinson","Walker",
               "Perez","Hall","Young","Allen","Sanchez","Wright","King","Scott","Green","Baker","Adams",
               "Nelson","Hill","Ramirez","Campbell","Mitchell","Roberts","Carter","Phillips","Evans",
               "Turner","Torres","Parker","Collins","Edwards","Stewart","Flores","Morris","Nguyen",
               "Murphy","Rivera","Cook","Rogers","Morgan","Peterson","Cooper","Reed","Bailey","Bell",
               "Gomez","Kelly","Howard","Ward","Cox","Diaz","Richardson","Wood","Watson","Brooks",
               "Bennett","Gray","James","Reyes","Cruz","Hughes","Price","Myers","Long","Foster",
               "Sanders","Ross","Morales","Powell","Sullivan","Russell","Ortiz","Jenkins","Gutierrez",
               "Perry","Butler","Barnes","Fisher","Henderson","Coleman","Simmons","Patterson","Jordan",
               "Reynolds","Hamilton","Graham","Kim","Gonzales","Alexander","Ramos","Wallace","Griffin",
               "West","Cole","Hayes","Chavez","Gibson","Bryant","Ellis","Stevens","Murray","Ford",
               "Marshall","Owens","Mcdonald","Harrison","Ruiz","Kennedy","Wells","Alvarez","Woods",
               "Mendoza","Castillo","Olson","Webb","Washington","Tucker","Freeman","Burns","Henry",
               "Vasquez","Snyder","Simpson","Crawford","Jimenez","Porter","Mason","Shaw","Gordon",
               "Wagner","Hunter","Romero","Hicks","Dixon","Hunt","Palmer","Robertson","Black","Holmes",
               "Stone","Meyer","Boyd","Mills","Warren","Fox","Rose","Rice","Moreno","Schmidt","Patel",
               "Ferguson","Nichols","Herrera","Medina","Ryan","Fernandez","Weaver","Daniels","Stephens",
               "Gardner","Payne","Kelley","Dunn","Pierce","Arnold","Tran","Spencer","Peters","Hawkins",
               "Grant","Hansen","Castro","Hoffman","Hart","Elliott","Cunningham","Knight","Bradley",
               "Carroll","Hudson","Duncan","Armstrong","Berry","Andrews","Johnston","Ray","Lane",
               "Riley","Carpenter","Perkins","Aguilar","Silva","Richards","Willis","Matthews","Chapman",
               "Lawrence","Garza","Vargas","Watkins","Wheeler","Larson","Carlson","Harper","George",
               "Greene","Burke","Guzman","Morrison","Munoz","Jacobs","Obrien","Lawson","Franklin","Lynch",
               "Bishop","Carr","Salazar","Austin","Mendez","Gilbert","Jensen","Williamson","Montgomery",
               "Harvey","Oliver","Howell","Dean","Hanson","Weber","Garrett","Sims","Burton","Fuller",
               "Soto","Mccoy","Welch","Chen","Schultz","Walters","Reid","Fields","Walsh","Little",
               "Fowler","Bowman","Davidson","May","Day","Schneider","Newman","Brewer","Lucas","Holland",
               "Wong","Banks","Santos","Curtis","Pearson","Delgado","Valdez","Pena","Rios","Douglas",
               "Sandoval","Barrett","Hopkins","Keller","Guerrero","Stanley","Bates","Alvarado","Beck",
               "Ortega","Wade","Estrada","Contreras","Barnett","Caldwell","Santiago","Lambert","Powers",
               "Chambers","Nunez","Craig","Leonard","Lowe","Rhodes","Byrd","Gregory","Shelton","Frazier",
               "Becker","Maldonado","Fleming","Vega","Sutton","Cohen","Jennings","Parks","Mcdaniel",
               "Watts","Barker","Norris","Vaughn","Vazquez","Holt","Schwartz","Steele","Benson","Neal",
               "Dominguez","Horton","Terry","Wolfe","Hale","Lyons","Graves","Haynes","Miles","Park",
               "Warner","Padilla","Bush","Thornton","Mccarthy","Mann","Zimmerman","Erickson","Fletcher",
               "Mckinney","Page","Dawson","Joseph","Marquez","Reeves","Klein","Espinoza","Baldwin",
               "Moran","Love","Robbins","Higgins","Ball","Cortez","Le","Griffith","Bowen","Sharp")

  set.seed(seed)
  cust_id <- vapply(seq_len(num_rows), function(i)
    paste(sample(c(0:9, LETTERS), 12, replace = TRUE), collapse = ""),
    character(1))

  first_names <- c(f_names, m_names)
  name_f <- sample(first_names, num_rows, replace = TRUE)
  name_l <- sample(l_names,     num_rows, replace = TRUE)

  data.frame(
    custID   = cust_id,
    nameF    = name_f,
    nameL    = name_l,
    nameFull = paste(name_f, name_l),
    stringsAsFactors = FALSE
  )
}
