#-----------------------------------------------------------------
# perceptual_data  (used in Chapter 9)
#
# Consolidated survey counts associating three teams with ten brand
# attributes, producing `FOSBAAS::perceptual_data`. Plain base-R simulation
# (no package functions required).
#
# Output schema: one row per team, ten numeric attribute columns.
# (`Inovative` keeps the original spelling used throughout the book.)
#-----------------------------------------------------------------

attributes <- c("Friendly", "Exciting", "Fresh", "Inovative", "Fun",
                "Old", "Historic", "Winners", "Great", "Expensive")
teams <- c("Chicken Hearts", "Grizzlies", "Predators")

perceptual_data <- as.data.frame(matrix(nrow = length(teams),
                                        ncol = length(attributes)))
names(perceptual_data)     <- attributes
row.names(perceptual_data) <- teams

set.seed(2632)
perceptual_data <- apply(perceptual_data, c(1, 2),
                         function(x) round(rnorm(1, 3000, 1000), 0))

# write.csv(perceptual_data, "data-raw/perceptual_data.csv", row.names = FALSE)
