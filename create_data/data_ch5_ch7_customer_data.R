#-----------------------------------------------------------------
# customer_data  (used in Chapters 5 and 7)
#
# Builds the customer identity table shipped as `FOSBAAS::customer_data`:
# a 12-character account ID plus a randomly assigned name. The name/ID logic
# now lives in the package, so this script just calls the exported function.
#
# Output schema: custID, nameF, nameL, nameFull (the published `customer_data`
# keeps custID and a single `name` column).
#-----------------------------------------------------------------

library(FOSBAAS)

customer_data <- f_build_customer_ids_names(seed = 755, num_rows = 200000)

# Gender is inferred from the first name (used when building demographic_data):
customer_data$gender <- vapply(customer_data$nameF, f_determine_gender,
                               character(1), USE.NAMES = FALSE)

# readr::write_csv(customer_data, "data-raw/customer_data.csv")
