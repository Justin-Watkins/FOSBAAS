#-----------------------------------------------------------------
# aggregated_crm_data  (used in Chapter 4)
#
# A pre-aggregated CRM extract of call counts and revenue by sales rep,
# producing `FOSBAAS::aggregated_crm_data`. This is plain base-R simulation
# (no package functions required).
#
# Output schema: repID, call, revenue.
#-----------------------------------------------------------------

n_rows <- 5000
n_reps <- 10

aggregated_crm_data <- data.frame(
  repID   = character(n_rows),
  call    = numeric(n_rows),
  revenue = numeric(n_rows),
  stringsAsFactors = FALSE
)

set.seed(755)

# Ten reps, each appearing 500 times.
rep_ids <- vapply(seq_len(n_reps), function(i)
  paste(sample(c(0:9, LETTERS), 12, replace = TRUE), collapse = ""),
  character(1))
aggregated_crm_data$repID <- rep(rep_ids, n_rows / n_reps)

# Call counts skew toward the middle of the 1-5 range.
calls <- sample(1:5, 10000, prob = c(.20, .25, .30, .20, .05), replace = TRUE)

# Revenue is a mixture: many typical deals, a few large ones, and many zeros.
revenue <- c(rnorm(25000, 3000, 800), rnorm(500, 10000, 1000),
             rnorm(2000, 500, 60),    rnorm(500, 50000, 8000),
             rep(0, 30000))

aggregated_crm_data$revenue <- sample(revenue, n_rows)
aggregated_crm_data$call    <- sample(calls, n_rows)

# readr::write_csv(aggregated_crm_data, "data-raw/aggregated_crm_data.csv")
