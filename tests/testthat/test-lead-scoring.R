test_that("f_assign_renewal returns one of the supplied labels", {
  set.seed(1)
  expect_true(f_assign_renewal(10, c("r", "nr")) %in% c("r", "nr"))
  expect_true(f_assign_renewal(1, c("r", "nr")) %in% c("r", "nr"))
})

test_that("f_assign_renewal favours renewal for high scores", {
  set.seed(1)
  high <- replicate(500, f_assign_renewal(10, c("r", "nr")))
  low  <- replicate(500, f_assign_renewal(1, c("r", "nr")))
  expect_gt(mean(high == "r"), mean(low == "r"))
})

test_that("lead-scoring helpers stay in range", {
  set.seed(1)
  expect_gte(f_calculate_tenure("c", "f", 10, 25), 0)
  expect_gte(f_calculate_spend("c", "f", 12, 8), 0)
  use <- f_calculate_ticket_use("c", 10, 25)
  expect_gte(use, 0)
  expect_lte(use, 1)
})

test_that("f_create_lead_scoring_data returns the renewal schema", {
  lead <- f_create_lead_scoring_data(
    seed = 434, num_purchasers = 250, season = 2023,
    f_tenure = f_calculate_tenure, f_spend = f_calculate_spend,
    f_use = f_calculate_ticket_use,
    f_renewal_assignment = f_renewal_assignment,
    f_assign_renewal = f_assign_renewal)

  expect_equal(nrow(lead), 250L)
  expect_named(lead, c("accountID", "corporate", "season", "planType",
                       "ticketUsage", "tenure", "spend", "tickets",
                       "distance", "renewed"))
  expect_setequal(unique(lead$renewed), c("r", "nr"))
  expect_true(all(lead$corporate %in% c("c", "i")))
  expect_true(all(lead$planType %in% c("f", "p")))
})

test_that("f_create_lead_scoring_data without renewals omits the renewed column", {
  lead <- f_create_lead_scoring_data(
    seed = 434, num_purchasers = 100, season = 2023,
    f_tenure = f_calculate_tenure, f_spend = f_calculate_spend,
    f_use = f_calculate_ticket_use,
    f_renewal_assignment = f_renewal_assignment,
    f_assign_renewal = f_assign_renewal, renew = FALSE)

  expect_false("renewed" %in% names(lead))
  expect_true(all(lead$tenure == 0))
})
