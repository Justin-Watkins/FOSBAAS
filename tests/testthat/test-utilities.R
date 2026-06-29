test_that("f_add_zero pads only single digits", {
  expect_equal(f_add_zero(2), "02")
  expect_equal(f_add_zero(9), "09")
  expect_equal(f_add_zero(10), "10")
  expect_equal(f_add_zero(11), "11")
})

test_that("f_define_line evaluates the line through two points", {
  # Line through (0,0) and (10,10) is y = x.
  expect_equal(f_define_line(y2 = 10, y1 = 0, x2 = 10, x1 = 0, x = 2), 2)
  expect_equal(f_define_line(y2 = 10, y1 = 0, x2 = 10, x1 = 0, x = 7), 7)
})

test_that("f_build_freq_table probabilities sum to one", {
  set.seed(1)
  ft <- f_build_freq_table(sample(0:5, 200, replace = TRUE))
  expect_named(ft, c("variable", "Freq", "prob"))
  expect_equal(sum(ft$prob), 1)
  expect_equal(sum(ft$Freq), 200)
  expect_type(ft$variable, "double")
})

test_that("f_get_miles_NV is zero at the reference point and positive elsewhere", {
  expect_equal(f_get_miles_NV(36.1613, -86.7786), 0)
  expect_gt(f_get_miles_NV(35.0, -85.0), 0)
})

test_that("f_get_time_observations produces ordered HH:MM labels", {
  obs <- f_get_time_observations(17, 21)
  expect_length(obs, 300)
  expect_equal(obs[1], "17:00")
  expect_equal(obs[length(obs)], "21:59")
  expect_true(all(grepl("^[0-9]{2}:[0-9]{2}$", obs)))
})
