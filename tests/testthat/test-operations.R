test_that("f_calc_scans peaks inside the window", {
  peak <- f_calc_scans(x = 230, y = 110, j = 150)
  edge <- f_calc_scans(x = 230, y = 110, j = 1)
  expect_gt(peak, edge)
})

test_that("f_get_scan_data returns 300 non-negative counts", {
  sd_ <- f_get_scan_data(x_value = 230, y_value = 110, seed = 714, sd_mod = 14)
  expect_equal(dim(sd_), c(300L, 2L))
  expect_named(sd_, c("observations", "scans"))
  expect_true(all(sd_$scans >= 0))
})

test_that("f_get_line_length returns n rows driven by its parameters", {
  ll <- f_get_line_length(seed = 755, n = 300, u1 = 20, sd1 = 10,
                          u2 = 30, sd2 = 5)
  expect_equal(dim(ll), c(300L, 2L))
  expect_named(ll, c("observation", "lineLength"))
  expect_true(all(ll$lineLength >= 0))
})

test_that("f_get_wait_times returns the documented columns", {
  wt <- f_get_wait_times(seed = 755, n = 300, rate1 = .03,
                         rate2 = .06, rate3 = .15)
  expect_equal(dim(wt), c(300L, 5L))
  expect_named(wt, c("transaction", "orderTimes", "paymentTimes",
                     "fulfillTimes", "totalTime"))
  # totalTime is the rounded sum of the (unrounded) stages, so it is within
  # rounding error of the sum of the rounded stage columns.
  expect_true(all(wt$totalTime >= 0))
  expect_true(all(abs(wt$totalTime -
    (wt$orderTimes + wt$paymentTimes + wt$fulfillTimes)) <= 2))
})

test_that("f_get_MMKN returns a 7x2 metric table", {
  frame <- f_get_MMKN(k = 2, N = 5, ta = 10, ts = 8)
  expect_equal(dim(frame), c(7L, 2L))
  expect_named(frame, c("Metric", "Value"))
  expect_equal(frame$Value[1], 2)  # servers
  expect_equal(frame$Value[2], 5)  # capacity
})

test_that("f_get_scans uses the supplied fit (no global)", {
  obs <- 1:300
  shape <- data.frame(t = obs, scans = -0.01 * (obs - 150)^2 + 200)
  fit <- lm(scans ~ t + I(t^2), data = shape)
  set.seed(1)
  val <- f_get_scans(new_time = 150, zero_1 = 0, zero_2 = 300, fit = fit)
  expect_true(is.numeric(val))
  expect_gte(val, 0)
})

test_that("f_get_third_degree_fit evaluates a cubic", {
  x <- 1:50
  y <- 2 + 0.5 * x - 0.01 * x^2 + 0.0001 * x^3
  fit <- lm(y ~ x + I(x^2) + I(x^3))
  expect_equal(unname(f_get_third_degree_fit(25, fit)),
               unname(predict(fit, newdata = data.frame(x = 25))),
               tolerance = 1e-6)
})

test_that("f_simulate_distribution returns the requested number of draws", {
  set.seed(42)
  sim <- f_simulate_distribution(sample(0:50, 500, replace = TRUE),
                                 rand = 100, seeds = 42)
  expect_length(sim, 100)
})
