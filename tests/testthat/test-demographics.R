test_that("f_determine_gender classifies known names", {
  expect_equal(f_determine_gender("Mary"), "f")
  expect_equal(f_determine_gender("Jennifer"), "f")
  expect_equal(f_determine_gender("Robert"), "m")
  expect_equal(f_determine_gender("Zzzzz"), "m") # not on the female list
})

test_that("f_get_age returns the requested number of whole-number ages", {
  ages <- f_get_age(seed = 200, num_rows = 1000, g1_mean = 30, g2_mean = 55,
                    g1_sd = 8, g2_sd = 10)
  expect_length(ages, 1000)
  expect_equal(ages, round(ages))
})

test_that("f_get_age is reproducible (seed bug fix)", {
  a <- f_get_age(seed = 5, num_rows = 500, g1_mean = 30, g2_mean = 55,
                 g1_sd = 8, g2_sd = 10)
  b <- f_get_age(seed = 5, num_rows = 500, g1_mean = 30, g2_mean = 55,
                 g1_sd = 8, g2_sd = 10)
  expect_identical(a, b)
})

test_that("f_get_distance returns three equal-length vectors and is reproducible", {
  loc <- f_get_distance(seed = 305, n = 1000)
  expect_length(loc, 3)
  expect_length(loc[[1]], 1000)
  expect_length(loc[[3]], 1000)
  expect_true(all(loc[[3]] >= 0))
  expect_identical(loc, f_get_distance(seed = 305, n = 1000))
})

test_that("demographic helpers return only allowed levels", {
  set.seed(1)
  expect_true(f_demographics_married("f", 52) %in% c("m", "s"))
  expect_true(f_demographics_children("m", 38) %in% c("y", "n"))
  expect_true(f_demographics_ethnicity(20, 35) %in% c("w", "aa", "h", "a"))
})

test_that("f_build_customer_ids_names returns four name columns", {
  cd <- f_build_customer_ids_names(seed = 755, num_rows = 50)
  expect_equal(dim(cd), c(50L, 4L))
  expect_named(cd, c("custID", "nameF", "nameL", "nameFull"))
  expect_true(all(nchar(cd$custID) == 12))
})
