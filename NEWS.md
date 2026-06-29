# FOSBAAS 0.2.1

* `f_create_dates()` regression fix: the 0.2.0 cleanup dropped a `sample()` draw
  and moved another, which shifted the random stream so some seeds (e.g. the
  ones used in the book's pricing chapter) ran a schedule past the calendar
  window and produced `NA` dates. That surfaced downstream as
  "missing value where TRUE/FALSE needed" in `f_attend_coefficient()` via
  `f_build_season()`. Restored the original draw sequence (so tuned seeds
  reproduce their schedules) and extended the candidate date window into
  December as a guard against overruns.

# FOSBAAS 0.2.0

Full rewrite of the package source and documentation.

## Documentation

* Every exported function now has a complete roxygen2 help page with
  `@param`, `@return`, `@details` and a runnable `@examples` section, grouped
  into families (season simulation, demographics, lead scoring, operations,
  utilities). Placeholder `\url{GIT}` sources were replaced with real links.
* All bundled data sets are documented with correct dimensions, column types
  and descriptions. Three previously undocumented data sets are now covered:
  `freq_table_data`, `freq_table_data_bin` and `wait_times_distribution_data`.
* Added a real `README`, a "Getting started" vignette and a `pkgdown` config.

## Bug fixes (these change simulation output)

* `f_assign_promotion()` now seeds the generator (`set.seed(seed)`) instead of
  assigning a variable named `set.seed`, so results are reproducible.
* `f_attend_coefficient()` no longer reseeds on every row, dropped the no-op
  `mod == mod + 0` branch, and fixed the misspelt `"BAl"` opponent code.
* `f_demographics_ethnicity()` had two duplicate (dead) branches and a
  probability vector summing to more than 1; the four distance-by-age cells are
  now distinct and normalised.
* `f_demographics_married()`/`_children()`/`_ethnicity()` no longer take an
  unused/misused per-row `seed`; seed once in the caller instead.
* `f_get_age()` and `f_get_distance()` now actually use their `seed` argument.
* `f_get_scans()` takes the fitted model as a `fit` argument instead of relying
  on a global variable.
* `f_renewal_assignment()` scores renewals on the combined loyalty score
  (usage + distance clusters, 2-10) rather than a single cluster column, so the
  full range of renewal probabilities is reachable.
* `f_create_lead_scoring_data()` corporate plan-type probabilities were
  `c(.95, .5)` (summing to 1.45); now `c(.95, .05)`.
* `f_build_season()` returns readable `dayOfWeek`/`month` labels and a `Date`
  column instead of integer factor codes.

## Infrastructure

* `dplyr`, `lubridate` and `stats` are declared in `Imports` and accessed via
  `::`/`@importFrom`; in-body `require()` calls were removed.
* Added a `testthat` suite covering the exported functions.
* `LazyDataCompression: xz` reduces the installed size.

Note: because several fixes change the random draws, re-running the simulation
functions now produces different output than 0.1.x. The bundled `.rda` data
sets are unchanged.
