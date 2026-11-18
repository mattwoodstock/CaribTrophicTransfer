mk_res <- function(n = 60, holdout = NA) {
  p <- ttm_example_project(n_draws = n)
  p$settings$sir_resample <- n; p$settings$holdout_from_year <- holdout
  p$settings$disturbance_loss_cv <- 0.3
  suppressWarnings(run_ttm(p, verbose = FALSE))
}

test_that("parameter matrix, sensitivity analysis and SIR diagnostics", {
  res <- mk_res(80)
  expect_equal(nrow(res$stage1$params), 80)
  expect_true(all(c("tte:reef", "npp:reef", "dist_loss_mult") %in% colnames(res$stage1$params)))
  sa <- sensitivity_analysis(res, top = 5)
  expect_true(all(c("SRRC", "PRCC", "R2_rank_model") %in% names(sa)))
  expect_true(all(abs(sa$SRRC) <= 1.5))
  dg <- sir_diagnostics(res)
  expect_true(dg$summary$ESS > 0 && dg$summary$ESS <= 80)
  expect_true(all(dg$shrinkage$shrinkage >= 0 & dg$shrinkage$shrinkage <= 1))
})

test_that("self-test returns bias and coverage", {
  res <- mk_res(80)
  st <- self_test(res, n_rep = 8)
  expect_true(all(c("median_rel_error", "coverage50", "coverage90") %in% names(st$summary)))
  expect_true(all(st$summary$coverage90 >= 0 & st$summary$coverage90 <= 1))
})

test_that("temporal hold-out excludes data from SIR and validates them", {
  res <- mk_res(60, holdout = 2017)
  fit <- .fit_indices(res$proj); val <- .validate_indices(res$proj)
  expect_true(all(fit$year < 2017)); expect_true(all(val$year >= 2017))
  v <- validate_out_of_sample(res)
  expect_true(all(v$summary$coverage90 >= 0 & v$summary$coverage90 <= 1))
  expect_true(nrow(v$observations) == nrow(val))
  lb <- loo_benchmarks(res)
  expect_true(lb$PIT >= 0 && lb$PIT <= 1)
  rp <- retrospective_peel(res, peels = c(1, 2))
  expect_true(all(is.finite(rp$rel_change_MMSY)))
})

test_that("ecosystem overfishing indices and structural ensemble run", {
  res <- mk_res(40)
  eo <- ecosystem_overfishing_indices(res)
  expect_true(all(eo$ryther_p50 > 0)); expect_true(all(eo$fogarty_permil_p50 > 0))
  p <- ttm_example_project(n_draws = 30); p$settings$sir_resample <- 30
  v <- default_structural_variants()[c("base", "tte_0.10")]
  ens <- run_structural_ensemble(p, v, n_draws = 30, verbose = FALSE)
  expect_equal(sort(unique(ens$by_variant$variant)), c("base", "tte_0.10"))
  expect_true(all(ens$pooled$MMSY_p05_t <= ens$pooled$MMSY_p95_t))
})

test_that("adaptive importance sampling adds rounds with finite corrections", {
  p <- ttm_example_project(n_draws = 30); p$settings$sir_resample <- 30; p$settings$sir_adaptive_rounds <- 1
  res <- suppressWarnings(run_ttm(p, verbose = FALSE))
  expect_equal(res$n_prior, 60)
  expect_true(all(is.finite(res$stage1$log_is_correction)))
  expect_equal(sum(res$stage1$log_is_correction[1:30] == res$stage1$log_is_correction[1]) < 30, TRUE)
  expect_true(res$sir$ess >= 1)
})
