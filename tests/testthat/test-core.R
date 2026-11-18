test_that("productivity unit conversions match the literature synthesis", {
  # 1 mmol O2 m-2 d-1 = 4.38 t C km-2 yr-1 (PQ = 1)
  expect_equal(convert_productivity(1, "mmol_O2_m2_d"), 4.38)
  # Adey & Steneck (1985) St. Croix fore-reef Pg 626 mmol O2 -> 2742 t C
  expect_equal(round(convert_productivity(626, "mmol_O2_m2_d")), 2742)
  expect_equal(convert_productivity(1, "g_C_m2_d"), 365)
  # Gomis et al. 2025: 5833 kg C ha-1 yr-1 = 583.3 t C km-2 yr-1
  expect_equal(convert_productivity(5833, "kg_C_ha_yr"), 583.3)
  expect_equal(round(convert_productivity(5.42, "g_O2_m2_d"), 1), round(5.42 * 12 / 32 * 365, 1))
  expect_error(convert_productivity(1, "furlongs"))
  expect_equal(lb_to_t(t_to_lb(3)), 3)
})

test_that("trophic levels follow from the diet matrix", {
  D <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0,
                0, 0.5, 0.5, 0, 0, 0, 0, 0), 2, byrow = TRUE, dimnames = list(c("a", "b"), ttm_pools()))
  expect_equal(group_trophic_level(D), c(2, 3.5))
})

test_that("exact logistic step is stable for large r and converges to K", {
  B <- 10
  for (i in 1:50) B <- CaribTTM:::.logistic_step(B, r = 60, K = 1000, dt = 1 / 12)
  expect_equal(B, 1000, tolerance = 1e-6)
})

test_that("energy is conserved: TL2 consumer production = TTE x TL1 production", {
  E <- matrix(c(1000, 150, 22.5, 3.375), 1)
  D <- matrix(0, 2, 8, dimnames = list(c("a", "b"), ttm_pools())); D[, "B_TL1"] <- 1
  HP <- matrix(1, 2, 1)
  P <- CaribTTM:::.group_production(E, numeric(0), 0.15, 0, c(a = 1, b = 3), D, HP)
  expect_equal(sum(P), 0.15 * 1000)
  expect_equal(P[2] / P[1], 3)
})

test_that("deterministic example run produces consistent reference points", {
  p <- ttm_example_project()
  o <- simulate_ttm(p, draw_parameters(p, FALSE), build_drivers(p))
  expect_true(all(is.finite(o$B)))
  expect_true(all(o$MMSY > 0))
  # MMSY = x * P/B * K
  expect_equal(o$MMSY[1, "lobster"], 0.5 * 0.8 * o$K[1, "lobster"])
  # historical catch reproduces landings x expansion
  expect_equal(unname(t_to_lb(o$C["2015", "lobster"])), 70000, tolerance = 1e-6)
  # fishing feedback lowers prevailing capacity of predators relative to unfished pools
  expect_true(o$K["2020", "snapper"] <= o$K0["2020", "snapper"] + 1e-9)
})

test_that("fixed-biomass partitioning calibrates B0", {
  p <- ttm_example_project()
  p$groups$partition_method[1] <- "fixed_biomass"; p$groups$B0_fixed_t[1] <- 123
  p <- validate_ttm_project(p)
  o <- simulate_ttm(p, draw_parameters(p, FALSE), build_drivers(p))
  expect_equal(unname(o$K0[1, "lobster"]), 123, tolerance = 1e-3)
})

test_that("habitat loss and hurricanes reduce production; scenarios differ", {
  p <- ttm_example_project()
  d0 <- build_drivers(p, "Baseline"); d1 <- build_drivers(p, "Hurricane_2025")
  expect_lt(d1$p1_mult["2025", "reef"], 1)
  expect_lt(d1$p1_mult["2025", "reef"], d0$p1_mult["2025", "reef"])
  expect_lt(d1$effort_mult["2025", "trap"], 1)
  par <- draw_parameters(p, FALSE)
  b <- simulate_ttm(p, par, d0); h <- simulate_ttm(p, par, d1, q = b$q)
  expect_lt(h$P1["2025", "reef"], b$P1["2025", "reef"])
  e <- simulate_ttm(p, par, build_drivers(p, "Effort_up"), q = b$q)
  expect_gt(sum(e$C["2030", ]), sum(b$C["2030", ]))
})

test_that("Monte Carlo + SIR + SPF outputs run end to end", {
  p <- ttm_example_project(n_draws = 30)
  p$settings$sir_resample <- 30
  res <- suppressWarnings(run_ttm(p, verbose = FALSE))
  expect_s3_class(res, "ttm_results")
  expect_false(is.null(res$sir))
  rp <- reference_points(res)
  expect_true(all(c("MMSY_p50_lb", "target_lb", "OFL_proxy_lb", "status") %in% names(rp)))
  expect_true(all(rp$MMSY_p05_t <= rp$MMSY_p95_t))
  ga <- gear_allocation(res)
  # catch-share allocation sums back to group MMSY
  tot <- apply(ga$draws, c(1, 3), sum)
  MM <- apply(get_metric(res, "MMSY")[, c("2018", "2019", "2020"), , drop = FALSE], c(1, 3), mean)
  harvested <- c("lobster", "parrotfish", "snapper")
  expect_equal(tot[, harvested], MM[, harvested], tolerance = 1e-8)
  td <- tempfile(); f <- write_spf_inputs(res, td)
  expect_true(file.exists(file.path(td, "spf_gear_effort_ceilings.csv")))
  g2 <- aggregate_mmsy(res, "guild")
  expect_equal(nrow(g2), 3)
})

test_that("gridded covariate aggregation is area-weighted and clipped", {
  df <- expand.grid(lon = c(-65, -64.5), lat = c(17.5, 17.7), year = 2020:2021)
  df$value <- 1 + (df$year - 2020)
  a <- aggregate_gridded_covariate(df, lon_range = c(-65.1, -64), lat_range = c(17, 18), variable = "sst")
  expect_equal(a$value, c(1, 2))
  expect_equal(a$variable[1], "sst")
})

test_that("templates round-trip through read_ttm_project", {
  td <- tempfile(); write_ttm_templates(td)
  p <- read_ttm_project(td)
  expect_equal(nrow(p$groups), 5)
  expect_equal(p$settings$hist_end, 2020)
})
