det_run <- function(p, scenario = "Baseline", mp = NULL) {
  par <- draw_parameters(p, stochastic = FALSE)
  simulate_ttm(p, par, build_drivers(p, scenario, par), mp = mp)
}

test_that("per-recruit x_msy follows theory", {
  a <- per_recruit_xmsy(M = 0.3, K = 0.25, linf = 40, lc = 20, h = 0.75)
  b <- per_recruit_xmsy(M = 0.3, K = 0.25, linf = 40, lc = 20, h = 0.5)
  expect_true(a$x > 0.1 && a$x < 0.5)
  expect_true(b$x < a$x)                      # lower steepness -> lower MSY
  expect_true(a$Bmsy_over_B0 > 0.2 && a$Bmsy_over_B0 < 0.5)
  sp <- data.frame(group = "g", M = 0.3, K = 0.25, linf = 40, weight = 1)
  lh <- life_history_xmsy(sp, data.frame(group = "g", lc = 20, lc_cv = 0.1), n = 30)
  expect_equal(nrow(lh), 1); expect_true(lh$x_msy_cv > 0)
})

test_that("natural-mortality basis: MMSY = x M K", {
  p <- validate_ttm_project(ttm_example_project())
  p$groups$m_nat[p$groups$group == "lobster"] <- 0.3; p$groups$m_nat_cv <- 0
  o <- det_run(p)
  expect_equal(unname(o$MMSY[1, "lobster"]), unname(p$groups$x_msy[1] * 0.3 * o$K[1, "lobster"]), tolerance = 1e-8)
})

test_that("open recruitment prevents collapse under heavy catch", {
  p <- validate_ttm_project(ttm_example_project())
  p$landings$landings_lb[p$landings$group == "lobster"] <- p$landings$landings_lb[p$landings$group == "lobster"] * 6
  closed <- det_run(p)
  p$groups$open_recruitment[p$groups$group == "lobster"] <- 0.9
  open <- det_run(p)
  expect_true(open$catch_shortfall[["lobster"]] <= closed$catch_shortfall[["lobster"]])
  expect_true(min(open$B[, "lobster"]) >= min(closed$B[, "lobster"]))
})

test_that("nursery habitat loss lowers K of dependent groups only", {
  p <- validate_ttm_project(ttm_example_project())
  p$groups$nursery_habitat[p$groups$group == "snapper"] <- "seagrass"
  p$groups$nursery_dependence[p$groups$group == "snapper"] <- 0.5
  p$scenarios <- rbind(p$scenarios[0, ], data.frame(scenario = "SG_loss", driver = "area_mult", target = "seagrass",
                                                     start_year = 2021, end_year = 2030, value = 0.5)[, names(p$scenarios)])
  b <- det_run(p); s <- det_run(p, "SG_loss")
  expect_lt(s$nursery["2030", "snapper"], 0.8)
  expect_equal(unname(s$nursery["2030", "lobster"]), 1)
})

test_that("no-take areas cap historical catch and scale projected F", {
  p <- validate_ttm_project(ttm_example_project())
  p$habitats$closed_fraction[p$habitats$habitat == "reef"] <- 0.5
  o <- det_run(p)
  expect_true(all(o$fopen[, "lobster"] < 1 & o$fopen[, "lobster"] > 0.4))
  p$scenarios <- rbind(p$scenarios[0, ], data.frame(scenario = "MPA", driver = "closed_fraction", target = "reef",
                                                     start_year = 2021, end_year = 2030, value = 0.9)[, names(p$scenarios)])
  m <- det_run(p, "MPA")
  expect_lt(sum(m$C["2025", ]), sum(o$C["2025", ]))
})

test_that("ghost fishing adds unaccounted mortality after trap losses", {
  p <- validate_ttm_project(ttm_example_project())
  p$settings$ghost_efficiency <- 0.3; p$settings$ghost_efficiency_cv <- 0; p$settings$ghost_gears <- "trap"
  o <- det_run(p, "Hurricane_2025")
  expect_true(any(o$ghost[, "lobster"] > 0))
  p$settings$ghost_efficiency <- 0
  expect_true(all(det_run(p, "Hurricane_2025")$ghost[, "lobster"] == 0))
})

test_that("herbivory feedback links grazer biomass to coral recovery", {
  p <- validate_ttm_project(ttm_example_project())
  if (!any(p$habitats$quality_model == "coral")) skip("example has no coral habitat")
  p$settings$herbivore_groups <- "parrotfish"; p$settings$herbivory_elasticity <- 1
  p$settings$herbivory_elasticity_cv <- 0
  o <- det_run(p)
  expect_true(all(is.finite(o$graze[o$graze > 0])))
})

test_that("observation model: hyperstability, creep and absolute indices", {
  B <- array(rep(seq(100, 50, length.out = 5), each = 3), c(3, 5, 1), dimnames = list(NULL, 2001:2005, "g"))
  idx <- data.frame(year = 2001:2005, index = "i", group = "g", value = seq(100, 50, length.out = 5), cv = 0.2)
  n1 <- .index_nll_rows(B, idx, 0.2)
  idx$hyperstability <- 0.5
  n2 <- .index_nll_rows(B, idx, 0.2)
  expect_lt(n1[1, 1], n2[1, 1])               # proportional data fit beta = 1 better
  idx$hyperstability <- 1; idx$type <- "absolute"; idx$q_prior <- 1; idx$q_cv <- 0.1
  n3 <- .index_nll_rows(B, idx, 0.2)
  idx$q_prior <- 0.2
  n4 <- .index_nll_rows(B, idx, 0.2)
  expect_lt(n3[1, 1], n4[1, 1])               # wrong detectability prior penalized
})

test_that("PSIS smooths heavy tails and reports k", {
  set.seed(3)
  ps <- psis_smooth(log(abs(stats::rcauchy(2000))))
  expect_true(is.finite(ps$k) && ps$k > 0.5)
  expect_true(ps$method %in% c("psis", "truncated"))
  ok <- psis_smooth(stats::rnorm(4000, 0, 0.5)); expect_equal(ok$method, "psis")
})

test_that("catch-only comparator, value of information and MSE run", {
  p <- ttm_example_project(n_draws = 60); p$settings$sir_resample <- 60
  res <- suppressWarnings(run_ttm(p, verbose = FALSE))
  cm <- catch_msy(res$proj, n = 3000)
  expect_true(all(c("MSY_p50_lb", "n_viable") %in% names(cm)))
  v <- value_of_information(res, bins = 5)
  expect_true(all(v$inputs$S1 >= 0 & v$inputs$S1 <= 1))
  m <- run_mse(res, mps = list(SQ = mp_status_quo(), Ceil = mp_spf_ceiling(), HCR = mp_cpue_hcr()), n = 4)
  expect_equal(nrow(m$summary), 3)
  expect_true(all(m$summary$P_any_focus_overfished >= 0 & m$summary$P_any_focus_overfished <= 1))
})
