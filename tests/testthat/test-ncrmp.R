test_that("NCRMP stratified biomass estimator recovers known densities", {
  set.seed(7)
  smap <- data.frame(species_cd = c("EPI GUTT", "HAE FLAV"), group = c("groupers", "grunts"),
                     a = c(0.01, 0.02), b = c(3, 3))
  mk <- function(strat, n_psu, mu) do.call(rbind, lapply(seq_len(n_psu), function(i)
    data.frame(YEAR = 2019, PRIMARY_SAMPLE_UNIT = paste(strat, i), STATION_NR = 1, STRAT = strat,
               SPECIES_CD = c("EPI GUTT", "HAE FLAV"), NUM = c(rpois(1, mu), rpois(1, 3 * mu)), LEN = c(30, 15))))
  d <- rbind(mk("A", 30, 2), mk("B", 30, 6))
  f <- tempfile(fileext = ".csv"); write.csv(d, f, row.names = FALSE)
  fish <- read_ncrmp_fish(f)
  expect_equal(nrow(fish), 120)
  strata <- data.frame(strat = c("A", "B"), ntot = c(300, 100))
  b <- ncrmp_stratified_biomass(fish, smap, strata)
  # expected groupers density: (0.75*2 + 0.25*6) * 0.01*30^3 / 177
  exp_g <- (0.75 * 2 + 0.25 * 6) * 0.01 * 30^3 / 177
  got <- b$density_g_m2[b$group == "groupers"]
  expect_lt(abs(got - exp_g) / exp_g, 0.25)
  expect_true(all(b$se > 0))
  io <- ncrmp_to_inputs(b, role = "validate")
  expect_true(all(io$indices$role == "validate"))
  expect_equal(nrow(io$priors), 2)
})
