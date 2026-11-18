test_that("ERDDAP URL builder handles strides, extra dims and descending latitude", {
  u <- erddap_griddap_url("https://x.gov/erddap/", "ds", "sst", c("2020-01-01", "2020-12-31"),
                          c(-65, 17.6, -64.4, 17.95), extra_dims = list(zlev = 0),
                          stride = c(time = 7, lat = 2, lon = 3), lat_descending = TRUE)
  dec <- utils::URLdecode(u)
  expect_match(dec, "^https://x.gov/erddap/griddap/ds.csv\\?sst\\[")
  expect_match(dec, "\\[\\(2020-01-01\\):7:\\(2020-12-31\\)\\]\\[\\(0\\)\\]", fixed = FALSE)
  expect_match(dec, "\\[\\(17.95\\):2:\\(17.6\\)\\]\\[\\(-65\\):3:\\(-64.4\\)\\]")
})

test_that("ERDDAP CSV parsing and annualization (mean / max with spatial sd)", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("time,latitude,longitude,dhw", "UTC,degrees_north,degrees_east,degC-weeks",
               "2023-01-01T12:00:00Z,17.7,-64.8,1", "2023-01-01T12:00:00Z,17.8,-64.8,3",
               "2023-09-01T12:00:00Z,17.7,-64.8,15", "2023-09-01T12:00:00Z,17.8,-64.8,17",
               "2024-01-02T12:00:00Z,17.7,-64.8,NaN"), f)
  g <- parse_erddap_csv(f, "dhw")
  expect_equal(nrow(g), 5)
  a <- annualize_grid(g, "max")
  expect_equal(a$year, 2023)
  expect_equal(a$value, 16, tolerance = 1e-3)
  expect_equal(a$sd, 1, tolerance = 1e-2)
  expect_equal(annualize_grid(g, "mean")$value, 9, tolerance = 1e-3)
})

test_that("HURDAT2 parsing and hurricane disturbance intensities", {
  txt <- c("AL152017,              MARIA,     3,",
           "20170919, 1800,  , HU, 17.3N,  64.7W, 150,  909,",
           "20170920, 0000,  , HU, 17.6N,  65.1W, 150,  910,",
           "20170921, 0000,  , HU, 18.5N,  66.5W, 100,  950,",
           "AL991999,            FARAWAY,     1,",
           "19990901, 0000,  , TS, 30.0N,  40.0W,  50, 1000,")
  tr <- parse_hurdat2(txt)
  expect_equal(nrow(tr), 4)
  expect_equal(tr$lon[1], -64.7)
  tmpl <- data.frame(year = NA, event = "major_hurricane", intensity = 1, habitat = "reef", p1_loss = 0.25,
                     quality_loss = 0.15, half_life_yr = 2, gear = "trap", effort_loss = 0.6,
                     effort_half_life_yr = 1.5, group = NA, production_loss = 0)
  ev <- hurricane_disturbances(tr, c(-65, 17.6, -64.4, 17.95), 1990:2020, tmpl)
  expect_equal(nrow(ev), 1)
  expect_equal(ev$year, 2017)
  expect_equal(ev$intensity, 1)            # Cat-5 winds within the core radius
  expect_equal(ev$intensity_cv, 0.3)
  expect_equal(ev$p1_loss, 0.25)           # losses copied from template
})

test_that("habitat class mapping sums classes into model habitats", {
  a <- data.frame(class = c("Reef/Colonized Pavement", "Reef/Linear Reef", "Seagrass/Continuous",
                            "Seagrass/Patchy/10-30%", "Land", "Sand"),
                  area_km2 = c(80, 12, 13, 8, 215, 11), n_polygons = 1)
  m <- map_habitat_classes(a)
  expect_equal(m$area_km2[m$habitat == "seagrass"], 21)
  expect_equal(m$area_km2[m$habitat == "colonized_pavement"], 80)
  expect_equal(m$area_km2[m$habitat == "coral_reef"], 12)
  expect_false("Land" %in% m$habitat)
})

test_that("acquired data merge respects user overrides and placeholders", {
  td <- tempfile(); write_ttm_templates(td)
  h <- read.csv(file.path(td, "habitats.csv")); h$area_km2[2] <- NA       # seagrass blank -> filled
  write.csv(h, file.path(td, "habitats.csv"), row.names = FALSE, na = "")
  cv <- read.csv(file.path(td, "env_covariates.csv")); cv$data_status <- ifelse(cv$variable == "sst", "PLACEHOLDER", "observed")
  write.csv(cv, file.path(td, "env_covariates.csv"), row.names = FALSE)
  dir.create(file.path(td, "acquired"))
  write.csv(data.frame(habitat = c("reef", "seagrass"), area_km2 = c(999, 42), area_cv = 0.15,
                       npp_value = NA, npp_unit = NA, npp_cv = NA),
            file.path(td, "acquired", "habitats.csv"), row.names = FALSE, na = "")
  write.csv(data.frame(year = 2010:2020, variable = "sst", value = 29, sd = 0.1, units = "degC",
                       source = "API", data_status = "API"),
            file.path(td, "acquired", "env_covariates.csv"), row.names = FALSE)
  write.csv(rbind(data.frame(year = 2017, event = "API storm", intensity = 0.9),
                  data.frame(year = 2012, event = "API storm 2", intensity = 0.3)),
            file.path(td, "acquired", "disturbances.csv"), row.names = FALSE)
  p <- read_ttm_project(td)
  expect_equal(p$habitats$area_km2[p$habitats$habitat == "reef"], 50)       # user value wins
  expect_equal(p$habitats$area_km2[p$habitats$habitat == "seagrass"], 42)   # blank filled
  expect_true(all(p$env_covariates$value[p$env_covariates$variable == "sst"] == 29))  # placeholder replaced
  expect_true(all(p$env_covariates$value[p$env_covariates$variable == "dhw"] == cv$value[cv$variable == "dhw"]))
  expect_false("API storm" %in% p$disturbances$event)                       # user year 2017 kept
  expect_true("API storm 2" %in% p$disturbances$event)
  expect_error(read_ttm_project(td, use_acquired = FALSE), "blank")
})

test_that("observation error in data inputs propagates to simulations", {
  p <- ttm_example_project()
  p$landings$cv <- 0.3; p$effort$cv <- 0.2
  p$env_covariates$sd <- ifelse(p$env_covariates$variable == "sst", 0.3, 0)
  p$disturbances$intensity_cv <- 0.4
  p <- validate_ttm_project(p)
  expect_true(has_driver_uncertainty(p))
  set.seed(1); a <- draw_parameters(p); b <- draw_parameters(p)
  expect_false(isTRUE(all.equal(a$landings_mult, b$landings_mult)))
  da <- build_drivers(p, "Baseline", a); db <- build_drivers(p, "Baseline", b)
  expect_false(isTRUE(all.equal(da$cov[, "sst"], db$cov[, "sst"])))
  expect_false(isTRUE(all.equal(da$p1_mult[, "reef"], db$p1_mult[, "reef"])))
  oa <- simulate_ttm(p, a, da); ob <- simulate_ttm(p, b, db)
  expect_false(isTRUE(all.equal(oa$C["2015", "lobster"], ob$C["2015", "lobster"])))
  d0 <- draw_parameters(p, stochastic = FALSE)
  expect_equal(unname(d0$landings_mult), rep(1, nrow(p$landings)))
  res <- suppressWarnings(run_ttm(p, n_draws = 10, verbose = FALSE, sir = FALSE))
  expect_true(all(is.finite(res$results$Baseline$MMSY)))
})
