# ==============================================================================
# St. Croix case study - build PLACEHOLDER time series
# ==============================================================================
# Confidential trip-ticket (CCL), TIP, and some survey series cannot be
# distributed. This script writes clearly-flagged placeholder series with
# realistic magnitudes and structure so the full pipeline runs end-to-end.
#
# REPLACE these files with real data before using results for management:
#   landings.csv        <- CCL commercial landings by year x gear x FMP group (lb)
#   effort.csv          <- trips by gear (CCL / logbooks)
#   env_covariates.csv  <- GLORYS/MOM6 SST, NOAA CRW DHW, satellite NPP ensemble,
#                          NCRMP/TCRMP coral cover
#   indices.csv         <- SEDAR 57 / SEDAR 80 standardized CPUE, NCRMP densities
#
# Anchors that ARE public: 2021 queen conch landings (30,535 lb; NOAA ACL
# monitoring), ACL/OFL magnitudes (benchmarks.csv), hurricane years, regional
# bleaching years (2005, 2010, 2019, 2023), SCTLD arrival in St. Croix (~2021).
#
# Run from the repository root:  Rscript Projects/StCroix_Trap/data-raw/build_placeholder_timeseries.R
# ==============================================================================
set.seed(2024)
out <- "Projects/StCroix_Trap/data"
years <- 1983:2024

# ---- landings (lb, reported) -------------------------------------------------
recent_lb <- c(spiny_lobster = 110000, queen_conch = 30500, parrotfish_1 = 0,
               parrotfish_2 = 60000, surgeonfishes = 25000, queen_triggerfish = 15000,
               grunts = 20000, snappers_shallow = 12000, yellowtail_snapper = 40000,
               snappers_deep = 50000, groupers_shallow = 15000, squirrelfishes = 8000,
               goatfishes = 5000, boxfishes = 6000, angelfishes = 4000,
               wrasses_hogfish = 6000, jacks = 20000, porgies = 3000, pelagics = 90000)
gear_split <- list(
  spiny_lobster = c(trap_lobster = 0.30, trap_fish = 0.15, dive = 0.55),
  queen_conch = c(dive = 1),
  parrotfish_1 = c(trap_fish = 1),
  parrotfish_2 = c(trap_fish = 0.55, dive = 0.45),
  surgeonfishes = c(trap_fish = 0.60, dive = 0.40),
  queen_triggerfish = c(trap_fish = 0.80, trap_lobster = 0.05, hook_line = 0.05, dive = 0.10),
  grunts = c(trap_fish = 0.70, hook_line = 0.20, dive = 0.10),
  snappers_shallow = c(trap_fish = 0.40, hook_line = 0.40, dive = 0.20),
  yellowtail_snapper = c(hook_line = 0.75, trap_fish = 0.20, dive = 0.05),
  snappers_deep = c(hook_line = 1),
  groupers_shallow = c(trap_fish = 0.50, hook_line = 0.30, dive = 0.20),
  squirrelfishes = c(trap_fish = 0.70, hook_line = 0.30),
  goatfishes = c(trap_fish = 0.80, dive = 0.20),
  boxfishes = c(trap_fish = 0.90, trap_lobster = 0.10),
  angelfishes = c(trap_fish = 0.90, dive = 0.10),
  wrasses_hogfish = c(dive = 0.70, trap_fish = 0.30),
  jacks = c(hook_line = 0.70, trap_fish = 0.20, dive = 0.10),
  porgies = c(trap_fish = 0.70, hook_line = 0.30),
  pelagics = c(hook_line = 1))
# year multiplier: higher effort early 2000s, Omar 2008 dip, Maria 2017-18 collapse, COVID 2020
# year multiplier: Hugo 1989 and Marilyn 1995 collapses, high effort ~2000, Omar 2008 dip,
# Maria 2017-18 collapse, COVID 2020
yr_mult <- approx(c(1983, 1988, 1989, 1990, 1991, 1994, 1995, 1996, 1997, 2000, 2007, 2008, 2009, 2016, 2017, 2018, 2019, 2020, 2021, 2024),
                  c(1.10, 1.20, 0.75, 0.60, 0.95, 1.15, 0.80, 0.85, 1.10, 1.35, 1.25, 1.05, 1.15, 1.00, 0.80, 0.55, 0.85, 0.80, 1.00, 1.00),
                  xout = years)$y
rows <- list()
for (gr in names(recent_lb)) for (ge in names(gear_split[[gr]])) {
  v <- recent_lb[[gr]] * gear_split[[gr]][[ge]] * yr_mult * exp(rnorm(length(years), 0, 0.12))
  if (gr == "parrotfish_1") v[years >= 2011] <- 0   # prohibition
  rows[[length(rows) + 1]] <- data.frame(year = years, gear = ge, group = gr,
                                         landings_lb = round(v), data_status = "PLACEHOLDER")
}
land <- do.call(rbind, rows)
land$landings_lb[land$group == "queen_conch" & land$year == 2021] <- 30535
land$data_status[land$group == "queen_conch" & land$year == 2021] <- "published (NOAA ACL monitoring)"
write.csv(land[land$landings_lb > 0 | land$group == "parrotfish_1", ], file.path(out, "landings.csv"), row.names = FALSE)

# ---- effort (trips) ----------------------------------------------------------
base_trips <- c(trap_fish = 2200, trap_lobster = 900, dive = 1600, hook_line = 2100)
ef <- do.call(rbind, lapply(names(base_trips), function(ge) {
  m <- yr_mult
  if (ge %in% c("trap_fish", "trap_lobster")) m[years %in% c(1989:1990, 1995, 2017:2018)] <- m[years %in% c(1989:1990, 1995, 2017:2018)] * 0.7
  data.frame(year = years, gear = ge, effort = round(base_trips[[ge]] * m * exp(rnorm(length(years), 0, 0.05))),
             units = "trips", data_status = "PLACEHOLDER")
}))
write.csv(ef, file.path(out, "effort.csv"), row.names = FALSE)

# ---- environmental covariates -------------------------------------------------
sst <- 27.6 + 0.022 * (years - 2000) + rnorm(length(years), 0, 0.12)
sst[years == 2005] <- sst[years == 2005] + 0.6; sst[years == 2010] <- sst[years == 2010] + 0.5
sst[years == 2019] <- sst[years == 2019] + 0.3; sst[years == 2023] <- sst[years == 2023] + 0.8
sst[years == 2024] <- sst[years == 2024] + 0.5
dhw <- pmax(0, rnorm(length(years), 1, 0.7))
dhw[years == 2005] <- 12; dhw[years == 2010] <- 6; dhw[years == 2017] <- 3
dhw[years == 2019] <- 6; dhw[years == 2020] <- 4; dhw[years == 2023] <- 16; dhw[years == 2024] <- 10
npp <- exp(rnorm(length(years), 0, 0.08)); npp[years == 2009] <- 1.35; npp[years == 2021] <- 1.30
cc_years <- c(1983, 1989, 1990, 1995, 1996, 2000, 2004, 2006, 2009, 2013, 2015, 2017, 2019, 2021, 2023, 2024)
cc <- c(0.22, 0.20, 0.16, 0.15, 0.13, 0.15, 0.14, 0.10, 0.10, 0.08, 0.075, 0.065, 0.045, 0.045, 0.035, 0.03)
cc_status <- ifelse(cc_years < 2000, "PLACEHOLDER", "approximate")
cov <- rbind(
  data.frame(year = years, variable = "sst", value = round(sst, 3), units = "degC",
             source = "PLACEHOLDER (replace with GLORYS12 / OISST annual mean)", data_status = "PLACEHOLDER"),
  data.frame(year = years, variable = "dhw", value = round(dhw, 2), units = "degC-weeks",
             source = "PLACEHOLDER (replace with NOAA Coral Reef Watch annual max DHW)", data_status = "PLACEHOLDER"),
  data.frame(year = years, variable = "npp_pelagic_index", value = round(npp, 3), units = "relative",
             source = "PLACEHOLDER (replace with St. Croix NPP ensemble / mean)", data_status = "PLACEHOLDER"),
  data.frame(year = cc_years, variable = "coral_cover", value = cc, units = "proportion",
             source = ifelse(cc_years < 2000, "PLACEHOLDER (pre-TCRMP; post-Hugo/Marilyn losses assumed)",
                             "APPROXIMATE (TCRMP 2001-; NCRMP ~8% in 2013 to ~4% in 2019; SCTLD from 2021)"),
             data_status = cc_status))
cov$sd <- ifelse(cov$variable == "coral_cover", ifelse(cov$year < 2000, 0.03, 0.01), 0)
write.csv(cov, file.path(out, "env_covariates.csv"), row.names = FALSE)

# ---- indices (for SIR) ------------------------------------------------------
idx <- rbind(
  data.frame(year = 2012:2021, index = "SEDAR57_lobster_trap_CPUE", group = "spiny_lobster",
             value = round(exp(rnorm(10, 0, 0.15)) * c(1.0, 0.95, 0.9, 0.9, 0.95, 0.85, 0.75, 0.9, 1.0, 1.05), 3),
             cv = 0.35, lambda = 0.5, role = "validate", data_status = "PLACEHOLDER", reference = "Replace with SEDAR 57 Update St. Croix trap CPUE"),
  data.frame(year = 1985:2019, index = "SEDAR80_QT_trap_CPUE", group = "queen_triggerfish",
             value = round(exp(rnorm(35, 0, 0.15)) * seq(0.9, 1.1, length.out = 35), 3),
             cv = 0.35, lambda = 0.5, role = "validate", data_status = "PLACEHOLDER", reference = "Replace with SEDAR 80 St. Croix commercial trap index"),
  data.frame(year = seq(2013, 2023, 2), index = "NCRMP_parrotfish_density", group = "parrotfish_1;parrotfish_2",
             value = round(exp(rnorm(6, 0, 0.1)) * c(1.0, 1.05, 0.95, 0.8, 0.9, 0.85), 3),
             cv = 0.35, lambda = 0.5, role = "validate", data_status = "PLACEHOLDER", reference = "Replace with NCRMP St. Croix Scaridae biomass density"))
write.csv(idx, file.path(out, "indices.csv"), row.names = FALSE)
message("Placeholder time series written to ", out)
