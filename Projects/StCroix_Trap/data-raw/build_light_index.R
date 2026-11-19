# ==============================================================================
# Benthic light index for light-limited habitats (seagrass, macroalgae)
# ==============================================================================
# Kd(PAR) = 0.0864 + 0.884 Kd490 (Morel et al. 2007); bottom light fraction
# exp(-Kd(PAR) z) at a representative depth z, expressed relative to its
# 1998-2024 mean. Production elasticity to bottom light = 1 (light-limited
# benthic primary production; Duarte 1991 for seagrass depth limits).
# ASSUMPTIONS: representative depths 4 m (seagrass) and 8 m (macroalgae);
# years before OC-CCI (1983-1997) held at 1 with the interannual SD.
# Input : data/acquired/env_covariates.csv (kd490, API)
# Output: rows light_seagrass / light_macroalgae appended to data/env_covariates.csv
#   Rscript Projects/StCroix_Trap/data-raw/build_light_index.R   (from repo root)
# ==============================================================================
pd <- "Projects/StCroix_Trap/data"
acq <- read.csv(file.path(pd, "acquired/env_covariates.csv"), stringsAsFactors = FALSE)
kd <- acq[acq$variable == "kd490", ]
ec <- read.csv(file.path(pd, "env_covariates.csv"), stringsAsFactors = FALSE)
ec <- ec[!ec$variable %in% c("light_seagrass", "light_macroalgae"), ]
depth <- c(light_seagrass = 4, light_macroalgae = 8)
yrs <- 1983:2024
for (v in names(depth)) {
  z <- depth[[v]]
  kpar <- 0.0864 + 0.884 * kd$value
  li <- exp(-kpar * z); rel <- li / mean(li)
  # delta-method SD from the spatial SD of Kd490
  sdr <- rel * 0.884 * z * kd$sd
  pre <- setdiff(yrs, kd$year)
  ec <- rbind(ec, data.frame(year = c(pre, kd$year), variable = v,
    value = round(c(rep(1, length(pre)), rel), 4),
    units = "relative bottom light (1998-2024 mean = 1)",
    source = c(rep("held at mean before OC-CCI (assumption)", length(pre)),
               rep(sprintf("derived: OC-CCI Kd490 (API) -> Kd(PAR) (Morel et al. 2007), depth %g m (assumption)", z), nrow(kd))),
    data_status = c(rep("ASSUMPTION", length(pre)), rep("derived", nrow(kd))),
    sd = round(c(rep(stats::sd(rel), length(pre)), pmax(sdr, 0.005)), 4))[, names(ec)])
}
write.csv(ec, file.path(pd, "env_covariates.csv"), row.names = FALSE)
h <- read.csv(file.path(pd, "habitats.csv"), stringsAsFactors = FALSE, check.names = FALSE)
h$npp_covariate[h$habitat == "seagrass"] <- "light_seagrass"
h$npp_covariate[h$habitat == "macroalgae"] <- "light_macroalgae"
write.csv(h, file.path(pd, "habitats.csv"), row.names = FALSE, na = "")
print(range(ec$value[ec$variable == "light_seagrass"]))
