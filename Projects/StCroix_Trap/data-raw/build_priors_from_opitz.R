# ==============================================================================
# Derive partition (biomass) and P/B priors from Opitz (1996) Caribbean reef
# Ecopath models (ICLARM Tech. Rep. 43; Puerto Rico - Virgin Islands shelf).
#
# * FMP fish groups: species biomass in Virgin Islands reef environments
#   (Table 3.6), summed to St. Croix FMP complexes and multiplied by Opitz's own
#   mass-balance adjustment for reef fish (balanced / raw biomass of fish groups
#   1-27 in Table 3.22 = 123.9 / 519.3 = 0.239). Prohibited / floor-density
#   giants (goliath and Nassau grouper, large sharks) are excluded.
# * Spiny lobster and non-fishery groups: balanced 50-box model (Table 3.22).
# * P/B: balanced Opitz group P/B matched to each model group (biomass-weighted).
#
# Densities are g m-2 = t km-2 of reef; they are used as RELATIVE partition
# weights (x P/B) across groups, so a common reef-area basis is what matters.
# Run from the repository root:
#   Rscript Projects/StCroix_Trap/data-raw/build_priors_from_opitz.R
# ==============================================================================
dir <- "Projects/StCroix_Trap"
sp <- read.csv(file.path(dir, "data-raw/opitz1996/opitz_table3_6_fmp_species.csv"))
op <- read.csv(file.path(dir, "data-raw/opitz1996/opitz_table3_22_groups.csv"))
balance <- 123.9 / 519.3

fish <- aggregate(biomass_g_m2_raw ~ fmp_group, data = sp, FUN = sum)
fish$biomass <- fish$biomass_g_m2_raw * balance

nf <- op[op$model_group != "", ]
nf$prod <- nf$biomass_g_m2 * nf$pb
nonfish <- do.call(rbind, lapply(split(nf, nf$model_group), function(x)
  data.frame(group = x$model_group[1], biomass = sum(x$biomass_g_m2), pb = sum(x$prod) / sum(x$biomass_g_m2))))

# P/B for FMP groups from matching balanced Opitz groups (documented choices)
pb_fmp <- c(spiny_lobster = 1.00,        # Lobsters
            parrotfish_1 = 0.38,          # Large Scaridae
            parrotfish_2 = (5.510 * 1.155 + 5.075 * 0.940) / (5.510 + 5.075),  # Intermediate + small Scaridae
            surgeonfishes = 0.71,         # Intermediate reef fish, herbivorous
            groupers_shallow = 0.46,      # Large groupers (0.37) / intermediate carnivores 4 (0.55)
            jacks = 1.00,                 # Intermediate (1.17) and small (0.83) jacks
            snappers_shallow = 0.64,      # Intermediate reef fish, carnivorous 3
            yellowtail_snapper = 0.64)

g <- read.csv(file.path(dir, "data/groups.csv"), stringsAsFactors = FALSE, check.names = FALSE)
upd <- function(grp, val, pb = NA, status, ref) {
  i <- which(g$group == grp); if (!length(i)) return(invisible())
  g$partition_value[i] <<- signif(val, 3)
  if (!is.na(pb)) g$pb[i] <<- signif(pb, 3)
  g$data_status[i] <<- status
  g$reference[i] <<- ref
}
for (k in seq_len(nrow(fish))) {
  grp <- fish$fmp_group[k]
  upd(grp, fish$biomass[k], if (grp %in% names(pb_fmp)) pb_fmp[[grp]] else NA,
      "derived: Opitz (1996) VI species biomass x mass-balance factor",
      sprintf("Opitz 1996 Table 3.6 (raw %.2f g/m2) x 0.239 (Table 3.22 balance)", fish$biomass_g_m2_raw[k]))
}
upd("spiny_lobster", 3.27, pb_fmp[["spiny_lobster"]], "derived: Opitz (1996) balanced model",
    "Opitz 1996 Table 3.22 Lobsters (B 3.27 g/m2, P/B 1.0); SEDAR 57 M ~0.34")
for (k in seq_len(nrow(nonfish)))
  upd(nonfish$group[k], nonfish$biomass[k], nonfish$pb[k], "derived: Opitz (1996) balanced model",
      "Opitz 1996 Table 3.22 (biomass-weighted P/B of matched groups)")
write.csv(g, file.path(dir, "data/groups.csv"), row.names = FALSE, na = "")
print(merge(rbind(data.frame(group = fish$fmp_group, biomass = fish$biomass), nonfish[, 1:2]),
            g[, c("group", "pb")], by = "group"))

# ==============================================================================
# Evidence synthesis with FishBase (data-raw/fishbase/fishbase_group_traits.csv;
# built by build_fishbase_traits.py)
#  1. P/B prior = geometric mean of Opitz balanced P/B and FishBase empirical M
#     (Then et al. 2015); CV = max(0.25, |log(ratio)| / 2)
#  2. Diet shifted up the TL pools so the implied TL matches FishBase TL
#  3. Thermal responses: Gaussian performance curves from AquaMaps temperature
#     preferences (t_opt = mean, width = (max - min) / 2)
# ==============================================================================
fb <- read.csv(file.path(dir, "data-raw/fishbase/fishbase_group_traits.csv"))
g <- read.csv(file.path(dir, "data/groups.csv"), stringsAsFactors = FALSE, check.names = FALSE)
for (k in seq_len(nrow(fb))) {
  i <- which(g$group == fb$group[k]); if (!length(i) || is.na(fb$M_then[k])) next
  pb_op <- g$pb[i]; m <- fb$M_then[k]
  g$pb[i] <- signif(exp(mean(log(c(pb_op, m)))), 3)
  g$pb_cv[i] <- round(max(0.25, abs(log(pb_op / m)) / 2), 2)
  g$reference[i] <- paste0(g$reference[i], sprintf("; P/B = geomean(Opitz %.2f, FishBase M_Then %.2f)", pb_op, m))
}
write.csv(g, file.path(dir, "data/groups.csv"), row.names = FALSE, na = "")

d <- read.csv(file.path(dir, "data/diet.csv"), stringsAsFactors = FALSE, check.names = FALSE)
pools_b <- paste0("B_TL", 1:4); pools_p <- paste0("P_TL", 1:4)
tl_of <- function(v) 1 + sum(v * rep(1:4, 2))
shift_up <- function(v) {   # move each pool's mass one TL up (TL4 stays)
  o <- v * 0
  for (pp in list(1:4, 5:8)) { o[pp[2:4]] <- o[pp[2:4]] + v[pp[1:3]]; o[pp[4]] <- o[pp[4]] + v[pp[4]] }
  o
}
for (k in seq_len(nrow(fb))) {
  i <- which(d$group == fb$group[k]); if (!length(i) || is.na(fb$troph[k])) next
  v <- as.numeric(d[i, c(pools_b, pools_p)]); v <- v / sum(v)
  t0 <- tl_of(v); target <- fb$troph[k]
  if (target > t0 + 0.05) {
    a <- 0
    for (step in 1:3) {           # up to three successive shifts
      vs <- shift_up(v); gain <- tl_of(vs) - tl_of(v)
      if (gain <= 1e-6) break
      a <- min(1, (target - tl_of(v)) / gain)
      v <- (1 - a) * v + a * vs
      if (tl_of(v) >= target - 0.01) break
    }
    d[i, c(pools_b, pools_p)] <- round(v, 3)
    d$reference[i] <- sprintf("%s; shifted to FishBase TL %.2f (was %.2f)", d$reference[i], target, t0)
  }
}
write.csv(d, file.path(dir, "data/diet.csv"), row.names = FALSE, na = "")

er <- read.csv(file.path(dir, "data/env_response.csv"), stringsAsFactors = FALSE)
er <- er[!(er$variable == "sst"), , drop = FALSE]
th <- fb[!is.na(fb$t_opt) & fb$group != "snappers_deep", ]   # deepwater snappers live at 100-300 m: surface SST not applicable
add <- data.frame(group = th$group, variable = "sst", type = "thermal_performance", target = "production",
                  baseline = NA, sensitivity = NA, sensitivity_cv = 0, t_opt = th$t_opt,
                  t_width = round((th$t_pref_max - th$t_pref_min) / 2, 2), threshold = NA,
                  data_status = "derived: FishBase/AquaMaps temperature preference",
                  reference = sprintf("AquaMaps (via FishBase v24.07) TempPref min/mean/max = %.1f/%.1f/%.1f degC; width = (max-min)/2",
                                      th$t_pref_min, th$t_opt, th$t_pref_max))
er <- rbind(er, add[, names(er)])
write.csv(er, file.path(dir, "data/env_response.csv"), row.names = FALSE, na = "")
message("Applied FishBase synthesis to groups.csv, diet.csv, env_response.csv")
