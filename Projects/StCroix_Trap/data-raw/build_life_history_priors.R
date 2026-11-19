# ==============================================================================
# Life-history priors for x_msy (MSY / (M B0)) and natural mortality M
# ==============================================================================
# Inputs : data-raw/fishbase/fishbase_species_traits.csv (FishBase v24.07: K, Linf,
#          length-weight a/b, Then et al. 2015 M), group species weights.
#          Spiny lobster from SEDAR 57 (not in FishBase).
# Method : per-recruit Beverton-Holt equilibrium (R/life_history.R) with trap
#          retention (fish traps: L50 ~ 15 cm TL, capped at 0.6 Linf, CV 0.3 -
#          assumption for 1.5-2 in. mesh; lobster: 8.9 cm CL legal minimum),
#          hook-and-line groups retained at maturity, steepness h ~ N(0.75, 0.1).
# Output : data-raw/life_history/xmsy_priors.csv; updates groups.csv columns
#          x_msy, x_msy_cv, m_nat, m_nat_cv.
#   Rscript Projects/StCroix_Trap/data-raw/build_life_history_priors.R   (from repo root)
# ==============================================================================
if (requireNamespace("CaribTTM", quietly = TRUE)) library(CaribTTM) else devtools::load_all(".", quiet = TRUE)
set.seed(2024)
pd <- "Projects/StCroix_Trap"
sp <- read.csv(file.path(pd, "data-raw/fishbase/fishbase_species_traits.csv"))
sp <- data.frame(group = sp$group, species = sp$species, M = sp$M_then, K = sp$K, linf = sp$Linf_used,
                 a = sp$a, b = sp$b, weight = sp$weight, lm = NA)
lob <- data.frame(group = "spiny_lobster", species = "Panulirus argus", M = 0.34, K = 0.24, linf = 18.5,
                  a = 0.0035, b = 2.75, weight = 1, lm = 8.0)   # SEDAR 57: M 0.34; CL (cm) growth/maturity
sp <- rbind(sp, lob)
hook <- c("snappers_deep", "yellowtail_snapper", "jacks", "pelagics")
lc <- do.call(rbind, lapply(split(sp, sp$group), function(x) {
  li <- stats::median(x$linf, na.rm = TRUE)
  if (x$group[1] == "spiny_lobster") return(data.frame(group = x$group[1], lc = 8.9, lc_cv = 0.05))
  if (x$group[1] %in% hook) return(data.frame(group = x$group[1], lc = lm_from_linf(li), lc_cv = 0.2))
  data.frame(group = x$group[1], lc = min(15, 0.6 * li), lc_cv = 0.3)
}))
lh <- life_history_xmsy(sp, lc, n = 400)
lh <- merge(lh, lc, by = "group")
gm <- stats::aggregate(M ~ group, data = sp[is.finite(sp$M), ], FUN = function(z) exp(mean(log(z))))
lh <- merge(lh, gm, by = "group", all.x = TRUE)
write.csv(lh, file.path(pd, "data-raw/life_history/xmsy_priors.csv"), row.names = FALSE)
print(lh, digits = 3)

g <- read.csv(file.path(pd, "data/groups.csv"), stringsAsFactors = FALSE, check.names = FALSE)
for (nm in c("m_nat", "m_nat_cv")) if (!nm %in% names(g)) g[[nm]] <- NA
fallback_x <- stats::median(lh$x_msy)
for (i in which(g$group_type == "fmp" & g$group != "queen_conch")) {
  k <- match(g$group[i], lh$group)
  tag <- "x_msy, M: per-recruit life-history prior (data-raw/build_life_history_priors.R)"
  if (!is.na(k)) {
    g$x_msy[i] <- round(lh$x_msy[k], 3); g$x_msy_cv[i] <- round(max(0.2, lh$x_msy_cv[k]), 2)
    g$m_nat[i] <- round(lh$M[k], 3); g$m_nat_cv[i] <- if (g$group[i] == "spiny_lobster") 0.2 else 0.3
  } else {
    g$x_msy[i] <- round(fallback_x, 3); g$x_msy_cv[i] <- 0.4
    tag <- "x_msy: median per-recruit prior of other groups (no FishBase growth data)"
  }
  if (!grepl("per-recruit", g$reference[i])) g$reference[i] <- paste0(g$reference[i], "; ", tag)
}
write.csv(g, file.path(pd, "data/groups.csv"), row.names = FALSE, na = "")
