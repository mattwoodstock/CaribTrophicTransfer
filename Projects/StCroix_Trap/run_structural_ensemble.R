# ==============================================================================
# St. Croix structural-uncertainty ensemble
# ==============================================================================
# Runs the Baseline under alternative structural assumptions (MMSY fraction,
# transfer efficiency, fishing feedback, thermal effects, food-web fraction,
# pelagic NPP source) and pools the SIR-conditioned MMSY with equal weight per
# variant. Variants run in parallel on Unix (parallel::mclapply).
#
#   Rscript Projects/StCroix_Trap/run_structural_ensemble.R [draws_per_round] [cores]
# ==============================================================================
args <- commandArgs(trailingOnly = TRUE)
if (requireNamespace("CaribTTM", quietly = TRUE)) library(CaribTTM) else devtools::load_all(".", quiet = TRUE)
n_draws <- if (length(args) >= 1) as.integer(args[1]) else 1500
cores <- if (length(args) >= 2) as.integer(args[2]) else max(1, parallel::detectCores() - 1)

proj <- read_ttm_project("Projects/StCroix_Trap")
proj$settings$sir_adaptive_rounds <- 3
proj$settings$cores <- 1   # parallelism is across variants
proj$settings$sir_resample <- 1000
variants <- default_structural_variants()
out_dir <- "Projects/StCroix_Trap/output"
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)

t0 <- Sys.time()
one <- function(nm) {
  set.seed(1)
  e <- run_structural_ensemble(proj, variants[nm], n_draws = n_draws, verbose = FALSE)
  e$results <- NULL
  e
}
parts <- parallel::mclapply(names(variants), one, mc.cores = cores)
names(parts) <- names(variants)
ok <- !vapply(parts, inherits, logical(1), "try-error")
by_var <- do.call(rbind, lapply(parts[ok], `[[`, "by_variant"))
message(sprintf("Ensemble run time: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

# pooled ensemble: equal weight per variant, using each variant's quantiles via log-normal approximation
g <- unique(by_var$group)
pooled <- do.call(rbind, lapply(g, function(gi) {
  d <- by_var[by_var$group == gi & is.finite(by_var$MMSY_p50_t) & by_var$MMSY_p50_t > 0, ]
  sims <- unlist(lapply(seq_len(nrow(d)), function(i) {
    mu <- log(d$MMSY_p50_t[i]); sg <- (log(d$MMSY_p95_t[i]) - log(pmax(d$MMSY_p05_t[i], 1e-9))) / (2 * 1.645)
    exp(stats::rnorm(2000, mu, max(sg, 1e-3)))
  }))
  q <- stats::quantile(sims, c(0.05, 0.5, 0.95))
  data.frame(group = gi, MMSY_p05_lb = t_to_lb(q[1]), MMSY_p50_lb = t_to_lb(q[2]), MMSY_p95_lb = t_to_lb(q[3]),
             between_variant_cv = stats::sd(d$MMSY_p50_t) / mean(d$MMSY_p50_t))
}))
write.csv(by_var, file.path(out_dir, "tables", "structural_ensemble_by_variant.csv"), row.names = FALSE)
write.csv(pooled, file.path(out_dir, "tables", "structural_ensemble_pooled.csv"), row.names = FALSE)
ens <- list(by_variant = by_var, pooled = pooled)
ggplot2::ggsave(file.path(out_dir, "figures", "fig15_structural_ensemble.png"),
                plot_ensemble(ens), width = 11, height = 7, dpi = 200, bg = "#fcfcfb")
print(pooled[pooled$group %in% c("spiny_lobster", "queen_triggerfish", "parrotfish_2", "grunts"), ], row.names = FALSE)
print(unique(by_var[, c("variant", "ess")]))
