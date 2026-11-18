# ==============================================================================
# St. Croix trap fishery case study - CaribTTM
# ==============================================================================
# Run from the repository root:
#   Rscript Projects/StCroix_Trap/run_stcroix.R            # full run (settings n_draws)
#   Rscript Projects/StCroix_Trap/run_stcroix.R 100        # quick run with 100 draws
#
# Outputs (Projects/StCroix_Trap/output/):
#   tables/  SPF hand-off tables, reference points, indicators, benchmark check
#   figures/ standard figure set
#   ttm_results.rds  full results object (all draws, all scenarios)
# ==============================================================================
args <- commandArgs(trailingOnly = TRUE)
if (requireNamespace("CaribTTM", quietly = TRUE)) {
  library(CaribTTM)
} else {
  devtools::load_all(".", quiet = TRUE)   # from a source checkout
}

proj_dir <- "Projects/StCroix_Trap"
out_dir  <- file.path(proj_dir, "output")
proj <- read_ttm_project(proj_dir)
if (length(args)) proj$settings$n_draws <- proj$settings$sir_resample <- as.integer(args[1])
print(proj)

t0 <- Sys.time()
res <- run_ttm(proj)
message(sprintf("Run time: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
print(res)

# ---- tables for SPF collaborators + diagnostics ------------------------------
files <- write_spf_inputs(res, file.path(out_dir, "tables"))
write.csv(reference_points(prior_results(res)), file.path(out_dir, "tables", "prior_group_reference_points.csv"),
          row.names = FALSE)

# ---- headline summaries -------------------------------------------------------
rp <- reference_points(res)
cat("\n=== Baseline reference points (2022-2024), FMP groups ===\n")
show <- rp[rp$group_type == "fmp", c("group", "MMSY_p05_lb", "MMSY_p50_lb", "MMSY_p95_lb",
                                     "target_lb", "catch_lb", "B_over_BMSY", "P_overfishing", "status")]
show[, 2:6] <- round(show[, 2:6]); show$B_over_BMSY <- round(show$B_over_BMSY, 2)
print(show, row.names = FALSE)
cat("\n=== Guild MMSY (lb) ===\n")
print(aggregate_mmsy(res, "guild")[, c("name", "MMSY_p50_lb", "catch_lb", "P_overfishing")], row.names = FALSE)
cat("\n=== Gear effort ceilings ===\n")
print(gear_allocation(res)$effort_ceilings[, c("gear", "effort_multiplier_p50", "effort_multiplier_target",
                                              "choke_group", "P_choke")], row.names = FALSE)
cat("\n=== Benchmark comparison ===\n")
print(benchmark_comparison(res)[, c("group", "metric", "value_lb", "MMSY_p50_lb", "ratio_MMSYp50_to_benchmark")],
      row.names = FALSE)

# ---- figures -----------------------------------------------------------------
save_standard_plots(res, file.path(out_dir, "figures"))
saveRDS(res, file.path(out_dir, "ttm_results.rds"))
message("Done. Outputs in ", out_dir)
