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
write.csv(prior_reference_points(res), file.path(out_dir, "tables", "prior_group_reference_points.csv"),
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

# ---- robustness and out-of-sample validation ---------------------------------
tdir <- file.path(out_dir, "tables"); fdir <- file.path(out_dir, "figures")
dir.create(fdir, showWarnings = FALSE, recursive = TRUE)
sv <- function(p, f, w = 10, h = 6) ggplot2::ggsave(file.path(fdir, f), p, width = w, height = h, dpi = 200, bg = "#fcfcfb")
diag <- sir_diagnostics(res)
write.csv(diag$summary, file.path(tdir, "sir_diagnostics_summary.csv"), row.names = FALSE)
write.csv(diag$shrinkage, file.path(tdir, "posterior_shrinkage.csv"), row.names = FALSE)
cat("\n=== SIR diagnostics ===\n"); print(diag$summary)
sa <- sensitivity_analysis(res)
write.csv(sa, file.path(tdir, "global_sensitivity.csv"), row.names = FALSE)
for (o in unique(sa$output)[1:3]) sv(plot_sensitivity(sa, o), sprintf("fig11_sensitivity_%s.png", o), 9, 6)
stt <- self_test(res, n_rep = 100)
write.csv(stt$summary, file.path(tdir, "self_test_summary.csv"), row.names = FALSE)
cat("\n=== Self-test ===\n"); print(stt$summary)
val <- tryCatch(validate_out_of_sample(res), error = function(e) NULL)
if (!is.null(val)) {
  write.csv(val$summary, file.path(tdir, "validation_out_of_sample.csv"), row.names = FALSE)
  write.csv(val$observations, file.path(tdir, "validation_predictions.csv"), row.names = FALSE)
  sv(plot_validation(val), "fig12_validation.png", 11, 5)
  cat("\n=== Out-of-sample validation (held-out indices) ===\n"); print(val$summary)
}
lb <- loo_benchmarks(res)
write.csv(lb, file.path(tdir, "validation_loo_benchmarks.csv"), row.names = FALSE)
sv(plot_loo_benchmarks(lb), "fig13_loo_benchmarks.png", 9, 3.5)
cat("\n=== Leave-one-benchmark-out ===\n"); print(lb)
eo <- ecosystem_overfishing_indices(res)
write.csv(eo, file.path(tdir, "ecosystem_overfishing_indices.csv"), row.names = FALSE)
sv(plot_eco_indices(eo), "fig14_eco_indices.png", 9, 6)
cat("\n=== Ecosystem-overfishing indices (2022-2024) ===\n"); print(eo[eo$year %in% 2022:2024, ], row.names = FALSE)

# ---- independent comparator, value of information, MSE ------------------------
set.seed(11)
mc <- method_comparison(res, catch_msy(proj, n = 50000))
write.csv(mc, file.path(tdir, "method_comparison_catch_only.csv"), row.names = FALSE)
cat("\n=== TTM vs catch-only MSY (CMSY-style) ===\n"); print(mc[, c("group", "CMSY_p50_lb", "MMSY_p50_lb", "OFL_lb", "ratio_TTM_to_catch_only")], row.names = FALSE)
sv(plot_method_comparison(mc), "fig16_method_comparison.png", 9, 6)
voi <- value_of_information(res)
write.csv(voi$inputs, file.path(tdir, "value_of_information_inputs.csv"), row.names = FALSE)
write.csv(voi$streams, file.path(tdir, "value_of_information_streams.csv"), row.names = FALSE)
sv(plot_voi(voi), "fig17_value_of_information.png", 10, 6)
cat("\n=== Value of information (total focus MMSY) ===\n"); print(head(voi$streams[voi$streams$output == "total_focus_MMSY", ], 8), row.names = FALSE)
mse <- run_mse(res, n = 150)
write.csv(mse$summary, file.path(tdir, "mse_performance.csv"), row.names = FALSE)
write.csv(mse$by_group, file.path(tdir, "mse_performance_by_group.csv"), row.names = FALSE)
sv(plot_mse(mse), "fig18_mse.png", 10, 6)
cat("\n=== Closed-loop MSE (2036-2045) ===\n"); print(mse$summary, row.names = FALSE)
pdx <- process_diagnostics(res)
write.csv(pdx, file.path(tdir, "process_diagnostics.csv"), row.names = FALSE)
cat("\n=== Ecological process diagnostics (Baseline, medians) ===\n"); print(pdx, row.names = FALSE)

# ---- figures -----------------------------------------------------------------
save_standard_plots(res, file.path(out_dir, "figures"))
res$pars <- NULL   # parameter draws are large; drop before saving
saveRDS(res, file.path(out_dir, "ttm_results.rds"))
message("Done. Outputs in ", out_dir)
