# ==============================================================================
# Build the CaribTTM Technical Memorandum
# ==============================================================================
# Assembles docs/technical_memo/sections/*.md, replaces {{AUTO:key}} tags with
# tables generated from the St. Croix project inputs and run outputs, replaces
# {{INCLUDE:file}} with other documentation pages (headings demoted), and
# renders Markdown, Word (.docx) and PDF (via LibreOffice) versions.
#   Rscript docs/technical_memo/build_memo.R      (from the repository root)
# ==============================================================================
if (requireNamespace("CaribTTM", quietly = TRUE)) library(CaribTTM) else devtools::load_all(".", quiet = TRUE)
md <- "docs/technical_memo"; pd <- "Projects/StCroix_Trap"; td <- file.path(pd, "output/tables")
out_md <- file.path(md, "CaribTTM_Technical_Memorandum.md")

fmt_num <- function(x, d = NULL) {
  if (!is.numeric(x)) return(as.character(x))
  vapply(x, function(v) {
    if (is.na(v)) return("")
    if (!is.null(d)) return(formatC(v, format = "f", digits = d, big.mark = ","))
    a <- abs(v)
    if (a >= 1000) formatC(v, format = "f", digits = 0, big.mark = ",") else
    if (a >= 10) formatC(v, format = "f", digits = 1) else
    if (a >= 0.01 || a == 0) formatC(v, format = "f", digits = 3) else formatC(v, format = "e", digits = 2)
  }, character(1))
}
tab <- function(df, digits = NULL, caption = NULL) {
  if (is.null(df) || !nrow(df)) return("*(not available: run the case study first)*")
  df[] <- lapply(names(df), function(n) { x <- df[[n]]; if (is.numeric(x)) fmt_num(x, digits[[n]]) else { x <- as.character(x); x[is.na(x)] <- ""; gsub("\\|", "/", x) } })
  c(if (!is.null(caption)) c(paste0("**", caption, "**"), ""),
    paste("|", paste(names(df), collapse = " | "), "|"),
    paste("|", paste(rep("---", ncol(df)), collapse = " | "), "|"),
    apply(df, 1, function(r) paste("|", paste(r, collapse = " | "), "|")), "")
}
rd <- function(f, dir = td) { p <- file.path(dir, f); if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) else NULL }
fig <- function(f, cap) {
  p <- file.path("../../Projects/StCroix_Trap/output/figures", f)
  if (!file.exists(file.path(pd, "output/figures", f))) return(paste0("*(figure ", f, " not available)*"))
  c(paste0("![", cap, "](", p, "){width=100%}"), "")
}

proj <- read_ttm_project(pd)
g <- proj$groups; h <- proj$habitats; foc <- focus_groups(proj)
auto <- list()
# ---- inputs ----
auto$in_habitats <- tab(data.frame(Habitat = h$habitat, Type = h$type, `Area (km²)` = h$area_km2, `Area CV` = h$area_cv,
  `NPP as entered` = paste(h$npp_value, gsub("_", " ", h$npp_unit)), `NPP (t C km⁻² yr⁻¹)` = h$npp_t_C_km2_yr, `NPP CV` = h$npp_cv,
  `Food-web fraction` = h$food_web_fraction, TTE = h$tte, `No-take share` = h$closed_fraction, check.names = FALSE),
  caption = "Table. Habitat inputs (St. Croix; areas merged from API acquisition).")
auto$in_habitat_sources <- tab(data.frame(Habitat = h$habitat, Status = h$data_status, Source = h$reference, check.names = FALSE))
gf <- g[g$group_type == "fmp", ]
auto$in_groups <- tab(data.frame(Group = gf$group, Complex = gf$fmp_complex, Guild = gf$guild, Tier = gf$data_tier, Trap = ifelse(gf$trap_relevant, "yes", ""),
  Harvest = ifelse(gf$harvest_allowed, "yes", "no"), `P/B` = gf$pb, M = gf$m_nat, x = gf$x_msy, `B_MSY/K` = gf$bmsy_k,
  `Partition (t km⁻²)` = gf$partition_value, `Partition CV` = gf$partition_cv, `Init. depletion` = gf$init_depletion,
  Refuge = gf$refuge_dependence, `Open rec.` = gf$open_recruitment, Nursery = gf$nursery_dependence, check.names = FALSE),
  caption = "Table. FMP functional-group parameters (St. Croix).")
auto$in_group_sources <- tab(data.frame(Group = g$group, Status = g$data_status, Source = g$reference, check.names = FALSE))
gn <- g[g$group_type != "fmp", ]
auto$in_competitors <- tab(data.frame(Group = gn$group, Guild = gn$guild, `P/B` = gn$pb, `Partition (t km⁻²)` = gn$partition_value,
  `Partition CV` = gn$partition_cv, Status = gn$data_status, check.names = FALSE), caption = "Table. Non-fishery competitor groups.")
auto$in_diet <- tab(cbind(group = proj$diet$group, round(as.data.frame(proj$diet_matrix), 3)), caption = "Table. Diet composition by trophic-level pool (B = benthic, P = pelagic pathway).")
hp <- as.data.frame(proj$pref_matrix); auto$in_pref <- tab(cbind(group = rownames(proj$pref_matrix), hp), caption = "Table. Habitat preferences (expert judgement; CV 0.2).")
er <- proj$env_response
auto$in_env <- tab(data.frame(Group = er$group, Variable = er$variable, Type = er$type, Target = er$target, Sensitivity = er$sensitivity,
  `Sens. CV` = er$sensitivity_cv, `T_opt` = er$t_opt, `T width` = er$t_width, Status = er$data_status, check.names = FALSE),
  caption = "Table. Environmental responses.")
st <- rd("settings.csv", file.path(pd, "data")); auto$in_settings <- tab(st, caption = "Table. Model settings (St. Croix).")
auto$in_scen <- tab(rd("scenarios.csv", file.path(pd, "data")), caption = "Table. Scenario definitions.")
auto$in_dist <- tab(rd("disturbances.csv", file.path(pd, "data"))[, 1:12], caption = "Table. Disturbance templates (hurricanes).")
acqd <- rd("disturbances.csv", file.path(pd, "data/acquired"))
if (!is.null(acqd)) auto$in_hurricanes <- tab(acqd[, intersect(c("year", "event", "intensity", "intensity_cv", "notes"), names(acqd))], caption = "Table. Hurricanes affecting St. Croix (NHC HURDAT2, 1983–2024).")
auto$in_bench <- tab(rd("benchmarks.csv", file.path(pd, "data")), caption = "Table. Management benchmarks.")
auto$in_sources <- tab(rd("data_sources.csv", file.path(pd, "data")), caption = "Table. Automated data-acquisition configuration.")
ec <- proj$env_covariates
cs <- do.call(rbind, lapply(split(ec, ec$variable), function(z) data.frame(Variable = z$variable[1], Years = paste(range(z$year), collapse = "–"),
  Mean = mean(z$value), Min = min(z$value), Max = max(z$value), Units = if ("units" %in% names(z)) z$units[1] else "",
  Status = paste(unique(z$data_status), collapse = "; "))))
auto$in_covariates <- tab(cs, caption = "Table. Environmental covariates (summary).")
L <- proj$landings
la <- aggregate(landings_lb ~ group + gear, data = L[L$year >= 2022, ], FUN = function(x) round(mean(x)))
auto$in_landings <- tab(la[order(la$group), ], caption = "Table. Mean annual landings 2022–2024 by group and gear (lb; PLACEHOLDER except where stated).")
auto$in_indices <- tab(aggregate(cbind(year) ~ index + group + role + data_status, data = proj$indices, FUN = function(z) paste(range(z), collapse = "–")),
  caption = "Table. Abundance indices (all held out for validation).")
lh <- rd("xmsy_priors.csv", file.path(pd, "data-raw/life_history"))
if (!is.null(lh)) auto$in_lh <- tab(lh[, c("group", "n_species", "M", "lc", "x_msy", "x_msy_cv", "x_p05", "x_p95", "Fmsy_over_M", "Bmsy_over_B0")],
  caption = "Table. Per-recruit life-history priors.")
# ---- results ----
auto$res_sir <- tab(rd("sir_diagnostics_summary.csv"), caption = "Table. Importance-sampling diagnostics.")
rp <- rd("spf_group_reference_points.csv")
if (!is.null(rp)) {
  r1 <- rp[rp$group_type == "fmp", ]
  auto$res_rp <- tab(data.frame(Group = r1$group, Status = r1$status, `MMSY p05` = r1$MMSY_p05_lb, `MMSY p50` = r1$MMSY_p50_lb, `MMSY p95` = r1$MMSY_p95_lb,
    `P* target` = r1$target_lb, Catch = r1$catch_lb, `B/B_MSY` = r1$B_over_BMSY, `P(overfishing)` = r1$P_overfishing,
    `P(overfished)` = r1$P_overfished, check.names = FALSE), digits = list(`P(overfishing)` = 3, `P(overfished)` = 3, `B/B_MSY` = 2),
    caption = "Table. Group reference points, Baseline, 2022–2024 (lb yr⁻¹).")
}
gl <- rd("spf_guild_mmsy.csv"); if (!is.null(gl)) auto$res_guild <- tab(gl[gl$level == "guild", intersect(c("name", "MMSY_p05_lb", "MMSY_p50_lb", "MMSY_p95_lb", "catch_lb", "P_overfishing"), names(gl))], caption = "Table. Guild MMSY (lb yr⁻¹; draw-level sums).")
ce <- rd("spf_gear_effort_ceilings.csv"); if (!is.null(ce)) auto$res_gear <- tab(ce[, c("gear", "effort_ref", "effort_multiplier_p05", "effort_multiplier_p50", "effort_multiplier_p95", "effort_multiplier_target", "choke_group", "P_choke", "choke_alternatives")], digits = list(effort_multiplier_p05 = 2, effort_multiplier_p50 = 2, effort_multiplier_p95 = 2, effort_multiplier_target = 2, P_choke = 2), caption = "Table. Gear effort ceilings and choke groups.")
sc <- rd("spf_scenario_reference_points.csv")
if (!is.null(sc)) {
  s1 <- sc[sc$calibrated & sc$harvest_allowed, ]
  auto$res_scen <- tab(data.frame(Scenario = s1$scenario, Group = s1$group, `MMSY p50` = s1$MMSY_p50_lb, Catch = s1$catch_lb, `B/B_MSY` = s1$B_over_BMSY,
    `P(overfishing)` = s1$P_overfishing, Status = s1$status, check.names = FALSE), digits = list(`B/B_MSY` = 2, `P(overfishing)` = 3),
    caption = "Table. Scenario results for calibrated trap groups, 2043–2045.")
}
auto$res_bench <- tab(rd("benchmark_comparison.csv")[, c("group", "metric", "value_lb", "MMSY_p05_lb", "MMSY_p50_lb", "MMSY_p95_lb", "ratio_MMSYp50_to_benchmark")], digits = list(ratio_MMSYp50_to_benchmark = 2), caption = "Table. Comparison with management benchmarks.")
auto$res_selftest <- tab(rd("self_test_summary.csv"), digits = list(median_rel_error = 3, MARE = 3, coverage50 = 2, coverage90 = 2), caption = "Table. Simulation self-test (100 replicates).")
auto$res_loo <- tab(rd("validation_loo_benchmarks.csv"), caption = "Table. Leave-one-benchmark-out validation.")
auto$res_oos <- tab(rd("validation_out_of_sample.csv"), caption = "Table. Held-out index validation (placeholder series).")
gs <- rd("global_sensitivity.csv"); if (!is.null(gs)) auto$res_gsa <- tab(do.call(rbind, lapply(split(gs, gs$output), function(z) utils::head(z[order(-abs(z$SRRC)), ], 6))), caption = "Table. Global sensitivity: six most influential inputs per output.")
ep <- rd("structural_ensemble_pooled.csv"); auto$res_ens_pooled <- tab(ep, digits = list(between_variant_cv = 2), caption = "Table. Structural ensemble, pooled.")
ev <- rd("structural_ensemble_by_variant.csv")
if (!is.null(ev)) { ev <- ev[ev$group %in% c("spiny_lobster", "queen_triggerfish", "wrasses_hogfish", "porgies", "parrotfish_2"), ]
  auto$res_ens_var <- tab(data.frame(Variant = ev$variant, Group = ev$group, `MMSY p50 (lb)` = t_to_lb(ev$MMSY_p50_t), ESS = ev$ess, check.names = FALSE), digits = list(ESS = 1), caption = "Table. Structural ensemble by variant (selected groups).") }
auto$res_cmsy <- tab(rd("method_comparison_catch_only.csv"), digits = list(ratio_TTM_to_catch_only = 2, r_p50 = 2), caption = "Table. TTM MMSY vs catch-only MSY.")
vs <- rd("value_of_information_streams.csv"); if (!is.null(vs)) auto$res_voi <- tab(vs[vs$output == "total_focus_MMSY", ], digits = list(S1_max = 3, S1_sum = 3), caption = "Table. Value of information by data stream (total focus MMSY).")
auto$res_mse <- tab(rd("mse_performance.csv"), digits = list(P_any_focus_overfished = 3, annual_P_any_focus_overfished = 3, min_BBmsy_p05 = 2, min_BBmsy_p50 = 2, catch_AAV_p50 = 3, effort_mult_p50 = 2), caption = "Table. Closed-loop MSE performance (2036–2045).")
mg <- rd("mse_performance_by_group.csv"); if (!is.null(mg)) auto$res_mse_group <- tab(mg, digits = list(P_overfished = 3, min_BBmsy_p50 = 2), caption = "Table. MSE: probability each focus group falls below 0.5 B_MSY.")
auto$res_proc <- tab(rd("process_diagnostics.csv"), digits = list(fishable_fraction = 3, nursery_index_hist_end = 3, nursery_index_proj_end = 3, ghost_share_of_catch = 3), caption = "Table. Ecological process diagnostics (Baseline medians).")
eo <- rd("ecosystem_overfishing_indices.csv"); if (!is.null(eo)) auto$res_eco <- tab(eo[eo$year %in% c(1985, 1995, 2005, 2015, 2022:2024, 2045), ], caption = "Table. Ecosystem-overfishing indices (selected years).")
# ---- figures ----
figs <- c(fig01 = "fig01_energy_budget.png", fig02 = "fig02_group_mmsy.png", fig03 = "fig03_scenario_status.png", fig04 = "fig04_bbmsy_trajectories.png",
          fig05 = "fig05_mmsy_trajectories.png", fig06 = "fig06_coral_quality.png", fig07 = "fig07_effort_ceilings.png", fig08 = "fig08_sir.png",
          fig09 = "fig09_environment.png", fig10 = "fig10_hurricanes.png", fig11 = "fig11_sensitivity_total_focus_MMSY.png", fig12 = "fig12_validation.png",
          fig13 = "fig13_loo_benchmarks.png", fig14 = "fig14_eco_indices.png", fig15 = "fig15_structural_ensemble.png", fig16 = "fig16_method_comparison.png",
          fig17 = "fig17_value_of_information.png", fig18 = "fig18_mse.png")
caps <- c(fig01 = "Primary production entering the food web by habitat (median and 90% interval).",
  fig02 = "Group MMSY intervals with current catch and management benchmarks.", fig03 = "Scenario status matrix, 2043–2045.",
  fig04 = "B/B_MSY trajectories by scenario.", fig05 = "MMSY trajectories by scenario.", fig06 = "Coral habitat quality.",
  fig07 = "Gear effort ceilings and choke groups.", fig08 = "MMSY prior and SIR-conditioned posterior for calibrated stocks.",
  fig09 = "Environmental drivers acquired from public APIs, 1983–2024.", fig10 = "Hurricane intensity at St. Croix (HURDAT2).",
  fig11 = "Global sensitivity of total focus-group MMSY.", fig12 = "Held-out index validation.", fig13 = "Leave-one-benchmark-out predictions.",
  fig14 = "Ecosystem-overfishing indices.", fig15 = "Structural ensemble.", fig16 = "TTM MMSY vs catch-only MSY and OFLs.",
  fig17 = "Value of information by data stream.", fig18 = "Closed-loop management strategy evaluation.")
for (k in names(figs)) auto[[k]] <- fig(figs[[k]], paste0("Figure (", k, "). ", caps[[k]]))
# ---- run facts ----
lg <- file.path(pd, "output/run_log.txt")
auto$run_time <- if (file.exists(lg)) { z <- grep("Run time", readLines(lg), value = TRUE); if (length(z)) sub(".*Run time: ", "", z[1]) else "" } else ""
# ---- function inventory ----
fns <- unlist(lapply(list.files("R", full.names = TRUE), function(f) { x <- readLines(f); n <- grep("^[a-zA-Z_][a-zA-Z0-9_.]* <- function", x, value = TRUE)
  n <- sub(" <- function.*", "", n); n <- n[!startsWith(n, ".")]; if (length(n)) paste0("`", basename(f), "`: ", paste(n, collapse = ", ")) else NULL }))
auto$code_inventory <- paste0("* ", fns)

# ---- assemble ----
secs <- sort(list.files(file.path(md, "sections"), pattern = "\\.md$", full.names = TRUE))
txt <- unlist(lapply(secs, function(f) c(readLines(f), "", "")))
include <- function(f, demote = 1) {
  x <- c("", readLines(file.path("docs", f))[-1], "")   # drop the page title; blank lines around
  x <- sub("^(#+)", paste0("\\1", strrep("#", demote)), x)
  x
}
outl <- character(0)
for (line in txt) {
  if (grepl("^\\{\\{AUTO:[a-z0-9_]+\\}\\}$", line)) { k <- sub("^\\{\\{AUTO:(.*)\\}\\}$", "\\1", line); outl <- c(outl, if (is.null(auto[[k]])) paste0("*(", k, " not available)*") else auto[[k]]) }
  else if (grepl("^\\{\\{INCLUDE:", line)) { spec <- strsplit(sub("^\\{\\{INCLUDE:(.*)\\}\\}$", "\\1", line), ",")[[1]]; outl <- c(outl, include(spec[1], if (length(spec) > 1) as.integer(spec[2]) else 1)) }
  else outl <- c(outl, gsub("\\{\\{RUN_TIME\\}\\}", auto$run_time, line))
}
# pandoc needs a blank line before a list that follows a paragraph line
is_item <- function(x) grepl("^\\s*([*+-]|[0-9]+\\.)\\s", x)
fixed <- character(0)
for (i in seq_along(outl)) {
  if (i > 1 && is_item(outl[i]) && !grepl("^\\s*$", outl[i - 1]) && !is_item(outl[i - 1]) &&
      !grepl("^\\s+\\S", outl[i - 1]) && !grepl("^\\|", outl[i - 1])) fixed <- c(fixed, "")
  fixed <- c(fixed, outl[i])
}
outl <- fixed
# links to other docs pages are meaningless inside the memo: keep the text only
outl <- gsub("\\[([^]]+)\\]\\((?!http)[^)]*\\.md[^)]*\\)", "\\1", outl, perl = TRUE)
writeLines(outl, out_md)
words <- length(strsplit(paste(outl, collapse = " "), "\\s+")[[1]])
message("Wrote ", out_md, " (", words, " words)")
# ---- render ----
if (nzchar(Sys.which("pandoc"))) {
  docx <- sub("\\.md$", ".docx", out_md)
  system2("pandoc", c(shQuote(out_md), "-o", shQuote(docx), "--toc", "--toc-depth=2", "--number-sections",
                      paste0("--resource-path=", md), "-f", "markdown+tex_math_dollars+pipe_tables+implicit_figures"))
  message("Wrote ", docx)
  # PDF: standalone HTML with MathML, printed by Chromium (LibreOffice drops Word equations)
  html <- sub("\\.md$", ".html", out_md)
  css <- file.path(md, "memo.css")
  system2("pandoc", c(shQuote(out_md), "-o", shQuote(html), "--standalone", "--toc", "--toc-depth=2", "--number-sections",
                      "--mathml", "--embed-resources", paste0("--css=", css), paste0("--resource-path=", md),
                      "-f", "markdown+tex_math_dollars+pipe_tables+implicit_figures"))
  pdf <- sub("\\.md$", ".pdf", out_md)
  js <- tempfile(fileext = ".js")
  writeLines(sprintf("const { chromium } = require('playwright');
(async () => { const b = await chromium.launch(); const p = await b.newPage();
  await p.goto('file://%s', { waitUntil: 'load', timeout: 600000 });
  await p.pdf({ path: '%s', format: 'Letter', printBackground: true, margin: { top: '0.8in', bottom: '0.8in', left: '0.75in', right: '0.75in' },
    displayHeaderFooter: true, headerTemplate: '<div></div>',
    footerTemplate: '<div style=\"font-size:8px;width:100%%;text-align:center;color:#666\">CaribTTM Technical Memorandum (draft) - page <span class=\"pageNumber\"></span> of <span class=\"totalPages\"></span></div>' });
  await b.close(); })();", normalizePath(html), file.path(normalizePath(md), basename(pdf))), js)
  r <- system2("node", js, stdout = TRUE, stderr = TRUE)
  if (file.exists(pdf)) message("Wrote ", pdf) else message("PDF failed: ", paste(r, collapse = "\n"))
}
