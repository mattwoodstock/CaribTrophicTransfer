# ==============================================================================
# Plotting helpers (require ggplot2)
# ==============================================================================
# Colour roles follow a fixed, colour-vision-deficiency validated categorical
# order (series assigned in fixed order, never cycled) and a reserved status
# palette that is always paired with a text label.
# ==============================================================================

#' Categorical palette (fixed order)
#' @export
ttm_palette <- function() c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                            "#e87ba4", "#008300", "#6250d6", "#e34948")

#' Status palette (reserved for status; always shown with labels)
#' @export
ttm_status_palette <- function() c("Below target" = "#0ca30c", "Caution" = "#fab219",
                                   "Overfishing" = "#ec835a", "Overfished" = "#d03b3b",
                                   "Prohibited" = "#8a8984")

.need_gg <- function() if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2 for plots")

.theme_ttm <- function(base = 11) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#0b0b0b"),
      plot.subtitle = ggplot2::element_text(colour = "#52514e"),
      axis.text = ggplot2::element_text(colour = "#52514e"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e6e5e0", linewidth = 0.3),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "top",
      plot.background = ggplot2::element_rect(fill = "#fcfcfb", colour = NA))
}

.scen_cols <- function(sc) setNames(ttm_palette()[seq_along(sc)], sc)

#' Primary production budget by habitat (median and 90% interval)
#' @param res `ttm_results`
#' @param scenario scenario
#' @export
plot_energy_budget <- function(res, scenario = "Baseline") {
  .need_gg()
  yy <- .ref_years(res)
  P1 <- apply(get_metric(res, "P1", scenario)[, yy, , drop = FALSE], c(1, 3), mean)
  h <- res$proj$habitats
  df <- data.frame(habitat = colnames(P1), type = h$type,
                   p50 = apply(P1, 2, stats::median), lo = apply(P1, 2, stats::quantile, 0.05),
                   hi = apply(P1, 2, stats::quantile, 0.95))
  df$habitat <- factor(df$habitat, levels = df$habitat[order(df$p50)])
  ggplot2::ggplot(df, ggplot2::aes(y = habitat, x = p50)) +
    ggplot2::geom_col(fill = ttm_palette()[1], width = 0.6) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi), height = 0.2, colour = "#52514e") +
    ggplot2::scale_x_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    ggplot2::labs(title = "Primary production entering the food web",
                  subtitle = sprintf("t wet weight per year (median, 90%% interval), %s-%s", yy[1], utils::tail(yy, 1)),
                  x = "t WW / yr (log scale)", y = NULL) + .theme_ttm()
}

#' Group MMSY intervals with current catch and management benchmarks
#' @param res `ttm_results`
#' @param scenario scenario
#' @param fmp_only plot only FMP groups
#' @param units "lb" or "t"
#' @export
plot_reference_points <- function(res, scenario = "Baseline", fmp_only = TRUE, units = "lb") {
  .need_gg()
  rp <- reference_points(res, scenario)
  if (fmp_only) rp <- rp[rp$group_type == "fmp", ]
  sfx <- if (units == "lb") "_lb" else "_t"
  df <- data.frame(group = rp$group, lo = rp[[paste0("MMSY_p05", sfx)]], q25 = rp[[paste0("MMSY_p25", sfx)]],
                   med = rp[[paste0("MMSY_p50", sfx)]], q75 = rp[[paste0("MMSY_p75", sfx)]],
                   hi = rp[[paste0("MMSY_p95", sfx)]], catch = rp[[paste0("catch", sfx)]],
                   trap = ifelse(rp$trap_relevant, "Trap-caught", "Other gears"))
  df$group <- factor(df$group, levels = df$group[order(df$med)])
  p <- ggplot2::ggplot(df, ggplot2::aes(y = group)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lo, xmax = hi), colour = "#86b6ef", linewidth = 1.2) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = q25, xmax = q75), colour = ttm_palette()[1], linewidth = 3) +
    ggplot2::geom_point(ggplot2::aes(x = med, shape = "MMSY median"), colour = "#0b0b0b", size = 2.2) +
    ggplot2::geom_point(data = df[df$catch > 0.01 * df$lo, ],
                        ggplot2::aes(x = catch, shape = "Current catch (expanded)"),
                        colour = ttm_palette()[2], size = 2.6)
  bm <- res$proj$benchmarks
  if (!is.null(bm) && units == "lb") {
    bm <- bm[bm$group %in% df$group, ]
    if (nrow(bm)) p <- p + ggplot2::geom_point(data = bm, ggplot2::aes(x = value_lb, y = group, shape = paste("Benchmark", metric)),
                                              colour = "#52514e", size = 2.4)
  }
  p + ggplot2::scale_x_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    ggplot2::scale_shape_manual(values = c("MMSY median" = 16, "Current catch (expanded)" = 17,
                                           "Benchmark OFL" = 5, "Benchmark ABC" = 0, "Benchmark ACL" = 4), name = NULL) +
    ggplot2::labs(title = "Group-level MMSY (prevailing conditions)",
                  subtitle = "Bars: 50% (dark) and 90% (light) intervals; log scale",
                  x = paste0("MMSY (", units, " / yr)"), y = NULL) + .theme_ttm()
}

#' Status stoplight across scenarios (catch / MMSY at end of projection)
#' @param res `ttm_results`
#' @param years years to average (default last 3 projection years)
#' @export
plot_scenario_status <- function(res, years = NULL) {
  .need_gg()
  st <- res$proj$settings
  if (is.null(years)) years <- seq(st$proj_end - 2, st$proj_end)
  rp <- do.call(rbind, lapply(res$scenarios, function(s) reference_points(res, s, years)))
  rp <- rp[rp$group_type == "fmp", ]
  rp$scenario <- factor(rp$scenario, levels = res$scenarios)
  ord <- rp[rp$scenario == "Baseline", ]; ord <- ord$group[order(ord$U_catch_over_MMSY)]
  rp$group <- factor(rp$group, levels = ord)
  rp$label <- ifelse(rp$harvest_allowed, sprintf("%.2f", rp$U_catch_over_MMSY), "-")
  ggplot2::ggplot(rp, ggplot2::aes(x = scenario, y = group, fill = status)) +
    ggplot2::geom_tile(colour = "#fcfcfb", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3, colour = "#0b0b0b") +
    ggplot2::scale_fill_manual(values = ttm_status_palette(), drop = FALSE, name = "Status") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(title = "Catch relative to MMSY across scenarios",
                  subtitle = sprintf("Median catch / MMSY, %d-%d. Overfished = B/BMSY < 0.5; Overfishing = catch > median MMSY (OFL proxy); Caution = catch > P* target",
                                     min(years), max(years)), x = NULL, y = NULL) +
    .theme_ttm() + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 20, hjust = 0),
                                  panel.grid = ggplot2::element_blank(),
                                  plot.margin = ggplot2::margin(10, 60, 10, 10))
}

#' Projected trajectories for selected groups and scenarios
#' @param res `ttm_results`
#' @param metric "BBmsy", "MMSY", "B", "C", "U", "EU"
#' @param groups groups to show (default trap-relevant harvested groups)
#' @param scenarios scenarios
#' @export
plot_trajectories <- function(res, metric = "BBmsy", groups = NULL, scenarios = res$scenarios) {
  .need_gg()
  g <- res$proj$groups
  if (is.null(groups)) groups <- g$group[g$trap_relevant & g$harvest_allowed][1:6]
  df <- summarize_timeseries(res, metric, scenarios, groups)
  df$scenario <- factor(df$scenario, levels = scenarios)
  ylab <- switch(metric, BBmsy = "B / BMSY", U = "Catch / MMSY", EU = "Energy utilization (catch / production)",
                 paste(metric, "(t)"))
  base <- df[df$scenario == scenarios[1], ]
  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = p50, colour = scenario)) +
    ggplot2::geom_ribbon(data = base, ggplot2::aes(ymin = p25, ymax = p75), fill = ttm_palette()[1],
                         alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_line(data = base, linewidth = 1.0) +
    ggplot2::geom_vline(xintercept = res$proj$settings$hist_end + 0.5, linetype = "dashed", colour = "#8a8984") +
    ggplot2::facet_wrap(~group, scales = "free_y") +
    ggplot2::scale_colour_manual(values = .scen_cols(scenarios), name = NULL) +
    ggplot2::labs(title = paste("Trajectories:", ylab),
                  subtitle = sprintf("Median by scenario; shaded = 50%% interval for %s; dashed line = start of projection", scenarios[1]),
                  x = NULL, y = ylab) + .theme_ttm()
  if (metric %in% c("BBmsy", "U")) p <- p + ggplot2::geom_hline(yintercept = 1, colour = "#52514e", linewidth = 0.3)
  p
}

#' Gear effort multipliers that keep every group at or below its gear-specific MMSY
#' @param ga output of [gear_allocation()]
#' @export
plot_effort_ceilings <- function(ga) {
  .need_gg()
  df <- ga$effort_ceilings
  df$label <- sprintf("choke: %s (%.0f%%)", df$choke_group, 100 * df$P_choke)
  ggplot2::ggplot(df, ggplot2::aes(y = gear)) +
    ggplot2::geom_vline(xintercept = 1, colour = "#52514e", linewidth = 0.3) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = effort_multiplier_p05, xmax = effort_multiplier_p95),
                            colour = "#86b6ef", linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(x = effort_multiplier_p50), colour = ttm_palette()[1], size = 2.6) +
    ggplot2::geom_point(ggplot2::aes(x = effort_multiplier_target), colour = ttm_palette()[2], shape = 18, size = 3) +
    ggplot2::geom_text(ggplot2::aes(x = effort_multiplier_p95, label = label), hjust = -0.05, size = 3, colour = "#52514e") +
    ggplot2::scale_x_log10(expand = ggplot2::expansion(mult = c(0.05, 0.6))) +
    ggplot2::labs(title = "Gear-specific effort ceilings and choke groups",
                  subtitle = "Multiplier of current effort that brings the most-limiting group to its gear MMSY\n(circle = median, diamond = P* target, bar = 90% interval; 1 = current effort)",
                  x = "Effort multiplier (log scale)", y = NULL) + .theme_ttm()
}

#' Habitat drivers through time: habitat quality index and benthic production
#' @param res `ttm_results`
#' @param habitat habitat to show quality for
#' @param scenarios scenarios
#' @export
plot_habitat_drivers <- function(res, habitat = "coral_reef", scenarios = res$scenarios) {
  .need_gg()
  out <- do.call(rbind, lapply(scenarios, function(s) {
    Q <- get_metric(res, "Q", s)[, , habitat]
    data.frame(scenario = s, year = as.integer(colnames(Q)),
               p50 = apply(Q, 2, stats::median), p25 = apply(Q, 2, stats::quantile, 0.25),
               p75 = apply(Q, 2, stats::quantile, 0.75))
  }))
  out$scenario <- factor(out$scenario, levels = scenarios)
  ggplot2::ggplot(out, ggplot2::aes(year, p50, colour = scenario, fill = scenario)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p25, ymax = p75), alpha = 0.12, colour = NA) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = res$proj$settings$hist_end + 0.5, linetype = "dashed", colour = "#8a8984") +
    ggplot2::scale_colour_manual(values = .scen_cols(scenarios), name = NULL) +
    ggplot2::scale_fill_manual(values = .scen_cols(scenarios), name = NULL) +
    ggplot2::labs(title = paste("Habitat quality index:", habitat),
                  subtitle = "Relative to reference (1 = reference condition); drives refuge-dependent capacity",
                  x = NULL, y = "Quality index") + .theme_ttm()
}

#' Prior vs SIR-conditioned MMSY for selected groups
#' @param res `ttm_results` with SIR applied
#' @param groups groups to show (default: groups used in SIR)
#' @export
plot_sir <- function(res, groups = NULL) {
  .need_gg()
  if (is.null(res$sir)) stop("No SIR in results")
  if (is.null(groups)) {
    groups <- unique(c(unlist(strsplit(res$proj$indices$group, ";")),
                       res$proj$benchmarks$group[as.logical(res$proj$benchmarks$use_in_sir) %in% TRUE]))
  }
  MM <- res$stage1$MMSY_ref[, groups, drop = FALSE]
  df <- rbind(
    data.frame(dist = "Prior", group = rep(groups, each = nrow(MM)), MMSY_lb = t_to_lb(as.vector(MM))),
    data.frame(dist = "SIR posterior", group = rep(groups, each = length(res$draws)),
               MMSY_lb = t_to_lb(as.vector(MM[res$draws, , drop = FALSE]))))
  p <- ggplot2::ggplot(df, ggplot2::aes(MMSY_lb, colour = dist, fill = dist)) +
    ggplot2::geom_density(alpha = 0.15, linewidth = 0.7) +
    ggplot2::facet_wrap(~group, scales = "free") +
    ggplot2::scale_x_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    ggplot2::scale_colour_manual(values = ttm_palette()[1:2], name = NULL) +
    ggplot2::scale_fill_manual(values = ttm_palette()[1:2], name = NULL) +
    ggplot2::labs(title = "MMSY: prior vs SIR-conditioned",
                  subtitle = sprintf("ESS = %.0f of %d draws", res$sir$ess, res$n_prior),
                  x = "MMSY (lb / yr, log scale)", y = "Density") + .theme_ttm()
  bm <- res$proj$benchmarks
  if (!is.null(bm)) {
    bm <- bm[bm$group %in% groups & bm$metric == "OFL", ]
    if (nrow(bm)) p <- p + ggplot2::geom_vline(data = bm, ggplot2::aes(xintercept = value_lb), linetype = "dashed", colour = "#52514e")
  }
  p
}

#' Save the standard figure set
#' @param res `ttm_results`
#' @param dir output directory
#' @export
save_standard_plots <- function(res, dir) {
  .need_gg()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  sv <- function(p, f, w = 9, h = 6) ggplot2::ggsave(file.path(dir, f), p, width = w, height = h, dpi = 200, bg = "#fcfcfb")
  sv(plot_energy_budget(res), "fig01_energy_budget.png", 8, 4.5)
  sv(plot_reference_points(res), "fig02_group_mmsy.png", 9, 7)
  sv(plot_scenario_status(res), "fig03_scenario_status.png", 11, 7)
  sv(plot_trajectories(res, "BBmsy"), "fig04_bbmsy_trajectories.png", 12, 7)
  sv(plot_trajectories(res, "MMSY"), "fig05_mmsy_trajectories.png", 12, 7)
  if ("coral_reef" %in% res$proj$habitats$habitat) sv(plot_habitat_drivers(res), "fig06_coral_quality.png", 9, 5)
  if (length(res$proj$gear_names)) sv(plot_effort_ceilings(gear_allocation(res)), "fig07_effort_ceilings.png", 10, 4.5)
  if (!is.null(res$sir)) sv(plot_sir(res), "fig08_sir.png", 10, 5)
  if (!is.null(res$proj$env_covariates)) sv(plot_environment(res$proj), "fig09_environment.png", 10, 6)
  if (!is.null(res$proj$disturbances)) sv(plot_disturbances(res$proj), "fig10_hurricanes.png", 10, 4)
  invisible(list.files(dir, pattern = "png$", full.names = TRUE))
}

#' Environmental drivers panel (covariates and disturbance events)
#'
#' Small multiples of the historical covariates (with observation SD bands)
#' and a lollipop timeline of disturbance intensities.
#' @param proj a `ttm_project`
#' @param variables covariates to show
#' @param labels facet labels
#' @export
plot_environment <- function(proj, variables = c("sst", "dhw", "npp_pelagic_index", "coral_cover"),
                             labels = c("SST (°C, annual mean)", "Degree heating weeks (annual max)",
                                        "Pelagic NPP index (VGPM ensemble)", "Coral cover (proportion)")) {
  .need_gg()
  ec <- proj$env_covariates
  ec <- ec[ec$variable %in% variables & ec$year <= proj$settings$hist_end & ec$year >= proj$settings$hist_start, ]
  if (!"sd" %in% names(ec)) ec$sd <- 0
  ec$sd[is.na(ec$sd)] <- 0
  ec$panel <- factor(labels[match(ec$variable, variables)], levels = labels)
  p1 <- ggplot2::ggplot(ec, ggplot2::aes(year, value)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = value - sd, ymax = value + sd), fill = "#86b6ef", alpha = 0.5) +
    ggplot2::geom_line(colour = ttm_palette()[1], linewidth = 0.8) +
    ggplot2::geom_point(colour = ttm_palette()[1], size = 1.2) +
    ggplot2::facet_wrap(~panel, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = NULL, y = NULL, title = "Environmental drivers acquired from public APIs",
                  subtitle = "Lines: annual values; bands: observation uncertainty (±1 SD) propagated in the Monte Carlo") +
    .theme_ttm()
  p1
}

#' Disturbance (hurricane) intensity timeline
#' @param proj a `ttm_project`
#' @export
plot_disturbances <- function(proj) {
  .need_gg()
  d <- proj$disturbances
  d <- d[!is.na(d$year) & !grepl("other gears", d$event), ]
  d$label <- sub("^Hurricane ", "", sub(" \\(AL[0-9]+\\)", "", d$event))
  d <- d[order(d$year, -d$intensity), ]
  lab <- d[d$intensity >= 0.3, ]
  # stagger labels of nearby events left/right to avoid overlap
  lab$hjust <- 0.5
  for (k in seq_len(nrow(lab))[-1]) if (lab$year[k] - lab$year[k - 1] < 5) {
    if (lab$hjust[k - 1] == 0.5) lab$hjust[k - 1] <- 1.05
    lab$hjust[k] <- -0.05
  }
  ggplot2::ggplot(d, ggplot2::aes(year, intensity)) +
    ggplot2::geom_segment(ggplot2::aes(xend = year, y = 0, yend = intensity), colour = "#86b6ef", linewidth = 1.2) +
    ggplot2::geom_point(colour = ttm_palette()[2], size = 3) +
    ggplot2::geom_text(data = lab, ggplot2::aes(label = paste(label, year), hjust = hjust),
                       vjust = -0.9, size = 3.2, colour = "#0b0b0b") +
    ggplot2::scale_y_continuous(limits = c(0, 1.15), breaks = c(0, 0.5, 1)) +
    ggplot2::labs(title = "Hurricane disturbance intensity (NHC HURDAT2)", x = NULL,
                  y = "Intensity (0-1)", subtitle = "Wind at closest approach x distance decay; losses scale with intensity") +
    .theme_ttm()
}
