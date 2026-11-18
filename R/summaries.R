# ==============================================================================
# Reference points, guild aggregation, gear allocation and SPF hand-off tables
# ==============================================================================

.probs <- c(p05 = 0.05, p25 = 0.25, p50 = 0.50, p75 = 0.75, p95 = 0.95)

.q <- function(x, probs = .probs) {
  x <- x[is.finite(x)]
  if (!length(x)) return(setNames(rep(NA_real_, length(probs) + 1), c("mean", names(probs))))
  c(mean = mean(x), setNames(stats::quantile(x, probs, names = FALSE), names(probs)))
}

#' Extract a metric array for the draws in use (prior or SIR-resampled)
#' @param res `ttm_results`
#' @param metric one of B, K, K0, P, P0, MMSY, MMSY0, C, F, Cg, P1, Q, Eff
#' @param scenario scenario name
#' @export
get_metric <- function(res, metric, scenario = "Baseline") {
  a <- res$results[[scenario]][[metric]]
  idx <- c(list(res$draws), rep(list(TRUE), length(dim(a)) - 1))
  do.call(`[`, c(list(a), idx, list(drop = FALSE)))
}

.ref_years <- function(res, years = NULL) {
  st <- res$proj$settings
  if (is.null(years)) years <- seq(st$hist_end - st$ref_years + 1, st$hist_end)
  as.character(years)
}

#' Time series quantiles of any group-level metric
#' @param res `ttm_results`
#' @param metric metric name (see [get_metric()]); also "BBmsy" (2B/K),
#'   "U" (C/MMSY) and "EU" (C/P).
#' @param scenarios scenarios to include
#' @param groups optional subset of groups
#' @export
summarize_timeseries <- function(res, metric = "MMSY", scenarios = res$scenarios, groups = NULL) {
  out <- list()
  for (s in scenarios) {
    a <- switch(metric,
      BBmsy = 2 * get_metric(res, "B", s) / get_metric(res, "K", s),
      U = get_metric(res, "C", s) / get_metric(res, "MMSY", s),
      EU = get_metric(res, "C", s) / get_metric(res, "P", s),
      get_metric(res, metric, s))
    yrs <- dimnames(a)[[2]]; gr <- dimnames(a)[[3]]
    if (!is.null(groups)) gr <- intersect(gr, groups)
    for (gi in gr) for (y in yrs) {
      qq <- .q(a[, y, gi])
      out[[length(out) + 1]] <- data.frame(scenario = s, year = as.integer(y), group = gi,
                                           metric = metric, t(qq), check.names = FALSE)
    }
  }
  do.call(rbind, out)
}

#' Group-level reference points (the core TTM deliverable)
#'
#' Computed over the reference years (default: last `ref_years` of the
#' historical period) and averaged within each draw before taking quantiles.
#'
#' * `MMSY_*`: prevailing-conditions MMSY quantiles (t WW/yr), includes
#'   trophic feedback of current fishing on prey pools.
#' * `MMSY0_p50`: MMSY with no fishing feedback (ecosystem potential).
#' * `OFL_proxy_t`: median MMSY (50% probability of overfishing, SEDAR103-DW-20).
#' * `target_t`: MMSY at percentile `settings$pstar` (e.g., 40%).
#' * `P_overfishing`: share of draws where catch > MMSY.
#' * `P_overfished`: share of draws with B/BMSY < `settings$bbmsy_overfished`.
#' * `landings_shortfall`: median (over draws) of the largest annual share of
#'   historical landings the simulated stock could not support (0 = feasible).
#' * `EU`: energy utilization ratio = catch / production.
#' @param res `ttm_results`
#' @param scenario scenario
#' @param years reference years (default last `ref_years` of history)
#' @export
reference_points <- function(res, scenario = "Baseline", years = NULL) {
  proj <- res$proj; g <- proj$groups; st <- proj$settings
  yy <- .ref_years(res, years)
  m <- function(metric) apply(get_metric(res, metric, scenario)[, yy, , drop = FALSE], c(1, 3), mean)
  MMSY <- m("MMSY"); MMSY0 <- m("MMSY0"); K <- m("K"); K0 <- m("K0"); B <- m("B"); C <- m("C"); P <- m("P")
  TL <- res$TL[res$draws, , drop = FALSE]
  rows <- lapply(g$group, function(gi) {
    mm <- .q(MMSY[, gi])
    data.frame(
      scenario = scenario, group = gi, fmp_complex = g[gi, "fmp_complex"],
      guild = if ("guild" %in% names(g)) g[gi, "guild"] else NA,
      group_type = g[gi, "group_type"], trap_relevant = g[gi, "trap_relevant"],
      harvest_allowed = g[gi, "harvest_allowed"], data_tier = g[gi, "data_tier"],
      TL = stats::median(TL[, gi]),
      B0_t = stats::median(K0[, gi]), K_t = stats::median(K[, gi]),
      B_t = stats::median(B[, gi]), B_over_BMSY = stats::median(2 * B[, gi] / K[, gi]),
      MMSY_mean_t = mm[["mean"]], MMSY_p05_t = mm[["p05"]], MMSY_p25_t = mm[["p25"]],
      MMSY_p50_t = mm[["p50"]], MMSY_p75_t = mm[["p75"]], MMSY_p95_t = mm[["p95"]],
      MMSY_cv = stats::sd(MMSY[, gi]) / mean(MMSY[, gi]),
      MMSY0_p50_t = stats::median(MMSY0[, gi]),
      OFL_proxy_t = mm[["p50"]],
      target_t = unname(stats::quantile(MMSY[, gi], st$pstar)),
      production_p50_t = stats::median(P[, gi]),
      catch_t = stats::median(C[, gi]),
      EU = stats::median(C[, gi] / P[, gi]),
      U_catch_over_MMSY = stats::median(C[, gi] / MMSY[, gi]),
      P_overfishing = mean(C[, gi] > MMSY[, gi]),
      P_overfished = mean(2 * B[, gi] / K[, gi] < st$bbmsy_overfished),
      landings_shortfall = if (is.null(res$catch_shortfall)) NA else
        stats::median(res$catch_shortfall[res$draws, gi]),
      stringsAsFactors = FALSE)
  })
  rp <- do.call(rbind, rows)
  rp$status <- with(rp, ifelse(!harvest_allowed, "Prohibited",
                       ifelse(B_over_BMSY < st$bbmsy_overfished, "Overfished",
                       ifelse(catch_t > OFL_proxy_t, "Overfishing",
                       ifelse(catch_t > target_t, "Caution", "Below target")))))
  lb <- grep("_t$", names(rp), value = TRUE)
  for (nm in lb) rp[[sub("_t$", "_lb", nm)]] <- t_to_lb(rp[[nm]])
  rp
}

#' Aggregate MMSY to guilds (or any grouping column of groups.csv)
#'
#' Sums draw-level MMSY across member groups before taking quantiles, so
#' guild uncertainty correctly reflects covariance among groups.
#' @param res `ttm_results`
#' @param by grouping column in groups.csv (e.g., "guild", "fmp_complex", "group_type")
#' @param scenario scenario
#' @param years reference years
#' @param fmp_only only aggregate FMP groups
#' @export
aggregate_mmsy <- function(res, by = "guild", scenario = "Baseline", years = NULL, fmp_only = TRUE) {
  g <- res$proj$groups
  if (!by %in% names(g)) stop("Column not in groups.csv: ", by)
  yy <- .ref_years(res, years)
  MMSY <- apply(get_metric(res, "MMSY", scenario)[, yy, , drop = FALSE], c(1, 3), mean)
  C <- apply(get_metric(res, "C", scenario)[, yy, , drop = FALSE], c(1, 3), mean)
  keep <- if (fmp_only) g$group_type == "fmp" else rep(TRUE, nrow(g))
  lv <- unique(g[[by]][keep])
  do.call(rbind, lapply(lv, function(l) {
    gi <- g$group[keep & g[[by]] == l]
    tot <- rowSums(MMSY[, gi, drop = FALSE]); ct <- rowSums(C[, gi, drop = FALSE])
    qq <- .q(tot)
    data.frame(scenario = scenario, level = by, name = l, n_groups = length(gi),
               MMSY_p05_t = qq[["p05"]], MMSY_p50_t = qq[["p50"]], MMSY_p95_t = qq[["p95"]],
               target_t = unname(stats::quantile(tot, res$proj$settings$pstar)),
               catch_t = stats::median(ct), P_overfishing = mean(ct > tot),
               MMSY_p50_lb = t_to_lb(qq[["p50"]]), catch_lb = t_to_lb(stats::median(ct)))
  }))
}

#' Gear-specific MMSY, targeting matrix, effort ceilings and choke groups
#'
#' Two allocation rules:
#' * `catch_share` (default): MMSY_g,i = MMSY_i * C_g,i / sum_g C_g,i
#'   (each group's MMSY is split among gears by their share of its catch).
#' * `slide`: MMSY_g,i = MMSY_i * W_g,i * E_g / sum(E), W_g,i = C_g,i / sum_i C_g,i
#'   (SEDAR 103 presentation, slide 11).
#'
#' For each gear the effort multiplier that would bring catch of every group to
#' its gear-specific MMSY is m_g = min_i (MMSY_g,i / C_g,i) (catch assumed
#' proportional to effort). The minimizing group is the choke group. Effort
#' ceiling = m_g * reference effort. Computed draw by draw.
#' @param res `ttm_results`
#' @param scenario scenario
#' @param years reference years
#' @param method allocation rule
#' @param stat which MMSY quantity to allocate: "MMSY" (prevailing) or "MMSY0"
#' @export
gear_allocation <- function(res, scenario = "Baseline", years = NULL,
                            method = res$proj$settings$gear_alloc_method, stat = "MMSY") {
  proj <- res$proj; g <- proj$groups; gears <- proj$gear_names
  if (!length(gears)) stop("No gears defined (gears.csv / landings.csv)")
  yy <- .ref_years(res, years)
  MMSY <- apply(get_metric(res, stat, scenario)[, yy, , drop = FALSE], c(1, 3), mean)
  Cg <- apply(get_metric(res, "Cg", scenario)[, yy, , , drop = FALSE], c(1, 3, 4), mean)
  Eff <- apply(get_metric(res, "Eff", scenario)[, yy, , drop = FALSE], c(1, 3), mean)
  nd <- dim(Cg)[1]; ng <- nrow(g); nge <- length(gears)
  alloc <- array(0, c(nd, nge, ng), dimnames = list(NULL, gears, g$group))
  for (d in seq_len(nd)) {
    cg <- Cg[d, , , drop = FALSE][1, , ]
    if (is.null(dim(cg))) cg <- matrix(cg, nge, ng, dimnames = list(gears, g$group))
    a <- if (method == "slide") {
      W <- cg / pmax(rowSums(cg), 1e-12)
      sweep(W, 1, Eff[d, ] / sum(Eff[d, ]), "*")
    } else sweep(cg, 2, pmax(colSums(cg), 1e-12), "/")
    alloc[d, , ] <- sweep(a, 2, MMSY[d, ], "*")
  }
  targeting <- do.call(rbind, lapply(gears, function(ge) {
    cg <- apply(Cg[, ge, , drop = FALSE], 3, stats::median)
    tot_g <- apply(Cg[, , , drop = FALSE], c(2, 3), stats::median)
    data.frame(gear = ge, group = g$group, catch_t = cg,
               composition_W = cg / max(sum(cg), 1e-12),
               share_of_group_catch = cg / pmax(colSums(tot_g), 1e-12))
  }))
  gear_mmsy <- do.call(rbind, lapply(gears, function(ge) do.call(rbind, lapply(g$group, function(gi) {
    qq <- .q(alloc[, ge, gi]); cc <- stats::median(Cg[, ge, gi])
    data.frame(gear = ge, group = gi, MMSY_gear_p05_t = qq[["p05"]], MMSY_gear_p50_t = qq[["p50"]],
               MMSY_gear_p95_t = qq[["p95"]],
               MMSY_gear_target_t = unname(stats::quantile(alloc[, ge, gi], proj$settings$pstar)),
               catch_t = cc, ratio_MMSY_to_catch = ifelse(cc > 0, qq[["p50"]] / cc, NA),
               MMSY_gear_p50_lb = t_to_lb(qq[["p50"]]), catch_lb = t_to_lb(cc))
  }))))
  gear_mmsy <- gear_mmsy[gear_mmsy$catch_t > 0 | gear_mmsy$MMSY_gear_p50_t > 0, ]
  ceil <- do.call(rbind, lapply(gears, function(ge) {
    m <- numeric(nd); choke <- character(nd)
    for (d in seq_len(nd)) {
      cc <- Cg[d, ge, ]; ok <- cc > 1e-9 & g$harvest_allowed
      if (!any(ok)) { m[d] <- NA; choke[d] <- NA; next }
      rr <- alloc[d, ge, ok] / cc[ok]
      m[d] <- min(rr); choke[d] <- g$group[ok][which.min(rr)]
    }
    tab <- sort(table(choke), decreasing = TRUE)
    mq <- .q(m)
    data.frame(gear = ge, effort_ref = stats::median(Eff[, ge]),
               effort_multiplier_p05 = mq[["p05"]], effort_multiplier_p50 = mq[["p50"]],
               effort_multiplier_p95 = mq[["p95"]],
               effort_multiplier_target = unname(stats::quantile(m, proj$settings$pstar, na.rm = TRUE)),
               effort_ceiling_p50 = mq[["p50"]] * stats::median(Eff[, ge]),
               effort_ceiling_target = unname(stats::quantile(m, proj$settings$pstar, na.rm = TRUE)) *
                 stats::median(Eff[, ge]),
               choke_group = if (length(tab)) names(tab)[1] else NA,
               P_choke = if (length(tab)) as.numeric(tab[1]) / nd else NA,
               choke_alternatives = paste(sprintf("%s (%.0f%%)", names(tab), 100 * tab / nd), collapse = "; "))
  }))
  list(targeting = targeting, gear_mmsy = gear_mmsy, effort_ceilings = ceil, draws = alloc,
       method = method)
}

#' Ecosystem indicators through time (SEDAR103-DW-20 triggers)
#'
#' Total catch, mean trophic level of the catch, pelagic:demersal catch ratio,
#' total FMP MMSY, aggregate catch/MMSY, benthic and pelagic primary production.
#' @param res `ttm_results`
#' @param scenarios scenarios
#' @export
ecosystem_indicators <- function(res, scenarios = res$scenarios) {
  g <- res$proj$groups; fmp <- g$group_type == "fmp"; h <- res$proj$habitats
  TL <- res$TL[res$draws, , drop = FALSE]; bf <- res$bfrac[res$draws, , drop = FALSE]
  out <- list()
  for (s in scenarios) {
    C <- get_metric(res, "C", s); MM <- get_metric(res, "MMSY", s); P1 <- get_metric(res, "P1", s)
    B <- get_metric(res, "B", s)
    yrs <- dimnames(C)[[2]]
    for (y in yrs) {
      c_ <- C[, y, fmp, drop = FALSE][, 1, ]; c_ <- matrix(c_, nrow = dim(C)[1])
      tl <- TL[, fmp, drop = FALSE]; b <- bf[, fmp, drop = FALSE]
      tot <- rowSums(c_)
      ind <- list(
        total_catch_t = tot,
        mean_TL_catch = rowSums(c_ * tl) / pmax(tot, 1e-12),
        pelagic_demersal_ratio = rowSums(c_ * (1 - b)) / pmax(rowSums(c_ * b), 1e-12),
        total_MMSY_t = rowSums(matrix(MM[, y, fmp, drop = FALSE], nrow = dim(C)[1])),
        catch_over_MMSY = tot / rowSums(matrix(MM[, y, fmp, drop = FALSE], nrow = dim(C)[1])),
        fmp_biomass_t = rowSums(matrix(B[, y, fmp, drop = FALSE], nrow = dim(C)[1])),
        benthic_P1_t = rowSums(matrix(P1[, y, h$type == "benthic", drop = FALSE], nrow = dim(C)[1])),
        pelagic_P1_t = rowSums(matrix(P1[, y, h$type == "pelagic", drop = FALSE], nrow = dim(C)[1])))
      for (nm in names(ind)) {
        qq <- .q(ind[[nm]])
        out[[length(out) + 1]] <- data.frame(scenario = s, year = as.integer(y), indicator = nm,
                                             t(qq), check.names = FALSE)
      }
    }
  }
  do.call(rbind, out)
}

#' Compare TTM reference points with management benchmarks (OFL/ABC/ACL)
#' @param res `ttm_results`
#' @param scenario scenario
#' @export
benchmark_comparison <- function(res, scenario = "Baseline") {
  bm <- res$proj$benchmarks
  if (is.null(bm)) return(NULL)
  rp <- reference_points(res, scenario)
  bm <- merge(bm, rp[, c("group", "MMSY_p05_lb", "MMSY_p50_lb", "MMSY_p95_lb", "target_lb", "catch_lb")],
              by = "group", all.x = TRUE)
  bm$ratio_MMSYp50_to_benchmark <- bm$MMSY_p50_lb / bm$value_lb
  bm
}

#' Write all tables needed by a stochastic production frontier (SPF) analysis
#'
#' Files written to `dir`:
#' * `spf_group_reference_points.csv` - group MMSY quantiles, OFL proxy, target,
#'   B/BMSY, EU, status (t and lb)
#' * `spf_guild_mmsy.csv` - guild and FMP-complex aggregates
#' * `spf_gear_targeting_matrix.csv` - catch composition W_g,i and catch shares
#' * `spf_gear_mmsy.csv` - gear x group MMSY allocation
#' * `spf_gear_effort_ceilings.csv` - effort multipliers / ceilings + choke groups
#' * `spf_mmsy_draws.csv` - draw-level group MMSY (full distribution)
#' * `spf_mmsy_timeseries.csv` - MMSY quantiles by scenario and year
#' * `ecosystem_indicators.csv`, `benchmark_comparison.csv`, `sir_diagnostics.csv`
#' @param res `ttm_results`
#' @param dir output directory
#' @param scenario scenario for reference-point tables
#' @export
write_spf_inputs <- function(res, dir, scenario = "Baseline") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  w <- function(x, f) if (!is.null(x)) utils::write.csv(x, file.path(dir, f), row.names = FALSE)
  rp <- do.call(rbind, lapply(res$scenarios, function(s) reference_points(res, s)))
  w(rp, "spf_group_reference_points.csv")
  gl <- rbind(aggregate_mmsy(res, "guild", scenario), aggregate_mmsy(res, "fmp_complex", scenario))
  w(gl, "spf_guild_mmsy.csv")
  if (length(res$proj$gear_names)) {
    ga <- gear_allocation(res, scenario)
    w(ga$targeting, "spf_gear_targeting_matrix.csv")
    w(ga$gear_mmsy, "spf_gear_mmsy.csv")
    w(ga$effort_ceilings, "spf_gear_effort_ceilings.csv")
  }
  yy <- .ref_years(res)
  MM <- apply(get_metric(res, "MMSY", scenario)[, yy, , drop = FALSE], c(1, 3), mean)
  w(data.frame(draw = res$draws, MM, check.names = FALSE), "spf_mmsy_draws.csv")
  w(summarize_timeseries(res, "MMSY"), "spf_mmsy_timeseries.csv")
  w(ecosystem_indicators(res), "ecosystem_indicators.csv")
  w(benchmark_comparison(res, scenario), "benchmark_comparison.csv")
  if (!is.null(res$sir))
    w(data.frame(draw = seq_along(res$sir$weights), weight = res$sir$weights, res$sir$nll),
      "sir_diagnostics.csv")
  invisible(list.files(dir, full.names = TRUE))
}
