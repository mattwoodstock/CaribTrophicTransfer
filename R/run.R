# ==============================================================================
# Monte Carlo driver and Sampling-Importance-Resampling (SIR)
# ==============================================================================

.metrics <- c("B", "K", "K0", "P", "P0", "MMSY", "MMSY0", "C", "F")

#' Run the TTM over Monte Carlo draws and scenarios
#'
#' Each draw samples one parameter set ([draw_parameters()]) that is shared by
#' all scenarios, so scenario contrasts are paired. The run has three stages:
#' (1) the Baseline is simulated for every prior draw; (2) optional SIR
#' conditions the draws on indices, benchmarks and catch feasibility using the
#' (scenario-independent) historical period; (3) the remaining scenarios are
#' simulated only for the draws retained, which makes large prior samples
#' affordable. The deterministic (mean-parameter) run is stored as
#' `deterministic`.
#'
#' @param proj validated `ttm_project`
#' @param n_draws number of Monte Carlo draws (default `settings$n_draws`)
#' @param scenarios character vector of scenarios (default all)
#' @param sir logical; condition draws on indices (default TRUE if
#'   `indices.csv` exists and `settings$sir_resample > 0`)
#' @param verbose print progress
#' @return a `ttm_results` object
#' @export
run_ttm <- function(proj, n_draws = proj$settings$n_draws, scenarios = scenario_names(proj),
                    sir = NULL, verbose = TRUE) {
  st <- proj$settings
  set.seed(st$seed)
  years <- proj$years; g <- proj$groups
  drvs <- lapply(scenarios, function(s) build_drivers(proj, s)); names(drvs) <- scenarios
  per_draw <- has_driver_uncertainty(proj)
  if (verbose && per_draw) message("Driver uncertainty (covariate sd / disturbance intensity_cv): drivers rebuilt per draw")
  ny <- length(years); ng <- nrow(g)
  gnames <- if (length(proj$gear_names)) proj$gear_names else "none"
  alloc <- function(n) {
    res <- lapply(scenarios, function(s) {
      l <- lapply(.metrics, function(m) array(NA_real_, c(n, ny, ng), dimnames = list(NULL, years, g$group)))
      names(l) <- .metrics
      l$Cg <- array(NA_real_, c(n, ny, length(gnames), ng), dimnames = list(NULL, years, gnames, g$group))
      l$P1 <- array(NA_real_, c(n, ny, nrow(proj$habitats)), dimnames = list(NULL, years, proj$habitats$habitat))
      l$Q <- l$P1
      l$Eff <- array(NA_real_, c(n, ny, length(gnames)), dimnames = list(NULL, years, gnames))
      l
    })
    names(res) <- scenarios
    res
  }
  # ---- adaptive importance sampling set-up ------------------------------------
  if (is.null(sir)) sir <- (!is.null(proj$indices) || !is.null(proj$benchmarks) ||
                           isTRUE(st$sir_catch_feasibility)) && isTRUE(st$sir_resample > 0)
  rounds <- if (sir && isTRUE(st$sir_adaptive_rounds > 0)) as.integer(st$sir_adaptive_rounds) else 0L
  pv_groups <- g$group[g$partition_cv > 0 & g$partition_method != "fixed_biomass" & g$group %in% focus_groups(proj)]
  s_pr <- sqrt(log(1 + g[pv_groups, "partition_cv"]^2))
  prior_mu <- log(g[pv_groups, "partition_value"]) - s_pr^2 / 2
  proposals <- list(list(mu = prior_mu, sd = s_pr, n = n_draws))
  N <- n_draws * (rounds + 1L)
  TL <- matrix(NA_real_, N, ng, dimnames = list(NULL, g$group))
  bfrac <- TL; shortfall <- TL
  seeds <- vector("list", N)   # RNG state per draw: parameters are regenerated in Stage 3
  part_vals <- matrix(NA_real_, N, length(pv_groups), dimnames = list(NULL, pv_groups))
  # deterministic run
  par0 <- draw_parameters(proj, stochastic = FALSE)
  det <- lapply(scenarios, function(s) {
    base <- simulate_ttm(proj, par0, drvs[["Baseline"]])
    if (s == "Baseline") base else simulate_ttm(proj, par0, drvs[[s]], q = base$q)
  }); names(det) <- scenarios
  qs <- vector("list", N)
  # Stage 1: Baseline for every draw (prior round + adaptive rounds). Only what
  # SIR and prior summaries need is kept for all draws (memory-light).
  ref_y <- as.character(seq(st$hist_end - st$ref_years + 1, st$hist_end))
  stage1 <- list(B = array(NA_real_, c(N, ny, ng), dimnames = list(NULL, years, g$group)))
  for (m in c("MMSY", "MMSY0", "K", "K0", "B", "C", "P")) stage1[[paste0(m, "_ref")]] <- TL
  log_mix <- function(x) {   # log density of the mixture of proposals used so far (log scale)
    lx <- log(x)
    comp <- vapply(proposals, function(pp) rowSums(stats::dnorm(lx, rep(pp$mu, each = nrow(lx)),
                                                               rep(pp$sd, each = nrow(lx)), log = TRUE)), numeric(nrow(lx)))
    comp <- matrix(comp, nrow = nrow(lx))
    nr <- vapply(proposals, function(pp) pp$n, numeric(1)); lw <- log(nr / sum(nr))
    apply(sweep(comp, 2, lw, "+"), 1, function(z) { mz <- max(z); mz + log(sum(exp(z - mz))) })
  }
  log_prior <- function(x) rowSums(stats::dnorm(log(x), rep(prior_mu, each = nrow(x)), rep(s_pr, each = nrow(x)), log = TRUE))
  d <- 0L
  cores <- if (is.null(st$cores) || is.na(st$cores)) 1L else as.integer(st$cores)
  if (.Platform$OS.type == "windows") cores <- 1L
  old_kind <- RNGkind()[1]; RNGkind("L'Ecuyer-CMRG"); set.seed(st$seed)
  stream <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(RNGkind(old_kind), add = TRUE)
  for (rd in 0:rounds) {
    pp <- proposals[[rd + 1L]]
    if (verbose) message(if (rd == 0) "Stage 1: Baseline, " else sprintf("Stage 1 (adaptive round %d): ", rd),
                         n_draws, " draws", if (cores > 1) sprintf(" on %d cores", cores) else "")
    # reproducible per-draw RNG streams (independent of the number of cores)
    idx_d <- d + seq_len(n_draws)
    for (dd in idx_d) { seeds[[dd]] <- stream; stream <- parallel::nextRNGStream(stream) }
    one_draw <- function(dd) {
      assign(".Random.seed", seeds[[dd]], envir = .GlobalEnv)
      par <- draw_parameters(proj, stochastic = TRUE)
      if (rd > 0 && length(pv_groups))
        par$partition_value[pv_groups] <- exp(stats::rnorm(length(pv_groups), pp$mu, pp$sd))
      base <- simulate_ttm(proj, par, .drv_for(proj, drvs, "Baseline", par, per_draw))
      list(pv_part = par$partition_value[pv_groups], q = base$q, B = base$B, params = .param_vector(par, proj),
           ref = lapply(setNames(nm = c("MMSY", "MMSY0", "K", "K0", "B", "C", "P")),
                        function(m) colMeans(base[[m]][ref_y, , drop = FALSE])),
           TL = base$TL, bfrac = base$bfrac, shortfall = base$catch_shortfall)
    }
    outs <- if (cores > 1) parallel::mclapply(idx_d, one_draw, mc.cores = cores, mc.preschedule = TRUE)
            else lapply(idx_d, one_draw)
    for (k in seq_along(idx_d)) {
      dd <- idx_d[k]; o <- outs[[k]]
      if (inherits(o, "try-error")) stop("Draw ", dd, " failed: ", o)
      part_vals[dd, ] <- o$pv_part; qs[[dd]] <- o$q; stage1$B[dd, , ] <- o$B
      if (is.null(stage1$params)) stage1$params <- matrix(NA_real_, N, length(o$params), dimnames = list(NULL, names(o$params)))
      stage1$params[dd, names(o$params)] <- o$params
      for (m in names(o$ref)) stage1[[paste0(m, "_ref")]][dd, ] <- o$ref[[m]]
      TL[dd, ] <- o$TL; bfrac[dd, ] <- o$bfrac; shortfall[dd, ] <- o$shortfall
    }
    rm(outs)
    d <- max(idx_d)
    if (rd < rounds) {   # fit the next proposal to the current importance-weighted posterior
      tmp <- list(proj = proj, stage1 = lapply(stage1, function(a) if (is.matrix(a)) a[seq_len(d), , drop = FALSE] else a[seq_len(d), , , drop = FALSE]),
                  catch_shortfall = shortfall[seq_len(d), , drop = FALSE])
      tmp$stage1$log_is_correction <- log_prior(part_vals[seq_len(d), , drop = FALSE]) - log_mix(part_vals[seq_len(d), , drop = FALSE])
      nll <- .total_nll(tmp, .fit_indices(proj), .sir_bm(proj))
      w <- exp(-(nll - min(nll))); w <- w / sum(w)
      lx <- log(part_vals[seq_len(d), , drop = FALSE])
      # Factorized proposal: each group's partition proposal is fitted to the
      # group's own likelihood components (catch feasibility, own benchmarks);
      # the final weights still use the joint likelihood with the exact
      # mixture correction, so only sampling efficiency is affected.
      cf <- catch_feasibility_nll(tmp)
      bmj <- .sir_bm(proj)
      nb <- .bench_nll_rows(tmp$stage1$MMSY_ref, bmj)
      ni <- .index_nll_rows(tmp$stage1$B, .fit_indices(proj), st$index_min_cv)
      shared <- if (ncol(ni)) rowSums(ni) else 0
      corr <- tmp$stage1$log_is_correction
      mu <- sdv <- setNames(numeric(length(pv_groups)), pv_groups)
      for (k in seq_along(pv_groups)) {
        gi <- pv_groups[k]
        own <- shared - corr
        if (!is.null(cf) && paste0("feasibility_", gi) %in% colnames(cf)) own <- own + cf[, paste0("feasibility_", gi)]
        if (ncol(nb)) { jj <- grepl(paste0("(^|;)", gi, "(;|_)"), colnames(nb)); if (any(jj)) own <- own + rowSums(nb[, jj, drop = FALSE]) }
        wk <- exp(-(own - min(own))); wk <- wk / sum(wk)
        if (1 / sum(wk^2) > 0.5 * length(wk)) {   # prior already adequate for this group: do not adapt
          mu[k] <- prior_mu[k]; sdv[k] <- s_pr[k] / 1.5
        } else {
          mu[k] <- sum(lx[, k] * wk); sdv[k] <- sqrt(sum(wk * (lx[, k] - mu[k])^2))
        }
      }
      proposals[[rd + 2L]] <- list(mu = mu, sd = pmax(0.1, 1.5 * sdv), n = n_draws)
      if (verbose) message("  groups adapted: ", paste(pv_groups[abs(mu - prior_mu) > 1e-9], collapse = ", "))
      if (verbose) message(sprintf("  joint importance ESS after round %d: %.1f", rd, 1 / sum(w^2)))
    }
  }
  n_draws <- N
  stage1$log_is_correction <- if (rounds > 0) log_prior(part_vals) - log_mix(part_vals) else rep(0, N)
  stage1$partition_draws <- part_vals
  out <- list(proj = proj, results = NULL, deterministic = det, TL = TL, bfrac = bfrac,
              catch_shortfall = shortfall, stage1 = stage1, n_prior = n_draws,
              draws = seq_len(n_draws), weights = rep(1 / n_draws, n_draws),
              scenarios = scenarios, sir = NULL)
  class(out) <- "ttm_results"
  # Stage 2: SIR on the Baseline history
  if (sir) out <- apply_sir(out, n_resample = st$sir_resample)
  # Stage 3: full results for all scenarios, only for the draws in use
  keep <- sort(unique(out$draws))
  res <- alloc(length(keep))
  if (verbose) message("Stage 3: ", length(scenarios), " scenarios x ", length(keep), " draws")
  pars <- vector("list", length(keep))
  rng_now <- get(".Random.seed", envir = .GlobalEnv)
  for (k in seq_along(keep)) {
    d <- keep[k]
    assign(".Random.seed", seeds[[d]], envir = .GlobalEnv)
    pars[[k]] <- draw_parameters(proj, stochastic = TRUE)
    if (length(pv_groups)) pars[[k]]$partition_value[pv_groups] <- part_vals[d, ]
    for (s in scenarios) {
      o <- simulate_ttm(proj, pars[[k]], .drv_for(proj, drvs, s, pars[[k]], per_draw),
                        q = if (s == "Baseline") NULL else qs[[d]])
      for (m in .metrics) res[[s]][[m]][k, , ] <- o[[m]]
      res[[s]]$Cg[k, , , ] <- o$Cg; res[[s]]$P1[k, , ] <- o$P1; res[[s]]$Q[k, , ] <- o$Q
      res[[s]]$Eff[k, , ] <- o$Eff
    }
  }
  assign(".Random.seed", rng_now, envir = .GlobalEnv)
  out$results <- res
  out$pars <- pars          # parameter sets of the stored (kept) draws
  out$keep <- keep
  out$row_of_draw <- match(seq_len(n_draws), keep)
  out
}

#' Groups that constrain the calibration
#'
#' `settings$sir_feasibility_groups`: "all" (every harvested group), "trap"
#' (groups with `trap_relevant = TRUE` and harvest allowed) or a
#' `;`-separated list. Groups with SIR benchmarks are always included.
#' Other harvested groups remain in the food web (energy competitors whose
#' landings are still removed) but do not constrain the posterior, so sampling
#' effort concentrates on the fishery of interest.
#' @param proj a `ttm_project`
#' @export
focus_groups <- function(proj) {
  g <- proj$groups; st <- proj$settings
  sel <- if (is.null(st$sir_feasibility_groups) || is.na(st$sir_feasibility_groups)) "all" else st$sir_feasibility_groups
  harv <- g$group[g$harvest_allowed & g$group_type == "fmp"]
  out <- switch(sel,
    all = harv,
    trap = g$group[g$trap_relevant & g$harvest_allowed],
    intersect(strsplit(sel, ";")[[1]], g$group))
  bm <- proj$benchmarks
  if (!is.null(bm) && "use_in_sir" %in% names(bm))
    out <- union(out, unlist(strsplit(bm$group[as.logical(bm$use_in_sir) %in% TRUE], ";")))
  out
}

#' Does the project carry uncertainty in its drivers?
#'
#' TRUE when env_covariates.csv has a positive `sd` or disturbances.csv a
#' positive `intensity_cv`; drivers are then rebuilt for every draw.
#' @param proj a `ttm_project`
#' @export
has_driver_uncertainty <- function(proj) {
  ec <- proj$env_covariates; dd <- proj$disturbances
  isTRUE(proj$settings$disturbance_loss_cv > 0) ||
  (!is.null(ec) && "sd" %in% names(ec) && any(ec$sd > 0, na.rm = TRUE)) ||
    (!is.null(dd) && "intensity_cv" %in% names(dd) && any(dd$intensity_cv > 0, na.rm = TRUE))
}

.drv_for <- function(proj, drvs, s, par, per_draw) {
  if (per_draw) build_drivers(proj, s, par) else drvs[[s]]
}

#' Index log-likelihood for every draw
#'
#' Only fitting observations are used (index rows with `role = "fit"` and
#' years before `settings$holdout_from_year`; see [validate_out_of_sample()]).
#' For each index series, simulated biomass of the matching group(s) (sum if
#' several groups separated by ";") is compared with the observed index under
#' a lognormal error model with analytical catchability:
#' log q = sum(w (log I - log B)) / sum(w), w = 1/sigma^2,
#' sigma = sqrt(log(1 + cv^2)).
#' @param res `ttm_results`
#' @return matrix draws x index of negative log-likelihoods
#' @export
index_nll <- function(res) {
  idx <- .fit_indices(res$proj)
  if (is.null(idx) || !nrow(idx)) stop("No fitting index observations")
  .index_nll_rows(res$stage1$B, idx, res$proj$settings$index_min_cv)
}

#' Benchmark (assessment reference point) log-likelihood for every draw
#'
#' Rows of benchmarks.csv with `use_in_sir = TRUE` anchor the ABSOLUTE scale
#' of the model: simulated prevailing MMSY (mean over the reference years, lb)
#' is compared with the benchmark value under lognormal error with CV
#' `sir_cv`. Intended for assessment-derived OFL / MSY values only (e.g.,
#' SEDAR 57 spiny lobster, SEDAR 80 queen triggerfish); ACLs embed
#' management buffers and should not be used.
#' @param res `ttm_results`
#' @return matrix draws x benchmark of negative log-likelihoods (or NULL)
#' @export
benchmark_nll <- function(res) {
  bm <- res$proj$benchmarks
  if (is.null(bm) || !"use_in_sir" %in% names(bm)) return(NULL)
  bm <- bm[as.logical(bm$use_in_sir) %in% TRUE, , drop = FALSE]
  if (!nrow(bm)) return(NULL)
  MM <- res$stage1$MMSY_ref
  nll <- matrix(0, nrow(MM), nrow(bm), dimnames = list(NULL, paste(bm$group, bm$metric, sep = "_")))
  for (j in seq_len(nrow(bm))) {
    grp <- strsplit(bm$group[j], ";")[[1]]
    pred <- t_to_lb(rowSums(MM[, grp, drop = FALSE]))
    sig <- sqrt(log(1 + ifelse(is.na(bm$sir_cv[j]), 0.5, bm$sir_cv[j])^2))
    nll[, j] <- log(sig) + 0.5 * ((log(bm$value_lb[j]) - log(pred)) / sig)^2
  }
  nll
}

#' Catch-feasibility (stock reduction analysis) log-likelihood
#'
#' Penalizes draws in which the simulated stock could not support the observed
#' (expanded) historical landings in some year, i.e. realized catch fell short
#' of landings because biomass was exhausted (largest annual shortfall). This is the logic of stock reduction analysis
#' (Kimura & Tagart 1982; Walters et al. 2006) and catch-MSY (Martell & Froese
#' 2013): observed removals bound productivity from below. Penalty per group:
#' 0.5 * (log(1 - shortfall) / cv)^2 with cv = `settings$catch_feasibility_cv`.
#' Enabled with `settings$sir_catch_feasibility = TRUE`. Only the groups in
#' [focus_groups()] (`settings$sir_feasibility_groups`) are constrained.
#' @param res `ttm_results`
#' @export
catch_feasibility_nll <- function(res) {
  st <- res$proj$settings
  if (!isTRUE(st$sir_catch_feasibility) || is.null(res$catch_shortfall)) return(NULL)
  sf <- res$catch_shortfall
  sf <- sf[, colSums(!is.na(sf)) > 0 & colnames(sf) %in% focus_groups(res$proj), drop = FALSE]
  if (!ncol(sf)) return(NULL)
  cv <- if (is.null(st$catch_feasibility_cv)) 0.1 else st$catch_feasibility_cv
  out <- 0.5 * (log(pmax(1 - sf, 1e-6)) / cv)^2
  out[is.na(out)] <- 0
  colnames(out) <- paste0("feasibility_", colnames(out))
  out
}

#' Condition Monte Carlo draws with Sampling-Importance-Resampling (SIR)
#'
#' Likelihood components: relative abundance indices ([index_nll()], shape of
#' biomass trends), assessment benchmarks ([benchmark_nll()], absolute scale)
#' and catch feasibility ([catch_feasibility_nll()], lower bound on productivity). Importance weights are proportional to exp(-total NLL)
#' (index components weighted by the `lambda` column of indices.csv). Draws
#' are resampled with replacement.
#' @param res `ttm_results`
#' @param n_resample number of resampled draws (default = number of draws)
#' @export
apply_sir <- function(res, n_resample = length(res$draws)) {
  if (is.null(n_resample) || n_resample <= 0) n_resample <- length(res$draws)
  comps <- list()
  idx <- .fit_indices(res$proj)
  if (!is.null(idx) && nrow(idx)) {
    nll <- index_nll(res)
    lam <- if ("lambda" %in% names(idx)) tapply(idx$lambda, idx$index, function(z) z[1])[colnames(nll)] else
      rep(1, ncol(nll))
    comps$indices <- sweep(nll, 2, lam, "*")
  }
  bn <- benchmark_nll(res)
  if (!is.null(bn)) comps$benchmarks <- bn
  cf <- catch_feasibility_nll(res)
  if (!is.null(cf)) comps$catch_feasibility <- cf
  if (!length(comps)) stop("SIR requested but no indices or SIR benchmarks are available")
  nll_all <- do.call(cbind, comps)
  tot <- rowSums(nll_all)
  if (!is.null(res$stage1$log_is_correction)) tot <- tot - res$stage1$log_is_correction
  lw <- -(tot - min(tot))
  wts <- exp(lw); wts <- wts / sum(wts)
  ess <- 1 / sum(wts^2)
  set.seed(res$proj$settings$seed + 1)
  pick <- sample(seq_along(wts), n_resample, replace = TRUE, prob = wts)
  res$sir <- list(nll = nll_all, weights = wts, ess = ess, resampled = pick,
                  n_unique = length(unique(pick)))
  ws <- sort(wts, decreasing = TRUE); m <- max(5, floor(0.2 * length(ws)))
  res$sir$pareto_k_hat <- mean(log(ws[seq_len(m - 1)]) - log(ws[m]))
  res$draws <- pick
  res$weights <- rep(1 / n_resample, n_resample)
  if (ess < 20) warning(sprintf("SIR effective sample size is low (%.1f); increase n_draws or review priors/CVs", ess))
  res
}

#' Return a copy of results using the unconditioned (prior) draws
#'
#' Full results are stored only for draws retained by SIR. This returns the
#' prior when all draws were retained (no SIR); otherwise use
#' [prior_reference_points()] for prior summaries.
#' @param res `ttm_results`
#' @export
prior_results <- function(res) {
  if (length(res$keep) < res$n_prior)
    stop("Full prior results are not stored when SIR is used; see prior_reference_points()")
  res$draws <- seq_len(res$n_prior); res$weights <- rep(1 / length(res$draws), length(res$draws))
  res$sir <- NULL
  res
}

#' Prior (unconditioned) reference-point summary for every group
#'
#' Uses the reference-year means kept for all prior draws in Stage 1.
#' @param res `ttm_results`
#' @export
prior_reference_points <- function(res) {
  s1 <- res$stage1; g <- res$proj$groups
  q <- function(x, p) unname(stats::quantile(x, p, na.rm = TRUE))
  data.frame(group = g$group, group_type = g$group_type,
             MMSY_p05_t = apply(s1$MMSY_ref, 2, q, 0.05), MMSY_p50_t = apply(s1$MMSY_ref, 2, q, 0.5),
             MMSY_p95_t = apply(s1$MMSY_ref, 2, q, 0.95), B0_p50_t = apply(s1$K0_ref, 2, q, 0.5),
             catch_p50_t = apply(s1$C_ref, 2, q, 0.5),
             P_infeasible_landings = colMeans(res$catch_shortfall > 0.05, na.rm = TRUE),
             MMSY_p50_lb = t_to_lb(apply(s1$MMSY_ref, 2, q, 0.5)), row.names = NULL)
}

#' @export
print.ttm_results <- function(x, ...) {
  cat("<ttm_results>", x$proj$settings$project_name, "\n")
  cat("  prior draws:", x$n_prior, " used:", length(x$draws), " stored:", length(x$keep),
      if (!is.null(x$sir)) sprintf("(SIR, ESS = %.1f, unique = %d)", x$sir$ess, x$sir$n_unique) else "(prior)", "\n")
  cat("  scenarios:", paste(x$scenarios, collapse = ", "), "\n")
  invisible(x)
}
