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
  TL <- matrix(NA_real_, n_draws, ng, dimnames = list(NULL, g$group))
  bfrac <- TL; shortfall <- TL
  seeds <- vector("list", n_draws)   # RNG state per draw: parameters are regenerated in Stage 3
  # deterministic run
  par0 <- draw_parameters(proj, stochastic = FALSE)
  det <- lapply(scenarios, function(s) {
    base <- simulate_ttm(proj, par0, drvs[["Baseline"]])
    if (s == "Baseline") base else simulate_ttm(proj, par0, drvs[[s]], q = base$q)
  }); names(det) <- scenarios
  qs <- vector("list", n_draws)
  # Stage 1: Baseline for every prior draw. Only what SIR and prior summaries
  # need is kept for all draws (memory-light): biomass trajectories and
  # reference-year means of the main metrics.
  ref_y <- as.character(seq(st$hist_end - st$ref_years + 1, st$hist_end))
  stage1 <- list(B = array(NA_real_, c(n_draws, ny, ng), dimnames = list(NULL, years, g$group)))
  for (m in c("MMSY", "MMSY0", "K", "K0", "B", "C", "P")) stage1[[paste0(m, "_ref")]] <- TL
  if (verbose) message("Stage 1: Baseline, ", n_draws, " prior draws")
  for (d in seq_len(n_draws)) {
    if (verbose && (d %% max(1, n_draws %/% 10) == 0)) message("  draw ", d, "/", n_draws)
    seeds[[d]] <- get(".Random.seed", envir = .GlobalEnv)
    par <- draw_parameters(proj, stochastic = TRUE)
    base <- simulate_ttm(proj, par, .drv_for(proj, drvs, "Baseline", par, per_draw))
    qs[[d]] <- base$q
    stage1$B[d, , ] <- base$B
    for (m in c("MMSY", "MMSY0", "K", "K0", "B", "C", "P"))
      stage1[[paste0(m, "_ref")]][d, ] <- colMeans(base[[m]][ref_y, , drop = FALSE])
    TL[d, ] <- base$TL; bfrac[d, ] <- base$bfrac; shortfall[d, ] <- base$catch_shortfall
  }
  out <- list(proj = proj, results = NULL, deterministic = det, TL = TL, bfrac = bfrac,
              catch_shortfall = shortfall, stage1 = stage1, n_prior = n_draws,
              draws = seq_len(n_draws), weights = rep(1 / n_draws, n_draws),
              scenarios = scenarios, sir = NULL)
  class(out) <- "ttm_results"
  # Stage 2: SIR on the Baseline history
  if (is.null(sir)) sir <- (!is.null(proj$indices) || !is.null(proj$benchmarks) ||
                           isTRUE(st$sir_catch_feasibility)) && isTRUE(st$sir_resample > 0)
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

#' Does the project carry uncertainty in its drivers?
#'
#' TRUE when env_covariates.csv has a positive `sd` or disturbances.csv a
#' positive `intensity_cv`; drivers are then rebuilt for every draw.
#' @param proj a `ttm_project`
#' @export
has_driver_uncertainty <- function(proj) {
  ec <- proj$env_covariates; dd <- proj$disturbances
  (!is.null(ec) && "sd" %in% names(ec) && any(ec$sd > 0, na.rm = TRUE)) ||
    (!is.null(dd) && "intensity_cv" %in% names(dd) && any(dd$intensity_cv > 0, na.rm = TRUE))
}

.drv_for <- function(proj, drvs, s, par, per_draw) {
  if (per_draw) build_drivers(proj, s, par) else drvs[[s]]
}

#' Index log-likelihood for every draw
#'
#' For each index series, simulated biomass of the matching group(s) (sum if
#' several groups separated by ";") is compared with the observed index under
#' a lognormal error model with analytical catchability:
#' log q = sum(w (log I - log B)) / sum(w), w = 1/sigma^2,
#' sigma = sqrt(log(1 + cv^2)).
#' @param res `ttm_results`
#' @return matrix draws x index of negative log-likelihoods
#' @export
index_nll <- function(res) {
  idx <- res$proj$indices
  if (is.null(idx) || !nrow(idx)) stop("No indices.csv in project")
  B <- res$stage1$B
  years <- as.integer(dimnames(B)[[2]])
  idx <- idx[idx$year %in% years, , drop = FALSE]
  names_i <- unique(idx$index)
  nll <- matrix(0, dim(B)[1], length(names_i), dimnames = list(NULL, names_i))
  for (nm in names_i) {
    x <- idx[idx$index == nm, ]
    grp <- strsplit(x$group[1], ";")[[1]]
    sig <- sqrt(log(1 + pmax(x$cv, res$proj$settings$index_min_cv)^2))
    wv <- 1 / sig^2
    yi <- as.character(x$year)
    for (d in seq_len(dim(B)[1])) {
      b <- rowSums(matrix(B[d, yi, grp, drop = FALSE], nrow = length(yi)))
      lr <- log(x$value) - log(pmax(b, 1e-12))
      lq <- sum(wv * lr) / sum(wv)
      nll[d, nm] <- sum(log(sig) + 0.5 * wv * (lr - lq)^2)
    }
  }
  nll
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
#' Enabled with `settings$sir_catch_feasibility = TRUE`.
#' @param res `ttm_results`
#' @export
catch_feasibility_nll <- function(res) {
  st <- res$proj$settings
  if (!isTRUE(st$sir_catch_feasibility) || is.null(res$catch_shortfall)) return(NULL)
  sf <- res$catch_shortfall
  sf <- sf[, colSums(!is.na(sf)) > 0, drop = FALSE]
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
  if (!is.null(res$proj$indices) && nrow(res$proj$indices)) {
    nll <- index_nll(res)
    idx <- res$proj$indices
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
  lw <- -(tot - min(tot))
  wts <- exp(lw); wts <- wts / sum(wts)
  ess <- 1 / sum(wts^2)
  set.seed(res$proj$settings$seed + 1)
  pick <- sample(seq_along(wts), n_resample, replace = TRUE, prob = wts)
  res$sir <- list(nll = nll_all, weights = wts, ess = ess, resampled = pick,
                  n_unique = length(unique(pick)))
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
