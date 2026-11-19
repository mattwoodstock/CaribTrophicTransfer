# ==============================================================================
# Robustness analyses: global sensitivity, SIR diagnostics, simulation
# self-testing, ecosystem-overfishing indices and structural ensembles
# ==============================================================================

# Scalar summary of a parameter draw (used for global sensitivity analysis and
# prior/posterior shrinkage). Multi-valued inputs are summarised by name.
.param_vector <- function(par, proj) {
  g <- proj$groups; fmp <- g$group_type == "fmp"
  v <- c(
    setNames(par$npp, paste0("npp:", names(par$npp))),
    setNames(par$area, paste0("area:", names(par$area))),
    setNames(par$fwf, paste0("fwf:", names(par$fwf))),
    setNames(par$tte, paste0("tte:", names(par$tte))),
    c_to_ww = par$c_to_ww, catch_expansion = par$catch_expansion,
    bleach_mort = par$bleach_mort_per_dhw,
    coral_growth = if (is.null(par$coral_growth)) NA else par$coral_growth,
    setNames(par$pb[fmp], paste0("pb:", g$group[fmp])),
    setNames(par$x_msy[fmp], paste0("x_msy:", g$group[fmp])),
    setNames(par$partition_value, paste0("partition:", g$group)),
    setNames(par$init_depletion[fmp], paste0("init_dep:", g$group[fmp])))
  if (!is.null(par$landings_mult)) v["landings_obs_err"] <- exp(mean(log(par$landings_mult)))
  if (!is.null(par$dist_loss_mult)) v["dist_loss_mult"] <- par$dist_loss_mult
  if (!is.null(par$dist_intensity_mult)) v["dist_intensity_err"] <- exp(mean(log(par$dist_intensity_mult)))
  if (!is.null(par$m_nat)) {
    mv <- par$m_nat[fmp]; keep <- !is.na(g$m_nat[fmp])
    if (any(keep)) v <- c(v, setNames(mv[keep], paste0("M:", g$group[fmp][keep])))
  }
  if (!is.null(par$open_rec) && any(par$open_rec > 0))
    v <- c(v, setNames(par$open_rec[par$open_rec > 0], paste0("open_rec:", g$group[par$open_rec > 0])))
  if (!is.null(par$ghost_eff) && par$ghost_eff > 0) v["ghost_efficiency"] <- par$ghost_eff
  if (!is.null(par$herbivory_elasticity) && par$herbivory_elasticity > 0) v["herbivory_elasticity"] <- par$herbivory_elasticity
  if (!is.null(par$env_sens) && !is.null(proj$env_response)) {
    er <- proj$env_response; ix <- which(er$variable %in% c("lionfish", "sargassum") & er$sensitivity_cv > 0)
    if (length(ix)) v <- c(v, setNames(par$env_sens[ix], paste0("env:", er$variable[ix], ":", er$group[ix])))
  }
  v
}

#' Global sensitivity analysis of reference points to inputs
#'
#' Uses the Stage-1 Monte Carlo draws (every draw samples every input),
#' resampled to the prior when adaptive importance sampling was used.
#' For each output, reports standardized rank regression coefficients (SRRC,
#' rank-transformed multiple regression; R^2 indicates how much of the output
#' variance is explained by a monotone-additive model) and partial rank
#' correlation coefficients (PRCC). Inputs with no variation are dropped.
#' @param res `ttm_results`
#' @param outputs named list of functions of `res$stage1` returning one value
#'   per draw; default: total FMP MMSY and MMSY of each assessed/trap group
#' @param top number of inputs to keep per output (ranked by |SRRC|)
#' @export
# Rows of the stored Stage-1 draws resampled to represent the PRIOR. With adaptive
# importance sampling the stored draws follow the proposal mixture, so they are
# resampled with probability proportional to prior / proposal (stored log ratio).
.prior_rows <- function(res, n = NULL, seed = 5) {
  lc <- res$stage1$log_is_correction
  N <- if (!is.null(res$stage1$MMSY_ref)) nrow(res$stage1$MMSY_ref) else length(lc)
  if (is.null(lc) || all(lc == 0)) return(seq_len(N))
  if (is.null(n)) n <- N
  w <- exp(lc - max(lc)); w <- w / sum(w)
  old <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed); idx <- sample.int(N, n, replace = TRUE, prob = w)
  if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
  idx
}

sensitivity_analysis <- function(res, outputs = NULL, top = 15) {
  pr <- .prior_rows(res)
  X <- res$stage1$params[pr, , drop = FALSE]
  if (is.null(X)) stop("No parameter matrix stored; rerun run_ttm() with this package version")
  g <- res$proj$groups; fmp <- intersect(focus_groups(res$proj), g$group[g$harvest_allowed])
  if (is.null(outputs)) {
    outputs <- list(total_focus_MMSY = function(s) rowSums(s$MMSY_ref[, fmp, drop = FALSE]))
    for (gi in intersect(c("spiny_lobster", "queen_triggerfish", "parrotfish_2", "grunts", "snappers_shallow", "groupers_shallow"), fmp))
      outputs[[paste0("MMSY_", gi)]] <- local({ gg <- gi; function(s) s$MMSY_ref[, gg] })
  }
  keep <- apply(X, 2, function(z) all(is.finite(z)) && stats::sd(z) > 0)
  X <- X[, keep, drop = FALSE]
  R <- apply(X, 2, rank)
  out <- list()
  for (nm in names(outputs)) {
    y <- outputs[[nm]](res$stage1)[pr]
    ok <- is.finite(y)
    ry <- rank(y[ok]); Rx <- scale(R[ok, , drop = FALSE]); ry <- as.numeric(scale(ry))
    fit <- stats::lm.fit(cbind(1, Rx), ry)
    srrc <- fit$coefficients[-1]
    r2 <- 1 - sum(fit$residuals^2) / sum((ry - mean(ry))^2)
    ord <- order(-abs(srrc))[seq_len(min(top, length(srrc)))]
    prcc <- vapply(ord, function(j) {
      ex <- stats::lm.fit(cbind(1, Rx[, -j]), Rx[, j])$residuals
      ey <- stats::lm.fit(cbind(1, Rx[, -j]), ry)$residuals
      stats::cor(ex, ey)
    }, numeric(1))
    out[[nm]] <- data.frame(output = nm, input = colnames(Rx)[ord], SRRC = round(srrc[ord], 3),
                            PRCC = round(prcc, 3), R2_rank_model = round(r2, 3), row.names = NULL)
  }
  do.call(rbind, out)
}

#' SIR diagnostics: effective sample size, Pareto-k, weight concentration and
#' posterior shrinkage of each input
#'
#' * ESS = 1 / sum(w^2).
#' * Pareto-k: generalized-Pareto shape of the importance-weight tail from
#'   PSIS ([psis_smooth()]); k > 0.7 indicates unreliable importance sampling
#'   (Vehtari et al. 2024). ESS_raw is before Pareto smoothing.
#' * Shrinkage = 1 - var_posterior / var_prior for each sampled input: values
#'   near 1 are strongly informed by the data, near 0 prior-dominated.
#' @param res `ttm_results` with SIR applied
#' @export
sir_diagnostics <- function(res) {
  if (is.null(res$sir)) stop("No SIR in results")
  w <- res$sir$weights
  ws <- sort(w, decreasing = TRUE)
  k_hat <- if (!is.null(res$sir$pareto_k_hat)) res$sir$pareto_k_hat else NA_real_
  summ <- data.frame(n_prior = length(w), ESS = 1 / sum(w^2),
                     ESS_raw = if (is.null(res$sir$ess_raw)) NA else res$sir$ess_raw,
                     unique_resampled = res$sir$n_unique,
                     max_weight = max(w), top10_weight_share = sum(ws[seq_len(min(10, length(ws)))]),
                     pareto_k_hat = k_hat,
                     weight_method = if (is.null(res$sir$weight_method)) NA else res$sir$weight_method)
  X <- res$stage1$params
  shrink <- NULL
  if (!is.null(X)) {
    keep <- apply(X, 2, function(z) all(is.finite(z)) && stats::sd(z) > 0)
    X <- X[, keep, drop = FALSE]
    mu <- colSums(X * w); vpost <- colSums(w * sweep(X, 2, mu)^2)
    lc <- res$stage1$log_is_correction   # prior moments undo the adaptive proposal
    wp <- if (is.null(lc)) rep(1 / nrow(X), nrow(X)) else { z <- exp(lc - max(lc)); z / sum(z) }
    mp <- colSums(X * wp); vprior <- colSums(wp * sweep(X, 2, mp)^2)
    shrink <- data.frame(input = colnames(X), prior_mean = mp, posterior_mean = mu,
                         shrinkage = pmax(0, 1 - vpost / vprior), row.names = NULL)
    shrink <- shrink[order(-shrink$shrinkage), ]
  }
  list(summary = summ, shrinkage = shrink)
}

#' Simulation self-test (estimation-model consistency)
#'
#' Repeatedly treats one prior draw as the "truth", generates pseudo-data from
#' it (indices with their observation CVs at the observed years, benchmarks
#' with their SIR CVs) and re-runs the SIR weighting on the remaining draws.
#' Reports, per group, the median relative error of the posterior median MMSY
#' and the coverage of the 50% and 90% posterior intervals. A well-behaved
#' estimator has small bias and coverage close to nominal.
#' Uses Stage-1 quantities only (no new simulations).
#' @param res `ttm_results`
#' @param n_rep replicates
#' @param groups groups to evaluate (default FMP harvested groups)
#' @param seed RNG seed
#' @export
self_test <- function(res, n_rep = 100, groups = NULL, seed = 11) {
  set.seed(seed)
  s1 <- res$stage1; proj <- res$proj; g <- proj$groups
  if (is.null(groups)) groups <- intersect(focus_groups(proj), g$group[g$harvest_allowed])
  fg <- intersect(focus_groups(proj), colnames(res$catch_shortfall))
  feas <- if (is.null(res$catch_shortfall)) rep(TRUE, nrow(s1$MMSY_ref)) else
    apply(res$catch_shortfall[, fg, drop = FALSE], 1, function(z) all(is.na(z) | z < 0.05))
  cand <- which(feas)
  # stored draws come from the (adaptive) proposal mixture: draw truths from the prior by
  # weighting candidates with the importance ratio prior / proposal
  lc <- if (is.null(s1$log_is_correction)) rep(0, length(feas)) else s1$log_is_correction
  pw <- exp(lc[cand] - max(lc[cand])); pw <- pw / sum(pw)
  idx <- .fit_indices(proj)
  bm <- proj$benchmarks; bm <- if (!is.null(bm) && "use_in_sir" %in% names(bm)) bm[as.logical(bm$use_in_sir) %in% TRUE, ] else NULL
  rows <- list()
  for (r in seq_len(n_rep)) {
    tr <- if (length(cand) == 1) cand else sample(cand, 1, prob = pw)
    pseudo_idx <- idx
    if (!is.null(idx) && nrow(idx)) {
      for (i in seq_len(nrow(idx))) {
        grp <- strsplit(idx$group[i], ";")[[1]]
        b <- sum(s1$B[tr, as.character(idx$year[i]), grp])
        sig <- sqrt(log(1 + pmax(idx$cv[i], proj$settings$index_min_cv)^2))
        pseudo_idx$value[i] <- b * exp(stats::rnorm(1, -sig^2 / 2, sig))
      }
    }
    pseudo_bm <- bm
    if (!is.null(bm) && nrow(bm)) for (j in seq_len(nrow(bm))) {
      grp <- strsplit(bm$group[j], ";")[[1]]
      sig <- sqrt(log(1 + ifelse(is.na(bm$sir_cv[j]), 0.5, bm$sir_cv[j])^2))
      pseudo_bm$value_lb[j] <- t_to_lb(sum(s1$MMSY_ref[tr, grp])) * exp(stats::rnorm(1, 0, sig))
    }
    nll <- .total_nll(res, pseudo_idx, pseudo_bm)
    nll[tr] <- Inf
    w <- exp(-(nll - min(nll))); w <- w / sum(w)
    for (gi in groups) {
      x <- s1$MMSY_ref[, gi]; truth <- x[tr]
      q <- .wquantile(x, w, c(0.05, 0.25, 0.5, 0.75, 0.95))
      rows[[length(rows) + 1]] <- data.frame(rep = r, group = gi, truth = truth, post_median = q[3],
                                             rel_error = q[3] / truth - 1,
                                             in50 = truth >= q[2] & truth <= q[4],
                                             in90 = truth >= q[1] & truth <= q[5], ess = 1 / sum(w^2))
    }
  }
  d <- do.call(rbind, rows)
  summ <- do.call(rbind, lapply(split(d, d$group), function(z)
    data.frame(group = z$group[1], median_rel_error = stats::median(z$rel_error),
               MARE = stats::median(abs(z$rel_error)), coverage50 = mean(z$in50), coverage90 = mean(z$in90),
               median_ess = stats::median(z$ess))))
  rownames(summ) <- NULL
  list(summary = summ, replicates = d)
}

.wquantile <- function(x, w, p) {
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(p, function(pp) x[which(cw >= pp)[1]], numeric(1))
}

#' Ecosystem-overfishing indices (Link & Watson 2019)
#'
#' * Ryther index = total catch / area (t km-2 yr-1)
#' * Fogarty ratio = total catch (t) / total primary production (t C), in per mil
#' Thresholds from settings (`ryther_low`/`ryther_high`, `fogarty_low`/`fogarty_high`;
#' defaults 1-3 t km-2 yr-1 and 0.22-0.92 per mil). Computed per draw and year.
#' @param res `ttm_results`
#' @param scenario scenario
#' @export
ecosystem_overfishing_indices <- function(res, scenario = "Baseline") {
  proj <- res$proj; st <- proj$settings; h <- proj$habitats
  C <- get_metric(res, "C", scenario)
  P1 <- get_metric(res, "P1", scenario)
  fmp <- proj$groups$group_type == "fmp"
  draws_c <- if (is.null(res$row_of_draw)) res$draws else res$row_of_draw[res$draws]
  yrs <- dimnames(C)[[2]]
  ben <- h$type == "benthic"
  area_ben <- sum(h$area_km2[ben])
  # catch / PP in t C: model P1 is t WW after food-web fraction; convert back with draw parameters
  out <- list()
  for (y in yrs) {
    ct <- rowSums(matrix(C[, y, fmp, drop = FALSE], nrow = dim(C)[1]))
    ppC <- vapply(seq_len(dim(P1)[1]), function(k) {
      par <- res$pars[[draws_c[k]]]
      if (is.null(par)) return(NA_real_)
      sum(P1[k, y, ] / (par$fwf * par$c_to_ww))
    }, numeric(1))
    out[[length(out) + 1]] <- data.frame(year = as.integer(y),
      ryther_p50 = stats::median(ct / area_ben), ryther_p05 = stats::quantile(ct / area_ben, 0.05, names = FALSE),
      ryther_p95 = stats::quantile(ct / area_ben, 0.95, names = FALSE),
      fogarty_permil_p50 = stats::median(1000 * ct / ppC, na.rm = TRUE),
      fogarty_permil_p05 = stats::quantile(1000 * ct / ppC, 0.05, na.rm = TRUE, names = FALSE),
      fogarty_permil_p95 = stats::quantile(1000 * ct / ppC, 0.95, na.rm = TRUE, names = FALSE))
  }
  d <- do.call(rbind, out)
  rt <- if (is.null(st$ryther_low)) c(1, 3) else c(st$ryther_low, st$ryther_high)
  ft <- if (is.null(st$fogarty_low)) c(0.22, 0.92) else c(st$fogarty_low, st$fogarty_high)
  d$ryther_status <- cut(d$ryther_p50, c(-Inf, rt, Inf), labels = c("below", "caution", "above"))
  d$fogarty_status <- cut(d$fogarty_permil_p50, c(-Inf, ft, Inf), labels = c("below", "caution", "above"))
  attr(d, "thresholds") <- list(ryther = rt, fogarty = ft)
  d
}


#' Structural-uncertainty ensemble
#'
#' Runs the model under alternative structural assumptions and pools the
#' (SIR-conditioned) reference points with equal weight per variant. Each
#' variant is a function that takes and returns a validated `ttm_project`.
#' @param proj validated `ttm_project`
#' @param variants named list of functions (default: [default_structural_variants()])
#' @param n_draws prior draws per variant
#' @param verbose progress
#' @return list(by_variant = reference points per variant, pooled = pooled
#'   MMSY quantiles by group, results = list of `ttm_results` (Baseline only))
#' @export
run_structural_ensemble <- function(proj, variants = default_structural_variants(), n_draws = 2000,
                                    verbose = TRUE) {
  res_l <- list(); rp_l <- list(); pooled <- list()
  for (nm in names(variants)) {
    if (verbose) message("Structural variant: ", nm)
    p <- variants[[nm]](proj)
    p$settings$n_draws <- n_draws; p$settings$sir_resample <- min(n_draws, p$settings$sir_resample)
    r <- suppressWarnings(run_ttm(p, n_draws = n_draws, scenarios = "Baseline", verbose = FALSE))
    rp <- reference_points(r); rp$variant <- nm
    rp$ess <- if (is.null(r$sir)) NA else r$sir$ess
    rp_l[[nm]] <- rp
    yy <- .ref_years(r)
    MM <- apply(get_metric(r, "MMSY")[, yy, , drop = FALSE], c(1, 3), mean)
    k <- sample(seq_len(nrow(MM)), 1000, replace = TRUE)
    pooled[[nm]] <- data.frame(variant = nm, MM[k, , drop = FALSE], check.names = FALSE)
    r$pars <- NULL; r$stage1 <- NULL
    res_l[[nm]] <- r
  }
  by_var <- do.call(rbind, rp_l)
  P <- do.call(rbind, pooled)
  g <- proj$groups$group
  pq <- do.call(rbind, lapply(g, function(gi) {
    q <- stats::quantile(P[[gi]], c(0.05, 0.5, 0.95))
    data.frame(group = gi, MMSY_p05_t = q[1], MMSY_p50_t = q[2], MMSY_p95_t = q[3],
               MMSY_p50_lb = t_to_lb(q[2]),
               between_variant_cv = stats::sd(tapply(P[[gi]], P$variant, stats::median)) /
                 mean(tapply(P[[gi]], P$variant, stats::median)))
  }))
  rownames(pq) <- NULL
  list(by_variant = by_var, pooled = pq, results = res_l)
}

#' Default structural variants for the ensemble
#'
#' base (life-history x on natural mortality, Pella-Tomlinson shape from the
#' per-recruit B_MSY/B0); `gulland_PB` (the original SEDAR103-DW-20 form,
#' x = 0.5 on P/B with Schaefer dynamics); `schaefer_shape` (B_MSY = K/2);
#' x reduced by 25%; closed population
#' (no external recruitment); no nursery dependence; no herbivory-coral
#' feedback; no ghost fishing; no lionfish / Sargassum effects; transfer
#' efficiency 10% and 20%; no direct thermal effects; food-web fraction x0.67
#' and x1.5. (Earlier runs showed that fishing feedback on the food web and the
#' pelagic NPP source change trap-group MMSY by < 0.1%.)
#' @export
default_structural_variants <- function() {
  id <- function(p) p
  list(
    base = id,
    gulland_PB = function(p) {   # original SEDAR103-DW-20 form: x = 0.5 on P/B (no per-recruit, no M basis)
      f <- p$groups$group_type == "fmp"; p$groups$x_msy[f] <- 0.5; p$groups$x_msy_cv[f] <- 0.2; p$groups$m_nat[f] <- NA; p$groups$bmsy_k[f] <- 0.5
      validate_ttm_project(p) },
    schaefer_shape = function(p) { p$groups$bmsy_k <- 0.5; validate_ttm_project(p) },
    x_msy_low = function(p) { f <- p$groups$group_type == "fmp"; p$groups$x_msy[f] <- 0.75 * p$groups$x_msy[f]; validate_ttm_project(p) },
    closed_population = function(p) { p$groups$open_recruitment <- 0; validate_ttm_project(p) },
    no_nursery = function(p) { p$groups$nursery_dependence <- 0; validate_ttm_project(p) },
    no_herbivory_feedback = function(p) { p$settings$herbivory_elasticity <- 0; p },
    no_ghost_fishing = function(p) { p$settings$ghost_efficiency <- 0; p },
    no_lionfish_sargassum = function(p) {
      if (!is.null(p$env_response)) p$env_response <- p$env_response[!p$env_response$variable %in% c("lionfish", "sargassum"), ]; p },
    tte_0.10 = function(p) { p$habitats$tte <- 0.10; validate_ttm_project(p) },
    tte_0.20 = function(p) { p$habitats$tte <- 0.20; validate_ttm_project(p) },
    no_thermal = function(p) { if (!is.null(p$env_response)) p$env_response <- p$env_response[p$env_response$variable != "sst", ]; p },
    fwf_low = function(p) { p$habitats$food_web_fraction <- p$habitats$food_web_fraction * 0.67; validate_ttm_project(p) },
    fwf_high = function(p) { p$habitats$food_web_fraction <- pmin(0.9, p$habitats$food_web_fraction * 1.5); validate_ttm_project(p) })
}
