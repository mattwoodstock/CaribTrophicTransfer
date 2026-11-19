# ==============================================================================
# Decision-support tools: independent catch-only comparator, value of
# information and closed-loop management strategy evaluation (MSE)
# ==============================================================================

# ---- 1. Catch-only MSY comparator (CMSY-style; Martell & Froese 2013) ---------

#' Catch-only MSY estimates as an independent comparator
#'
#' A Schaefer surplus-production stock-reduction analysis that uses only the
#' landings series and a resilience prior, so it shares no energy-budget
#' assumptions with the TTM. Draws (r, K) from log-uniform priors, projects
#' B(t+1) = B + r B (1 - B/K) - C, and keeps viable pairs whose stock never
#' collapses and whose final depletion lies in `end_depletion`. MSY = rK/4.
#' Resilience follows Froese et al. (2017): r ranges from natural mortality
#' (M > 0.6: high 0.6-1.5; 0.3-0.6: medium 0.2-0.8; < 0.3: low 0.05-0.5).
#' @param proj a `ttm_project`
#' @param groups groups to evaluate (default: [focus_groups()])
#' @param n number of (r, K) draws per group
#' @param start_depletion,end_depletion prior ranges for B/K in the first and last year
#' @return data frame group, MSY_p05/p50/p95 (lb), r_p50, n_viable, resilience
#' @export
catch_msy <- function(proj, groups = focus_groups(proj), n = 20000,
                      start_depletion = c(0.5, 0.9), end_depletion = c(0.2, 0.8)) {
  L <- proj$landings; st <- proj$settings; g <- proj$groups
  yrs <- proj$hist_years
  do.call(rbind, lapply(groups, function(gi) {
    x <- L[L$group == gi & L$year %in% yrs, ]
    if (!nrow(x)) return(NULL)
    C <- tapply(lb_to_t(x$landings_lb), factor(x$year, levels = yrs), sum)
    C[is.na(C)] <- 0; C <- as.numeric(C) * st$catch_expansion
    M <- g$m_nat[g$group == gi]; if (is.na(M)) M <- g$pb[g$group == gi]
    rr <- if (M > 0.6) c(0.6, 1.5) else if (M >= 0.3) c(0.2, 0.8) else c(0.05, 0.5)
    res <- if (M > 0.6) "high" else if (M >= 0.3) "medium" else "low"
    kr <- c(max(C) / rr[2], 25 * max(C) / rr[1])
    r <- exp(stats::runif(n, log(rr[1]), log(rr[2]))); K <- exp(stats::runif(n, log(kr[1]), log(kr[2])))
    B <- K * stats::runif(n, start_depletion[1], start_depletion[2])
    ok <- rep(TRUE, n)
    for (t in seq_along(C)) {
      B <- B + r * B * (1 - B / K) - C[t]
      ok <- ok & B > 0.01 * K
      B <- pmax(B, 1e-6)
    }
    ok <- ok & B / K >= end_depletion[1] & B / K <= end_depletion[2]
    if (sum(ok) < 10) return(data.frame(group = gi, MSY_p05_lb = NA, MSY_p50_lb = NA, MSY_p95_lb = NA,
                                       r_p50 = NA, n_viable = sum(ok), resilience = res))
    msy <- r[ok] * K[ok] / 4
    q <- stats::quantile(msy, c(0.05, 0.5, 0.95), names = FALSE)
    data.frame(group = gi, MSY_p05_lb = t_to_lb(q[1]), MSY_p50_lb = t_to_lb(q[2]), MSY_p95_lb = t_to_lb(q[3]),
               r_p50 = stats::median(r[ok]), n_viable = sum(ok), resilience = res)
  }))
}

#' Compare TTM MMSY with the catch-only comparator and assessment benchmarks
#' @param res `ttm_results`
#' @param cm output of [catch_msy()] (computed if NULL)
#' @export
method_comparison <- function(res, cm = NULL) {
  if (is.null(cm)) cm <- catch_msy(res$proj)
  rp <- reference_points(res)
  out <- merge(cm, rp[, c("group", "MMSY_p05_lb", "MMSY_p50_lb", "MMSY_p95_lb")], by = "group",
               suffixes = c("_catch_only", "_TTM"))
  names(out) <- sub("^MSY_", "CMSY_", names(out))
  bm <- res$proj$benchmarks
  if (!is.null(bm)) {
    ofl <- bm[bm$metric == "OFL", c("group", "value_lb")]; names(ofl)[2] <- "OFL_lb"
    out <- merge(out, ofl, by = "group", all.x = TRUE)
  }
  out$ratio_TTM_to_catch_only <- out$MMSY_p50_lb / out$CMSY_p50_lb
  out
}

# ---- 2. Value of information --------------------------------------------------

.data_stream <- function(input) {
  pre <- sub(":.*", "", input)
  switch(pre,
    partition = "Biomass surveys (NCRMP / assessment B0)",
    npp = "Primary-production measurements", fwf = "Food-web / detritus flows",
    tte = "Trophic transfer efficiency", area = "Habitat mapping",
    pb = "Production/biomass (Ecopath)", M = "Natural mortality (life history / tagging)",
    x_msy = "Life history (growth, maturity, selectivity)", init_dep = "Historical CPUE (1980s depletion)",
    open_rec = "Larval connectivity / settlement", env = "Lionfish / Sargassum effects",
    if (input %in% c("catch_expansion", "landings_obs_err")) "Catch monitoring (landings, expansion)" else
    if (input %in% c("dist_loss_mult", "dist_intensity_err", "ghost_efficiency")) "Hurricane and trap-loss impacts" else
    if (input %in% c("bleach_mort", "coral_growth", "herbivory_elasticity")) "Coral and herbivory dynamics" else
    "Other")
}

#' Value of information: which inputs (and data streams) would most reduce
#' uncertainty in MMSY
#'
#' First-order variance-based sensitivity (Sobol' main effect) of log MMSY to
#' each sampled input, estimated from the Stage-1 Monte Carlo draws by
#' quantile binning: S_i = Var(E[Y | X_i]) / Var(Y). S_i is the expected
#' fraction of output variance removed if X_i were known exactly - the
#' expected value of perfect (partial) information for a variance loss
#' (Oakley & O'Hagan 2004; Saltelli et al. 2008). Inputs are mapped to the data
#' stream that would inform them, and streams are ranked by the largest S_i
#' among their inputs.
#' @param res `ttm_results`
#' @param groups output groups (default: focus groups)
#' @param bins number of quantile bins
#' @param posterior use SIR weights (posterior VOI) instead of the prior draws
#' @return list(inputs = per input x output, streams = per data stream)
#' @export
value_of_information <- function(res, groups = intersect(focus_groups(res$proj), colnames(res$stage1$MMSY_ref)),
                                 bins = 20, posterior = FALSE) {
  X <- res$stage1$params
  if (is.null(X)) stop("No stored parameters (res$stage1$params)")
  w <- if (posterior && !is.null(res$sir)) res$sir$weights else rep(1 / nrow(X), nrow(X))
  keep <- apply(X, 2, function(z) all(is.finite(z)) && stats::sd(z) > 0)
  X <- X[, keep, drop = FALSE]
  Ys <- c(list(total_focus_MMSY = rowSums(res$stage1$MMSY_ref[, groups, drop = FALSE])),
          lapply(setNames(groups, paste0("MMSY_", groups)), function(gi) res$stage1$MMSY_ref[, gi]))
  rows <- list()
  for (yn in names(Ys)) {
    y <- log(pmax(Ys[[yn]], 1e-12)); ok <- is.finite(y)
    mu <- sum(w[ok] * y[ok]) / sum(w[ok]); vy <- sum(w[ok] * (y[ok] - mu)^2) / sum(w[ok])
    for (j in seq_len(ncol(X))) {
      xb <- cut(rank(X[ok, j], ties.method = "first"), bins, labels = FALSE)
      ww <- w[ok]; yy <- y[ok]
      mb <- tapply(ww * yy, xb, sum) / tapply(ww, xb, sum); pb_ <- tapply(ww, xb, sum) / sum(ww)
      S <- sum(pb_ * (mb - mu)^2) / vy
      rows[[length(rows) + 1]] <- data.frame(output = yn, input = colnames(X)[j], S1 = S,
                                             cv_reduction = 1 - sqrt(max(0, 1 - S)))
    }
  }
  inp <- do.call(rbind, rows)
  # bias correction for binning noise: subtract the expected S of an irrelevant input (~ (bins-1)/n_eff)
  n_eff <- 1 / sum(w^2)
  inp$S1 <- pmax(0, inp$S1 - (bins - 1) / n_eff)
  inp$cv_reduction <- 1 - sqrt(pmax(0, 1 - inp$S1))
  inp$data_stream <- vapply(inp$input, .data_stream, character(1))
  str <- do.call(rbind, lapply(split(inp, list(inp$output, inp$data_stream), drop = TRUE), function(z)
    data.frame(output = z$output[1], data_stream = z$data_stream[1], S1_max = max(z$S1), S1_sum = sum(z$S1),
               top_input = z$input[which.max(z$S1)])))
  str <- str[order(str$output, -str$S1_sum), ]
  inp <- inp[order(inp$output, -inp$S1), ]
  rownames(inp) <- rownames(str) <- NULL
  list(inputs = inp, streams = str)
}

# ---- 3. Closed-loop management strategy evaluation ---------------------------

#' Management procedures for [run_mse()]
#'
#' Each constructor returns a factory `function(seed)` that creates a fresh,
#' stateful management procedure `mp(year, state)` for one operating-model
#' draw. The procedure returns an effort multiplier for every gear.
#' * `mp_status_quo()`: effort stays at the reference level.
#' * `mp_fixed(mult, gears)`: constant multiplier on the listed gears.
#' * `mp_spf_ceiling()`: every `interval` years an assessment estimates each
#'   focus group's MMSY with lognormal error (`obs_cv`, `bias`); each trap gear's
#'   effort is set so that no focus group's catch by that gear exceeds its share
#'   of the P* target (the SPF effort-ceiling rule), with changes capped at
#'   +/- `max_change` per decision.
#' * `mp_cpue_hcr()`: empirical rule; effort follows the gear's observed catch
#'   rate relative to the reference years, E <- E (1 + lambda (I/I_ref - 1)).
#' @param interval years between assessments
#' @param obs_cv,bias assessment error (CV, multiplicative bias)
#' @param pstar percentile used for the target
#' @param max_change largest proportional change in effort per decision
#' @param gears gears managed (default: all gears)
#' @param mult constant multiplier
#' @param lambda response of effort to the catch-rate signal
#' @name management_procedures
NULL

#' @rdname management_procedures
#' @export
mp_status_quo <- function() function(seed) function(year, state) NULL

#' @rdname management_procedures
#' @export
mp_fixed <- function(mult, gears = NULL) function(seed) function(year, state) {
  m <- setNames(rep(1, length(state$gears)), state$gears)
  m[if (is.null(gears)) state$gears else intersect(gears, state$gears)] <- mult
  m
}

#' @rdname management_procedures
#' @export
mp_spf_ceiling <- function(interval = 3, obs_cv = 0.3, bias = 1, pstar = 0.4, max_change = 0.25,
                           gears = NULL) {
  function(seed) {
    env <- new.env(); env$mult <- NULL; env$last <- -Inf; env$seed <- seed
    function(year, state) {
      gs <- state$gears; if (is.null(env$mult)) env$mult <- setNames(rep(1, length(gs)), gs)
      if (year - env$last >= interval) {
        env$last <- year
        set.seed(env$seed + year)
        foc <- state$focus
        t <- state$t
        Cg <- matrix(state$out$Cg[t - 1, , ], nrow = length(gs), dimnames = list(gs, state$groups$group))
        sig <- sqrt(log(1 + obs_cv^2))
        mmsy_hat <- state$out$MMSY[t - 1, foc] * bias * exp(stats::rnorm(length(foc), -sig^2 / 2, sig))
        target <- mmsy_hat * exp(stats::qnorm(pstar) * sig)
        share <- sweep(Cg[, foc, drop = FALSE], 2, pmax(colSums(Cg[, foc, drop = FALSE]), 1e-12), "/")
        man <- if (is.null(gears)) gs else intersect(gears, gs)
        for (ge in man) {
          cc <- Cg[ge, foc]; use <- cc > 1e-6 & share[ge, ] > 0.05
          if (!any(use)) next
          m_new <- min((target[use] * share[ge, use]) / cc[use])   # proportional effort change to the ceiling
          m_new <- min(1 + max_change, max(1 - max_change, m_new))
          env$mult[ge] <- env$mult[ge] * m_new
        }
      }
      env$mult
    }
  }
}

#' @rdname management_procedures
#' @export
mp_cpue_hcr <- function(lambda = 0.5, obs_cv = 0.2, gears = NULL) {
  function(seed) {
    env <- new.env(); env$mult <- NULL; env$ref <- NULL; env$seed <- seed
    function(year, state) {
      gs <- state$gears; t <- state$t
      if (is.null(env$mult)) env$mult <- setNames(rep(1, length(gs)), gs)
      cpue <- function(tt) rowSums(matrix(state$out$Cg[tt, , ], nrow = length(gs))) / pmax(state$Eff[tt, ], 1e-12)
      if (is.null(env$ref)) {
        hy <- which(state$years <= state$hist_end); hy <- utils::tail(hy, 3)
        env$ref <- rowMeans(sapply(hy, cpue))
      }
      set.seed(env$seed + year)
      I <- cpue(t - 1) * exp(stats::rnorm(length(gs), 0, obs_cv))
      man <- if (is.null(gears)) gs else intersect(gears, gs)
      env$mult[man] <- pmin(2, pmax(0.3, env$mult[man] * (1 + lambda * (I[match(man, gs)] / env$ref[match(man, gs)] - 1))))
      env$mult
    }
  }
}

#' Default management procedures evaluated for the SPF hand-off
#' @export
default_mps <- function() list(
  Status_quo = mp_status_quo(),
  Trap_minus25 = mp_fixed(0.75, c("trap_fish", "trap_lobster")),
  SPF_ceiling = mp_spf_ceiling(),
  SPF_ceiling_biased = mp_spf_ceiling(bias = 1.5),
  CPUE_HCR = mp_cpue_hcr())

#' Closed-loop management strategy evaluation
#'
#' Uses the SIR-conditioned draws as operating models (one per resampled
#' draw, parameters regenerated from the stored draws) and simulates each
#' management procedure year by year through the projection period. The
#' procedure sees only noisy assessment outputs or catch rates; the operating
#' model keeps the true dynamics, including open recruitment, nursery,
#' herbivory and ghost-fishing processes.
#' @param res `ttm_results` with `res$pars` available (run before dropping it)
#' @param mps named list of management procedures (see [management_procedures])
#' @param n number of operating-model draws (sampled from the SIR resample)
#' @param scenario operating-model scenario
#' @param eval_years years used for performance statistics (default: final 10)
#' @return list(summary = performance by procedure, by_group = per group)
#' @export
run_mse <- function(res, mps = default_mps(), n = 100, scenario = "Baseline", eval_years = NULL) {
  if (is.null(res$pars)) stop("run_mse needs res$pars (call before they are dropped)")
  proj <- res$proj; st <- proj$settings; g <- proj$groups
  if (is.null(eval_years)) eval_years <- seq(st$proj_end - 9, st$proj_end)
  ey <- as.character(eval_years)
  foc <- intersect(focus_groups(proj), g$group[g$harvest_allowed])
  set.seed(st$seed + 7)
  rows <- sample(res$row_of_draw[res$draws], n, replace = TRUE)
  out <- list(); grp <- list()
  for (mn in names(mps)) {
    perf <- matrix(NA_real_, n, 5, dimnames = list(NULL, c("min_BBmsy", "P_any_overfished", "catch_t", "AAV", "effort_mult")))
    gb <- matrix(NA_real_, n, length(foc), dimnames = list(NULL, foc))
    for (i in seq_len(n)) {
      par <- res$pars[[rows[i]]]
      drv <- build_drivers(proj, scenario, par)
      mp_i <- mps[[mn]](seed = st$seed * 1000 + i)
      mpw <- function(year, state) { state$focus <- foc; mp_i(year, state) }
      o <- simulate_ttm(proj, par, drv, mp = mpw)
      bb <- 2 * o$B[ey, foc, drop = FALSE] / o$K[ey, foc, drop = FALSE]
      ct <- rowSums(o$C[ey, foc, drop = FALSE])
      perf[i, ] <- c(min(bb), mean(apply(bb < st$bbmsy_overfished, 1, any)), mean(ct),
                     mean(abs(diff(ct))) / max(mean(ct), 1e-9),
                     mean(o$Eff[ey, ] / matrix(o$Eff[as.character(st$hist_end), ], length(ey), ncol(o$Eff), byrow = TRUE)))
      gb[i, ] <- apply(bb, 2, min)
    }
    out[[mn]] <- data.frame(procedure = mn, P_any_focus_overfished = mean(perf[, "P_any_overfished"]),
                            min_BBmsy_p05 = stats::quantile(perf[, "min_BBmsy"], 0.05, names = FALSE),
                            min_BBmsy_p50 = stats::median(perf[, "min_BBmsy"]),
                            focus_catch_lb_p50 = t_to_lb(stats::median(perf[, "catch_t"])),
                            catch_AAV_p50 = stats::median(perf[, "AAV"]),
                            effort_mult_p50 = stats::median(perf[, "effort_mult"]))
    grp[[mn]] <- data.frame(procedure = mn, group = foc,
                            P_overfished = colMeans(gb < st$bbmsy_overfished),
                            min_BBmsy_p50 = apply(gb, 2, stats::median))
  }
  list(summary = do.call(rbind, out), by_group = do.call(rbind, grp))
}
