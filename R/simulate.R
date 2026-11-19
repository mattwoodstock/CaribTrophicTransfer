# ==============================================================================
# Core annual trophic-transfer simulation
# ==============================================================================
# Per year t:
#   1. Habitat state      A_h(t), Q_h(t) (coral cover dynamics or covariate)
#   2. Primary production P1_h(t) = A_h * NPP_h * idx_h(t) * dist_h(t)
#                                    * Q_h^elasticity * fwf_h * c_to_ww    [t WW/yr]
#   3. Energy pools       E_h,1 = P1_h ; E_h,k+1 = TTE_h * (E_h,k - R_h,k)
#                         (R = last-year catch removed from the TL it was produced at)
#   4. Partitioning       share_i,h,k = w_i D_i,k pref_i,h / sum_j(w_j D_j,k pref_j,h)
#                         P_i = sum_h,k TTE_h * share_i,h,k * E_h,k
#   5. Carrying capacity  K_i = P_i * env_i / M_i * Qbar_i^refuge_i * N_i^nu_i
#                         (M = natural mortality, default P/B; N = nursery-habitat index)
#   6. Reference point    MMSY_i = x_i * M_i * K_i   (Gulland 1971: x = 0.5; group x from
#                         per-recruit theory, see life_history_xmsy())
#   7. Biomass dynamics   dB/dt = (1-phi) r B (1 - B/K) + phi 2xM (K s_t - B) - C - G + I
#                         r = 4 x M; phi = open-recruitment fraction with supply index s_t;
#                         G = unaccounted ghost-trap mortality after trap losses.
#                         Exact logistic + exact linear sub-steps (`substeps`), catch removed
#                         after each sub-step, from the fishable (non-MPA) fraction only.
#   8. Feedbacks          coral recovery scales with herbivore grazing (B/K0)^gamma
#                         (Mumby et al. 2006); projected effort can respond to catch rates.
# ==============================================================================

.tl_weights <- function(tl) {
  # split a fractional trophic level into weights on integer pools 1..4
  W <- matrix(0, length(tl), 4)
  tl <- pmin(pmax(tl, 1), 4)
  lo <- floor(tl); hi <- pmin(lo + 1, 4); f <- tl - lo
  for (i in seq_along(tl)) { W[i, lo[i]] <- W[i, lo[i]] + (1 - f[i]); if (f[i] > 0) W[i, hi[i]] <- W[i, hi[i]] + f[i] }
  W
}

.base_weights <- function(proj, par) {
  g <- proj$groups
  w <- numeric(nrow(g))
  for (i in seq_len(nrow(g))) {
    w[i] <- switch(g$partition_method[i],
      uniform_pb = par$pb[i],
      biomass = par$partition_value[i] * par$pb[i],
      catch_proportion = par$partition_value[i] * par$pb[i],
      landings_proportion = par$partition_value[i] * par$pb[i],
      weight = par$partition_value[i],
      fixed_biomass = par$pb[i])
  }
  setNames(pmax(w, 1e-12), g$group)
}

# Group production (t WW/yr) given pools, weights, diet and preferences
.group_production <- function(Eb, Ep, tte_b, tte_p, w, D, HP) {
  ng <- length(w); P <- numeric(ng)
  bcols <- 1:4; pcols <- 5:8
  for (k in 1:4) {
    # benthic: ng x nb demand
    dk <- (w * D[, bcols[k]]) * HP
    cs <- colSums(dk)
    ok <- cs > 0
    if (any(ok)) P <- P + as.vector(dk[, ok, drop = FALSE] %*%
                                      (tte_b[ok] * Eb[ok, k] / cs[ok]))
    dp <- w * D[, pcols[k]]
    s <- sum(dp)
    if (s > 0 && length(Ep)) P <- P + tte_p * Ep[k] * dp / s
  }
  P
}

.logistic_step <- function(B, r, K, dt) {
  # exact solution of dB/dt = r B (1 - B/K) over dt (K held constant within step)
  e <- exp(-r * dt)
  K / (1 + (K / pmax(B, 1e-12) - 1) * e)
}

.pools <- function(P1, tte, R) {
  # P1: vector (habitats); tte: vector; R: habitats x 4 removals
  E <- matrix(0, length(P1), 4)
  E[, 1] <- P1
  for (k in 1:3) E[, k + 1] <- tte * pmax(0, E[, k] - R[, k])
  E
}

#' Simulate one parameter set under one scenario
#'
#' @param proj validated `ttm_project`
#' @param par parameter list from [draw_parameters()]
#' @param drv drivers from [build_drivers()]
#' @param q optional catchability matrix (gear x group); if NULL it is
#'   estimated from the historical period.
#' @param mp optional management procedure for closed-loop simulation: a
#'   function `mp(year, state)` called at the start of every projection year
#'   that returns an effort multiplier per gear (see [run_mse()]).
#' @return list of year x group (and year x habitat / year x gear x group) matrices.
#' @export
simulate_ttm <- function(proj, par, drv, q = NULL, mp = NULL) {
  st <- proj$settings; h <- proj$habitats; g <- proj$groups
  years <- proj$years; yc <- as.character(years); ny <- length(years)
  hist_end <- st$hist_end
  ng <- nrow(g); gn <- g$group
  bi <- which(h$type == "benthic"); pi_ <- which(h$type == "pelagic")
  bn <- h$habitat[bi]
  D <- par$diet
  HP <- (if (!is.null(par$pref)) par$pref else proj$pref_matrix)[, bn, drop = FALSE]
  tl <- group_trophic_level(D); names(tl) <- gn
  WTL <- .tl_weights(tl)
  bfrac <- rowSums(D[, 1:4, drop = FALSE])
  pb <- par$pb; x <- par$x_msy
  mnat <- if (is.null(par$m_nat)) pb else par$m_nat
  r <- 4 * x * mnat
  phi <- if (is.null(par$open_rec)) rep(0, ng) else par$open_rec
  mu_open <- 2 * x * mnat
  gears <- proj$gear_names; nge <- length(gears)
  S <- max(1, st$substeps)
  env <- env_multipliers(proj, drv, par)
  if (isTRUE(par$stochastic) && !is.na(par$process_dev_seed)) {
    old <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
    set.seed(par$process_dev_seed)
    eps <- matrix(stats::rnorm(ny * ng), ny, ng) * rep(g$process_sd, each = ny)
    if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
  } else eps <- matrix(0, ny, ng)

  # ---- historical catch (t) by year x gear x group -------------------------
  Chist <- array(0, c(ny, max(1, nge), ng), dimnames = list(yc, if (nge) gears else "none", gn))
  L <- proj$landings
  if (!is.null(L) && nrow(L)) {
    L <- L[L$year %in% years & L$year <= hist_end, , drop = FALSE]
    for (j in seq_len(nrow(L)))
      Chist[as.character(L$year[j]), L$gear[j], L$group[j]] <-
        Chist[as.character(L$year[j]), L$gear[j], L$group[j]] + lb_to_t(L$landings_lb[j]) * par$catch_expansion *
        (if (is.null(par$landings_mult)) 1 else par$landings_mult[j])
  }
  # ---- effort (relative) ---------------------------------------------------
  Eff <- matrix(1, ny, max(1, nge), dimnames = list(yc, if (nge) gears else "none"))
  if (!is.null(proj$effort) && nge) {
    ef <- proj$effort
    if (!is.null(par$effort_mult)) ef$effort <- ef$effort * par$effort_mult
    for (ge in gears) {
      e <- ef[ef$gear == ge, ]
      if (nrow(e)) Eff[, ge] <- stats::approx(e$year, e$effort, xout = years, rule = 2)$y
    }
  }
  hy <- years[years <= hist_end]
  qy <- utils::tail(hy, st$q_years)
  Eref <- colMeans(Eff[as.character(qy), , drop = FALSE])
  if (nge) for (t in which(years > hist_end)) Eff[t, ] <- Eref * drv$effort_mult[t, gears]
  eff_resp <- if (!is.null(drv$effort_response)) drv$effort_response else
    if (is.null(st$effort_response)) 0 else st$effort_response
  dyn <- setNames(rep(1, max(1, nge)), colnames(Eff))
  # no-take (MPA) fraction by habitat and the fishable fraction of each group
  closed <- if (!is.null(drv$closed)) drv$closed else
    matrix(rep(h$closed_fraction, each = ny), ny, nrow(h), dimnames = list(yc, h$habitat))
  # nursery habitats
  nurs <- lapply(seq_len(ng), function(i) {
    v <- g$nursery_habitat[i]
    if (is.na(v) || v == "" || g$nursery_dependence[i] <= 0) integer(0) else match(intersect(strsplit(v, ";")[[1]], h$habitat), h$habitat)
  })
  nurs_ref <- rep(NA_real_, ng)
  # recruitment supply index for open populations
  supply <- matrix(1, ny, ng, dimnames = list(yc, gn))
  for (i in which(phi > 0 & !is.na(g$recruit_covariate))) {
    v <- g$recruit_covariate[i]
    if (v %in% colnames(drv$cov)) { z <- drv$cov[, v]; supply[, i] <- pmax(0, z / mean(z[years <= hist_end])) }
  }
  # herbivory - coral feedback
  herb <- if (is.null(st$herbivore_groups) || is.na(st$herbivore_groups)) integer(0) else
    match(intersect(strsplit(st$herbivore_groups, ";")[[1]], gn), gn)
  gam_h <- if (is.null(par$herbivory_elasticity)) 0 else par$herbivory_elasticity
  graze <- rep(NA_real_, ny); graze_ref <- NA_real_
  ghost_eff <- if (is.null(par$ghost_eff)) 0 else par$ghost_eff
  Fgear_prev <- NULL

  # ---- containers ----------------------------------------------------------
  mat <- function() matrix(NA_real_, ny, ng, dimnames = list(yc, gn))
  out <- list(B = mat(), K = mat(), K0 = mat(), P = mat(), P0 = mat(), MMSY = mat(),
              MMSY0 = mat(), C = mat(), F = mat())
  out$Cg <- array(0, dim(Chist), dimnames = dimnames(Chist))
  out$P1 <- matrix(NA_real_, ny, nrow(h), dimnames = list(yc, h$habitat))
  out$Q <- out$P1
  out$TL <- tl; out$bfrac <- bfrac
  out$fopen <- mat(); out$ghost <- mat(); out$nursery <- mat()

  # ---- habitat quality state ----------------------------------------------
  Qstate <- h$init_quality
  cov <- drv$cov
  dhw_var <- if (!is.null(st$dhw_variable)) st$dhw_variable else "dhw"
  obs_years <- function(v) {
    ec <- proj$env_covariates
    if (is.null(ec) || is.na(v)) return(numeric(0))
    ec$year[ec$variable == v]
  }

  # ---- weights (with fixed-biomass calibration at first year, no fishing) --
  w <- .base_weights(proj, par)
  fixed <- which(g$partition_method == "fixed_biomass")
  Rzero_b <- matrix(0, length(bi), 4); Rzero_p <- matrix(0, length(pi_), 4)

  B <- NULL; Cprev <- rep(0, ng); fopen_ref <- NULL; cpue_ref <- NULL
  hist_target <- hist_taken <- max_short <- setNames(numeric(ng), gn)
  for (t in seq_len(ny)) {
    yr <- years[t]
    # 1. habitat state
    for (j in seq_len(nrow(h))) {
      qm <- h$quality_model[j]; v <- h$quality_covariate[j]
      if (qm == "coral") {
        if (yr <= hist_end && !is.na(v) && length(obs_years(v)) &&
            yr >= min(obs_years(v))) {
          Qstate[j] <- cov[t, v]
        } else if (t > 1) {
          cc <- Qstate[j]
          dhw <- if (dhw_var %in% colnames(cov)) cov[t, dhw_var] else 0
          bleach <- min(0.95, par$bleach_mort_per_dhw * max(0, dhw - st$bleach_dhw_threshold))
          cg <- if (is.null(par$coral_growth)) st$coral_growth else par$coral_growth
          if (gam_h > 0 && yr > hist_end && is.finite(graze_ref) && t > 1 && is.finite(graze[t - 1]))
            cg <- cg * min(2, (graze[t - 1] / graze_ref)^gam_h)
          cc <- cc + cg * cc * (1 - cc / st$coral_max_cover)
          cc <- cc * (1 - bleach) * (1 - min(0.95, drv$coral_disease[t, j])) *
                (1 - drv$coral_loss[t, j])
          Qstate[j] <- max(1e-4, cc)
        }
        out$Q[t, j] <- Qstate[j] / h$quality_ref[j] * drv$quality_mult[t, j]
      } else if (qm == "covariate" && !is.na(v) && v %in% colnames(cov)) {
        out$Q[t, j] <- cov[t, v] / h$quality_ref[j] * drv$quality_mult[t, j]
      } else out$Q[t, j] <- drv$quality_mult[t, j]
    }
    Q <- pmax(out$Q[t, ], 1e-4)
    area <- par$area * drv$area_mult[t, ]
    # 2. primary production
    P1 <- area * par$npp * drv$npp_index[t, ] * drv$p1_mult[t, ] *
          Q^h$p1_quality_elasticity * par$fwf * par$c_to_ww
    out$P1[t, ] <- P1
    tte <- pmin(0.5, par$tte * drv$tte_mult[t, ])
    # 3. removals (previous-year catch) by habitat and TL
    use <- HP * rep(area[bi], each = ng); use <- use / pmax(rowSums(use), 1e-12)
    if (isTRUE(st$fishing_feedback)) {
      Rb <- t(use * (Cprev * bfrac)) %*% WTL
      Rp <- matrix(colSums((Cprev * (1 - bfrac)) * WTL), nrow = length(pi_), ncol = 4, byrow = TRUE)
    } else { Rb <- Rzero_b; Rp <- Rzero_p }
    Eb <- .pools(P1[bi], tte[bi], Rb); Ep <- if (length(pi_)) .pools(P1[pi_], tte[pi_], Rp)[1, ] else numeric(0)
    Eb0 <- .pools(P1[bi], tte[bi], Rzero_b); Ep0 <- if (length(pi_)) .pools(P1[pi_], tte[pi_], Rzero_p)[1, ] else numeric(0)
    tte_p <- if (length(pi_)) tte[pi_] else 0
    # 4. habitat refuge + environment multipliers
    Qbar <- ifelse(bfrac > 0, as.vector(use %*% Q[bi]), 1)
    hab_k <- Qbar^g$refuge_dependence
    envP <- env$production[t, ] * drv$prod_mult[t, ]
    #    calibrate fixed-biomass weights once so that K0 (first year) = B0_fixed
    if (t == 1 && length(fixed)) {
      kmult <- envP / mnat * hab_k * env$K[t, ]
      for (it in 1:100) {
        Pc <- .group_production(Eb0, Ep0, tte[bi], tte_p, w, D, HP)
        ratio <- par$B0_fixed[fixed] / pmax(Pc[fixed] * kmult[fixed], 1e-12)
        w[fixed] <- w[fixed] * ratio
        if (max(abs(ratio - 1)) < 1e-6) break
      }
      out$fixed_calibration_ratio <- ratio
    }
    P  <- .group_production(Eb, Ep, tte[bi], tte_p, w, D, HP)
    P0 <- .group_production(Eb0, Ep0, tte[bi], tte_p, w, D, HP)
    # 5. capacity
    # nursery-habitat index (area x quality of nursery habitats, relative to the first year)
    nmult <- rep(1, ng)
    for (i in which(lengths(nurs) > 0)) {
      ni <- sum(area[nurs[[i]]] * Q[nurs[[i]]])
      if (is.na(nurs_ref[i])) nurs_ref[i] <- ni
      nmult[i] <- (ni / max(nurs_ref[i], 1e-12))^g$nursery_dependence[i]
    }
    out$nursery[t, ] <- nmult
    K  <- pmax(P * envP / mnat * hab_k * env$K[t, ] * nmult, 1e-9)
    K0 <- pmax(P0 * envP / mnat * hab_k * env$K[t, ] * nmult, 1e-9)
    out$P[t, ] <- P * envP; out$P0[t, ] <- P0 * envP
    out$K[t, ] <- K; out$K0[t, ] <- K0
    out$MMSY[t, ] <- x * mnat * K; out$MMSY0[t, ] <- x * mnat * K0
    if (t == 1) B <- par$init_depletion * K0
    # 6. catch + biomass dynamics
    imm <- (if (is.null(par$immigration)) g$immigration_t else par$immigration) * drv$imm_mult[t, ]
    Bstart <- B
    fopen <- 1 - ifelse(bfrac > 0, bfrac * as.vector(use %*% closed[t, bi]), 0)
    out$fopen[t, ] <- fopen
    sup <- supply[t, ] * nmult
    # ghost fishing by traps lost in disturbances (unaccounted mortality, not landings)
    Fghost <- rep(0, ng)
    if (ghost_eff > 0 && !is.null(drv$ghost) && !is.null(Fgear_prev)) {
      for (ge in intersect(colnames(drv$ghost), rownames(Fgear_prev))) {
        L <- drv$ghost[t, ge]
        if (L > 0) Fghost <- Fghost + ghost_eff * min(3, L / max(0.05, 1 - L)) * Fgear_prev[ge, ]
      }
    }
    step_bio <- function(B, dt) {
      B <- .logistic_step(B, (1 - phi) * r, K, dt)
      op <- phi > 0
      if (any(op)) B[op] <- K[op] * sup[op] + (B[op] - K[op] * sup[op]) * exp(-phi[op] * mu_open[op] * dt)
      B * exp(-Fghost * dt)
    }
    if (yr <= hist_end) {
      Cg_target <- Chist[t, , , drop = FALSE][1, , ]
      if (is.null(dim(Cg_target))) Cg_target <- matrix(Cg_target, nrow = dim(Chist)[2], dimnames = list(dimnames(Chist)[[2]], gn))
      Ctot_target <- colSums(Cg_target)
      Ctake <- numeric(ng)
      for (s in seq_len(S)) {
        B <- step_bio(B, 1 / S)
        cs <- pmin(Ctot_target / S, 0.9 * fopen * B)
        B <- pmax(B - cs + imm / S, 1e-6 * K0)
        Ctake <- Ctake + cs
      }
      hist_target <- hist_target + Ctot_target; hist_taken <- hist_taken + Ctake
      ann_short <- ifelse(Ctot_target > 0, 1 - Ctake / Ctot_target, 0)
      max_short <- pmax(max_short, ann_short)
      scale <- ifelse(Ctot_target > 0, Ctake / Ctot_target, 0)
      out$Cg[t, , ] <- sweep(Cg_target, 2, scale, "*")
      out$C[t, ] <- Ctake
      out$F[t, ] <- -log(pmax(1e-9, 1 - pmin(0.99, Ctake / pmax((Bstart + B) / 2, 1e-9))))
    } else {
      if (is.null(q)) q <- .estimate_q(out, Eff, years, qy, gears, gn)
      if (is.null(fopen_ref)) fopen_ref <- colMeans(out$fopen[as.character(qy), , drop = FALSE])
      # effort dynamics: effort responds to last year's catch rate relative to the reference years
      if (eff_resp != 0 && nge && t > 1) {
        cpue_prev <- rowSums(matrix(out$Cg[t - 1, , ], nrow = nge)) / pmax(Eff[t - 1, ], 1e-12)
        dyn <- pmin(3, pmax(0.2, dyn * (pmax(cpue_prev, 1e-12) / pmax(cpue_ref, 1e-12))^eff_resp))
        Eff[t, ] <- Eff[t, ] * dyn
      }
      if (is.function(mp)) {
        em <- mp(yr, list(t = t, years = years, out = out, B = B, K = K, MMSY = out$MMSY[t, ], Eff = Eff,
                          Eref = Eref, gears = gears, groups = g, q = q, hist_end = hist_end))
        if (!is.null(em)) Eff[t, ] <- Eff[t, ] * em[colnames(Eff)]
      }
      Fg <- q * Eff[t, ]  # gear x group
      Fg <- sweep(Fg, 2, fopen / pmax(fopen_ref, 1e-6), "*")   # change in no-take area since the reference years
      Fg[, !g$harvest_allowed] <- 0
      Ft <- colSums(Fg)
      Ctake <- numeric(ng)
      for (s in seq_len(S)) {
        B <- step_bio(B, 1 / S)
        cs <- B * (1 - exp(-Ft / S))
        B <- pmax(B - cs + imm / S, 1e-6 * K0)
        Ctake <- Ctake + cs
      }
      share <- sweep(Fg, 2, pmax(Ft, 1e-12), "/")
      out$Cg[t, , ] <- sweep(share, 2, Ctake, "*")
      out$C[t, ] <- Ctake; out$F[t, ] <- Ft
    }
    B <- B * exp(eps[t, ] - (g$process_sd^2) / 2)
    out$B[t, ] <- Bstart
    out$ghost[t, ] <- Fghost * (Bstart + B) / 2
    Cprev <- out$C[t, ]
    Bmid <- pmax((Bstart + B) / 2, 1e-9)
    if (nge) Fgear_prev <- sweep(matrix(out$Cg[t, , ], nrow = nge, dimnames = list(dimnames(out$Cg)[[2]], gn)), 2, Bmid, "/")
    if (length(herb)) {
      graze[t] <- sum(Bmid[herb]) / sum(out$K0[t, herb])
      if (yr == hist_end) graze_ref <- mean(graze[years %in% qy], na.rm = TRUE)
    }
    if (yr == hist_end && nge) cpue_ref <- colMeans(matrix(sapply(as.character(qy), function(yy)
      rowSums(matrix(out$Cg[yy, , ], nrow = nge)) / pmax(Eff[yy, ], 1e-12)), nrow = nge) |> t())
  }
  # SRA-style check: largest annual share of landings the simulated stock could NOT support
  out$catch_shortfall <- ifelse(hist_target > 0, max_short, NA_real_)
  out$catch_shortfall_total <- ifelse(hist_target > 0, 1 - hist_taken / hist_target, NA_real_)
  out$q <- if (is.null(q)) .estimate_q(out, Eff, years, qy, gears, gn) else q
  out$weights <- w
  out$Eff <- Eff
  out$graze <- graze
  out
}

.estimate_q <- function(out, Eff, years, qy, gears, gn) {
  ix <- as.character(qy)
  nge <- max(1, length(gears))
  q <- matrix(0, nge, length(gn), dimnames = list(if (length(gears)) gears else "none", gn))
  for (yy in ix) {
    Cg <- out$Cg[yy, , , drop = FALSE][1, , ]
    if (is.null(dim(Cg))) Cg <- matrix(Cg, nrow = nge, dimnames = dimnames(q))
    Ftot <- out$F[yy, ]
    share <- sweep(Cg, 2, pmax(colSums(Cg), 1e-12), "/")
    q <- q + sweep(share, 2, Ftot, "*") / pmax(Eff[yy, ], 1e-12)
  }
  q / length(ix)
}
