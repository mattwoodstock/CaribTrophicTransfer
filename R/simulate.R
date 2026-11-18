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
#   5. Carrying capacity  K_i = P_i * env_i / (P/B)_i * Qbar_i^refuge_i
#   6. Reference point    MMSY_i = x_i * (P/B)_i * K_i     (x = 0.5 -> Schaefer/Gulland)
#   7. Biomass dynamics   dB/dt = r_i B (1 - B/K_i) - C_i + I_i,  r_i = 4 x_i (P/B)_i
#                         integrated with `substeps` exact logistic sub-steps (stable for
#                         any r) with catch removed after each sub-step + lognormal process error
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
#' @return list of year x group (and year x habitat / year x gear x group) matrices.
#' @export
simulate_ttm <- function(proj, par, drv, q = NULL) {
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
  pb <- par$pb; x <- par$x_msy; r <- 4 * x * pb
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

  # ---- containers ----------------------------------------------------------
  mat <- function() matrix(NA_real_, ny, ng, dimnames = list(yc, gn))
  out <- list(B = mat(), K = mat(), K0 = mat(), P = mat(), P0 = mat(), MMSY = mat(),
              MMSY0 = mat(), C = mat(), F = mat())
  out$Cg <- array(0, dim(Chist), dimnames = dimnames(Chist))
  out$P1 <- matrix(NA_real_, ny, nrow(h), dimnames = list(yc, h$habitat))
  out$Q <- out$P1
  out$TL <- tl; out$bfrac <- bfrac; out$Eff <- Eff

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

  B <- NULL; Cprev <- rep(0, ng)
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
      kmult <- envP / pb * hab_k * env$K[t, ]
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
    K  <- pmax(P * envP / pb * hab_k * env$K[t, ], 1e-9)
    K0 <- pmax(P0 * envP / pb * hab_k * env$K[t, ], 1e-9)
    out$P[t, ] <- P * envP; out$P0[t, ] <- P0 * envP
    out$K[t, ] <- K; out$K0[t, ] <- K0
    out$MMSY[t, ] <- x * pb * K; out$MMSY0[t, ] <- x * pb * K0
    if (t == 1) B <- par$init_depletion * K0
    # 6. catch + biomass dynamics
    imm <- (if (is.null(par$immigration)) g$immigration_t else par$immigration) * drv$imm_mult[t, ]
    Bstart <- B
    if (yr <= hist_end) {
      Cg_target <- Chist[t, , , drop = FALSE][1, , ]
      if (is.null(dim(Cg_target))) Cg_target <- matrix(Cg_target, nrow = dim(Chist)[2], dimnames = list(dimnames(Chist)[[2]], gn))
      Ctot_target <- colSums(Cg_target)
      Ctake <- numeric(ng)
      for (s in seq_len(S)) {
        B <- .logistic_step(B, r, K, 1 / S)
        cs <- pmin(Ctot_target / S, 0.9 * B)
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
      Fg <- q * Eff[t, ]  # gear x group
      Fg[, !g$harvest_allowed] <- 0
      Ft <- colSums(Fg)
      Ctake <- numeric(ng)
      for (s in seq_len(S)) {
        B <- .logistic_step(B, r, K, 1 / S)
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
    Cprev <- out$C[t, ]
  }
  # SRA-style check: largest annual share of landings the simulated stock could NOT support
  out$catch_shortfall <- ifelse(hist_target > 0, max_short, NA_real_)
  out$catch_shortfall_total <- ifelse(hist_target > 0, 1 - hist_taken / hist_target, NA_real_)
  out$q <- if (is.null(q)) .estimate_q(out, Eff, years, qy, gears, gn) else q
  out$weights <- w
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
