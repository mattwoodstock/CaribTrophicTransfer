# ==============================================================================
# Parameter draws for Monte Carlo uncertainty propagation
# ==============================================================================

#' Lognormal draw with given arithmetic mean and CV
#' @param n number of draws
#' @param mean arithmetic mean (recycled)
#' @param cv coefficient of variation (recycled; 0 returns `mean`)
#' @export
rlnorm_cv <- function(n, mean, cv) {
  mean <- rep_len(mean, n); cv <- rep_len(cv, n)
  sdlog <- sqrt(log(1 + cv^2))
  out <- mean * exp(stats::rnorm(n, 0, sdlog) - sdlog^2 / 2)
  out[cv <= 0 | is.na(cv)] <- mean[cv <= 0 | is.na(cv)]
  out
}

#' Beta draw on (0,1) with given mean and CV (falls back to the mean if infeasible)
#' @inheritParams rlnorm_cv
#' @export
rbeta_cv <- function(n, mean, cv) {
  mean <- rep_len(mean, n); cv <- rep_len(cv, n)
  out <- mean
  ok <- !is.na(cv) & cv > 0 & mean > 0 & mean < 1
  if (any(ok)) {
    m <- mean[ok]; v <- (cv[ok] * m)^2
    v <- pmin(v, m * (1 - m) * 0.95)
    k <- m * (1 - m) / v - 1
    out[ok] <- stats::rbeta(sum(ok), m * k, (1 - m) * k)
  }
  pmin(pmax(out, 1e-6), 1 - 1e-6)
}

.rdirichlet_rows <- function(P, conc) {
  if (is.null(conc) || is.na(conc) || conc <= 0) return(P)
  out <- P
  for (i in seq_len(nrow(P))) {
    a <- P[i, ] * conc
    x <- ifelse(a > 0, stats::rgamma(length(a), shape = pmax(a, 1e-8)), 0)
    out[i, ] <- x / sum(x)
  }
  out
}

#' Build one parameter set
#'
#' @param proj a validated `ttm_project`.
#' @param stochastic if FALSE returns the deterministic (mean) parameter set.
#' @return a list of parameter vectors/matrices consumed by [simulate_ttm()].
#' @export
draw_parameters <- function(proj, stochastic = TRUE) {
  h <- proj$habitats; g <- proj$groups; st <- proj$settings
  nh <- nrow(h); ng <- nrow(g)
  cv0 <- function(x) if (stochastic) x else 0
  par <- list()
  par$area   <- setNames(rlnorm_cv(nh, h$area_km2, cv0(h$area_cv)), h$habitat)
  par$npp    <- setNames(rlnorm_cv(nh, h$npp_t_C_km2_yr, cv0(h$npp_cv)), h$habitat)
  par$fwf    <- setNames(if (stochastic) rbeta_cv(nh, h$food_web_fraction, h$food_web_fraction_cv)
                         else h$food_web_fraction, h$habitat)
  par$fwf[h$food_web_fraction >= 1 & h$food_web_fraction_cv <= 0] <- 1
  par$tte    <- setNames(if (stochastic) rbeta_cv(nh, h$tte, h$tte_cv) else h$tte, h$habitat)
  par$c_to_ww <- rlnorm_cv(1, st$c_to_ww, cv0(st$c_to_ww_cv))
  par$pb     <- setNames(rlnorm_cv(ng, g$pb, cv0(g$pb_cv)), g$group)
  par$x_msy  <- setNames(if (stochastic) rbeta_cv(ng, g$x_msy, g$x_msy_cv) else g$x_msy, g$group)
  par$init_depletion <- setNames(if (stochastic) rbeta_cv(ng, g$init_depletion, g$init_depletion_cv)
                                 else g$init_depletion, g$group)
  par$partition_value <- setNames(rlnorm_cv(ng, g$partition_value, cv0(g$partition_cv)), g$group)
  par$B0_fixed <- setNames(ifelse(is.na(g$B0_fixed_t), NA,
                                  rlnorm_cv(ng, ifelse(is.na(g$B0_fixed_t), 1, g$B0_fixed_t),
                                            cv0(g$B0_fixed_cv))), g$group)
  par$diet <- if (stochastic) .rdirichlet_rows(proj$diet_matrix, st$diet_concentration)
              else proj$diet_matrix
  par$catch_expansion <- rlnorm_cv(1, st$catch_expansion, cv0(st$catch_expansion_cv))
  par$bleach_mort_per_dhw <- rlnorm_cv(1, st$bleach_mort_per_dhw, cv0(st$bleach_mort_cv))
  er <- proj$env_response
  if (!is.null(er) && nrow(er)) {
    par$env_sens <- rlnorm_cv(nrow(er), abs(ifelse(is.na(er$sensitivity), 0, er$sensitivity)),
                              cv0(er$sensitivity_cv)) * sign(ifelse(is.na(er$sensitivity), 0, er$sensitivity))
  }
  # ---- observation error in data inputs ------------------------------------
  # covariates: value ~ N(value, sd) for rows with an `sd` column
  ec <- proj$env_covariates
  if (stochastic && !is.null(ec) && "sd" %in% names(ec) && any(ec$sd > 0, na.rm = TRUE)) {
    sdv <- ifelse(is.na(ec$sd), 0, ec$sd)
    ec$value <- ec$value + stats::rnorm(nrow(ec), 0, sdv)
    if ("lower" %in% names(ec)) ec$value <- pmax(ec$value, ec$lower, na.rm = TRUE)
    nonneg <- ec$variable %in% c(st$dhw_variable, "coral_cover") | grepl("npp|chl|cover", ec$variable)
    ec$value[nonneg] <- pmax(ec$value[nonneg], 0)
    par$env_covariates <- ec
  }
  # landings and effort: lognormal observation error per row (cv column or setting)
  L <- proj$landings
  if (!is.null(L) && nrow(L)) {
    lcv <- if ("cv" %in% names(L)) ifelse(is.na(L$cv), st$landings_cv, L$cv) else rep(st$landings_cv, nrow(L))
    par$landings_mult <- if (stochastic) rlnorm_cv(nrow(L), 1, lcv) else rep(1, nrow(L))
  }
  E <- proj$effort
  if (!is.null(E) && nrow(E)) {
    ecv <- if ("cv" %in% names(E)) ifelse(is.na(E$cv), st$effort_cv, E$cv) else rep(st$effort_cv, nrow(E))
    par$effort_mult <- if (stochastic) rlnorm_cv(nrow(E), 1, ecv) else rep(1, nrow(E))
  }
  # disturbance intensities
  dd <- proj$disturbances
  if (!is.null(dd) && nrow(dd)) {
    icv <- if ("intensity_cv" %in% names(dd)) ifelse(is.na(dd$intensity_cv), 0, dd$intensity_cv) else rep(0, nrow(dd))
    par$dist_intensity_mult <- if (stochastic) rlnorm_cv(nrow(dd), 1, icv) else rep(1, nrow(dd))
  }
  # habitat preferences, immigration, coral growth
  par$pref <- proj$pref_matrix
  if (stochastic && isTRUE(st$pref_cv > 0))
    par$pref[] <- proj$pref_matrix * rlnorm_cv(length(proj$pref_matrix), 1, st$pref_cv)
  par$immigration <- setNames(rlnorm_cv(ng, g$immigration_t, cv0(g$immigration_cv)), g$group)
  par$coral_growth <- rlnorm_cv(1, st$coral_growth, cv0(st$coral_growth_cv))
  par$dist_loss_mult <- rlnorm_cv(1, 1, cv0(if (is.null(st$disturbance_loss_cv)) 0 else st$disturbance_loss_cv))
  # natural mortality (K and MMSY basis), open recruitment, feedback parameters
  mn <- ifelse(is.na(g$m_nat), NA, rlnorm_cv(ng, ifelse(is.na(g$m_nat), 1, g$m_nat), cv0(g$m_nat_cv)))
  par$m_nat <- setNames(ifelse(is.na(mn), par$pb, mn), g$group)
  par$open_rec <- setNames(ifelse(g$open_recruitment > 0,
                                  if (stochastic) rbeta_cv(ng, pmax(g$open_recruitment, 1e-3), g$open_recruitment_cv)
                                  else g$open_recruitment, 0), g$group)
  par$ghost_eff <- rlnorm_cv(1, st$ghost_efficiency, cv0(st$ghost_efficiency_cv))
  par$herbivory_elasticity <- rlnorm_cv(1, st$herbivory_elasticity, cv0(st$herbivory_elasticity_cv))
  par$process_dev_seed <- if (stochastic) sample.int(.Machine$integer.max, 1) else NA
  par$stochastic <- stochastic
  par
}

#' Trophic level of each group implied by its diet
#'
#' TL_i = 1 + sum_k DC_ik * TL_k where pool TL_k = 1..4.
#' @param diet group x pool diet matrix (rows sum to 1).
#' @export
group_trophic_level <- function(diet) {
  tl_pool <- as.numeric(sub(".*TL", "", colnames(diet)))
  1 + as.vector(diet %*% tl_pool)
}
