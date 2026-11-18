# ==============================================================================
# Out-of-sample validation
# ==============================================================================
# Data can be held out of calibration in three ways:
#   1. index rows with `role = "validate"` in indices.csv (e.g., NCRMP survey
#      biomass held entirely out of the fit);
#   2. all index observations from `settings$holdout_from_year` onward
#      (temporal hold-out / forecast skill);
#   3. benchmarks with `use_in_sir = FALSE` (or leave-one-benchmark-out).
# The SIR uses only the fitting data. Because the prior draws (Stage 1) are
# retained, alternative hold-out designs are evaluated by re-weighting the
# same draws, without new simulations.
# ==============================================================================

.fit_indices <- function(proj) {
  idx <- proj$indices
  if (is.null(idx) || !nrow(idx)) return(NULL)
  if (!"role" %in% names(idx)) idx$role <- "fit"
  idx$role[is.na(idx$role) | idx$role == ""] <- "fit"
  hy <- proj$settings$holdout_from_year
  if (!is.null(hy) && !is.na(hy)) idx$role[idx$year >= hy] <- "validate"
  idx <- .fill_col(idx, "cv", proj$settings$index_min_cv)
  idx[idx$role == "fit" & idx$year %in% proj$hist_years, , drop = FALSE]
}

.validate_indices <- function(proj) {
  idx <- proj$indices
  if (is.null(idx) || !nrow(idx)) return(NULL)
  if (!"role" %in% names(idx)) idx$role <- "fit"
  idx$role[is.na(idx$role) | idx$role == ""] <- "fit"
  hy <- proj$settings$holdout_from_year
  if (!is.null(hy) && !is.na(hy)) idx$role[idx$year >= hy] <- "validate"
  idx <- .fill_col(idx, "cv", proj$settings$index_min_cv)
  idx[idx$role == "validate" & idx$year %in% proj$hist_years, , drop = FALSE]
}

# NLL of every prior draw for a given set of index rows (lognormal, analytic q)
.index_nll_rows <- function(B, idx, min_cv) {
  if (is.null(idx) || !nrow(idx)) return(matrix(0, dim(B)[1], 0))
  names_i <- unique(idx$index)
  nll <- matrix(0, dim(B)[1], length(names_i), dimnames = list(NULL, names_i))
  for (nm in names_i) {
    x <- idx[idx$index == nm, ]
    grp <- strsplit(x$group[1], ";")[[1]]
    sig <- sqrt(log(1 + pmax(x$cv, min_cv)^2)); wv <- 1 / sig^2
    Bs <- matrix(B[, as.character(x$year), grp, drop = FALSE], nrow = dim(B)[1])
    Bs <- if (length(grp) > 1) {
      a <- B[, as.character(x$year), grp, drop = FALSE]; apply(a, c(1, 2), sum)
    } else matrix(B[, as.character(x$year), grp], nrow = dim(B)[1])
    lr <- sweep(-log(pmax(Bs, 1e-12)), 2, log(x$value), "+")
    lq <- as.vector(lr %*% wv) / sum(wv)
    nll[, nm] <- sum(log(sig)) + 0.5 * rowSums(sweep((lr - lq)^2, 2, wv, "*"))
  }
  nll
}

.bench_nll_rows <- function(MM_ref, bm) {
  if (is.null(bm) || !nrow(bm)) return(matrix(0, nrow(MM_ref), 0))
  nll <- matrix(0, nrow(MM_ref), nrow(bm), dimnames = list(NULL, paste(bm$group, bm$metric, sep = "_")))
  for (j in seq_len(nrow(bm))) {
    grp <- strsplit(bm$group[j], ";")[[1]]
    pred <- t_to_lb(rowSums(MM_ref[, grp, drop = FALSE]))
    sig <- sqrt(log(1 + ifelse(is.na(bm$sir_cv[j]), 0.5, bm$sir_cv[j])^2))
    nll[, j] <- log(sig) + 0.5 * ((log(bm$value_lb[j]) - log(pred)) / sig)^2
  }
  nll
}

# total NLL used by SIR for arbitrary index / benchmark sets (feasibility included)
.total_nll <- function(res, idx, bm) {
  s1 <- res$stage1; st <- res$proj$settings
  tot <- rep(0, nrow(s1$MMSY_ref))
  ni <- .index_nll_rows(s1$B, idx, st$index_min_cv)
  if (ncol(ni)) {
    lam <- if ("lambda" %in% names(idx)) tapply(idx$lambda, idx$index, function(z) z[1])[colnames(ni)] else rep(1, ncol(ni))
    lam[is.na(lam)] <- 1
    tot <- tot + as.vector(ni %*% lam)
  }
  nb <- .bench_nll_rows(s1$MMSY_ref, bm)
  if (ncol(nb)) tot <- tot + rowSums(nb)
  cf <- catch_feasibility_nll(res)
  if (!is.null(cf)) tot <- tot + rowSums(cf)
  tot
}

.sir_bm <- function(proj) {
  bm <- proj$benchmarks
  if (is.null(bm) || !"use_in_sir" %in% names(bm)) return(NULL)
  bm[as.logical(bm$use_in_sir) %in% TRUE, , drop = FALSE]
}

#' Out-of-sample validation of held-out indices
#'
#' For every held-out series (role `validate` or years >= `holdout_from_year`),
#' the posterior predictive distribution of each observation is built from the
#' SIR-weighted prior draws:
#' * if the series also has fitting years, catchability q is estimated per draw
#'   from those years and predictions are q B_t with lognormal observation error;
#' * if the whole series is held out, the comparison is scale-free: predicted
#'   and observed log-anomalies (value / series geometric mean).
#' Metrics: coverage of 50% / 90% predictive intervals, RMSE of log values,
#' mean absolute scaled error vs a naive persistence/mean forecast (MASE < 1 =
#' better than naive), and correlation of anomalies.
#' @param res `ttm_results` (Stage-1 draws retained)
#' @param idx_fit fitting index rows (default from project)
#' @param idx_val held-out rows (default from project)
#' @param bm benchmarks used in fitting (default project SIR benchmarks)
#' @export
validate_out_of_sample <- function(res, idx_fit = .fit_indices(res$proj), idx_val = .validate_indices(res$proj),
                                   bm = .sir_bm(res$proj)) {
  if (is.null(idx_val) || !nrow(idx_val)) stop("No held-out index observations (set role = 'validate' or settings$holdout_from_year)")
  s1 <- res$stage1; st <- res$proj$settings
  nll <- .total_nll(res, idx_fit, bm)
  w <- exp(-(nll - min(nll))); w <- w / sum(w)
  draw <- sample(seq_along(w), 2000, replace = TRUE, prob = w)
  rows <- list(); obs_rows <- list()
  for (nm in unique(idx_val$index)) {
    v <- idx_val[idx_val$index == nm, ]
    f <- if (!is.null(idx_fit)) idx_fit[idx_fit$index == nm, ] else NULL
    grp <- strsplit(v$group[1], ";")[[1]]
    Bsum <- function(yrs) {
      a <- s1$B[draw, as.character(yrs), grp, drop = FALSE]
      matrix(apply(a, c(1, 2), sum), nrow = length(draw))
    }
    Bv <- Bsum(v$year)
    sig_v <- sqrt(log(1 + pmax(v$cv, st$index_min_cv)^2))
    if (!is.null(f) && nrow(f) >= 2) {
      Bf <- Bsum(f$year); sig_f <- sqrt(log(1 + pmax(f$cv, st$index_min_cv)^2))
      lq <- as.vector(sweep(-log(Bf), 2, log(f$value), "+") %*% (1 / sig_f^2)) / sum(1 / sig_f^2)
      pred <- exp(lq) * Bv * exp(matrix(stats::rnorm(length(Bv), 0, rep(sig_v, each = nrow(Bv))), nrow = nrow(Bv)))
      naive <- rep(utils::tail(f$value[order(f$year)], 1), nrow(v))
      mode <- "forecast (q from fitting years)"
      obs <- v$value
    } else {
      gm <- exp(rowMeans(log(Bv)))
      pred <- (Bv / gm) * exp(matrix(stats::rnorm(length(Bv), 0, rep(sig_v, each = nrow(Bv))), nrow = nrow(Bv)))
      obs <- v$value / exp(mean(log(v$value)))
      naive <- rep(1, nrow(v))
      mode <- "scale-free anomalies (whole series held out)"
    }
    q <- apply(pred, 2, stats::quantile, c(0.05, 0.25, 0.5, 0.75, 0.95))
    err_m <- abs(log(obs) - log(q[3, ])); err_n <- abs(log(obs) - log(naive))
    rows[[nm]] <- data.frame(index = nm, group = v$group[1], mode = mode, n_obs = nrow(v),
                             coverage50 = mean(obs >= q[2, ] & obs <= q[4, ]),
                             coverage90 = mean(obs >= q[1, ] & obs <= q[5, ]),
                             rmse_log = sqrt(mean((log(obs) - log(q[3, ]))^2)),
                             MASE_vs_naive = mean(err_m) / max(mean(err_n), 1e-9),
                             cor_anomaly = if (nrow(v) > 2) stats::cor(log(obs), log(q[3, ])) else NA)
    obs_rows[[nm]] <- data.frame(index = nm, year = v$year, observed = obs, pred_p05 = q[1, ], pred_p50 = q[3, ],
                                 pred_p95 = q[5, ])
  }
  list(summary = do.call(rbind, rows), observations = do.call(rbind, obs_rows),
       ess_fit = 1 / sum(w^2))
}

#' Leave-one-benchmark-out cross-validation
#'
#' For each SIR benchmark (e.g., SEDAR OFL), refits the SIR weights without it
#' and reports where the held-out benchmark falls in the predictive
#' distribution of that group's MMSY (PIT; 0.05-0.95 = inside the 90%
#' interval) and the ratio of the predicted median to the benchmark.
#' @param res `ttm_results`
#' @export
loo_benchmarks <- function(res) {
  bm <- .sir_bm(res$proj)
  if (is.null(bm) || !nrow(bm)) stop("No SIR benchmarks")
  idx <- .fit_indices(res$proj); s1 <- res$stage1
  do.call(rbind, lapply(seq_len(nrow(bm)), function(j) {
    nll <- .total_nll(res, idx, bm[-j, , drop = FALSE])
    w <- exp(-(nll - min(nll))); w <- w / sum(w)
    grp <- strsplit(bm$group[j], ";")[[1]]
    pred <- t_to_lb(rowSums(s1$MMSY_ref[, grp, drop = FALSE]))
    q <- .wquantile(pred, w, c(0.05, 0.5, 0.95))
    data.frame(held_out = paste(bm$group[j], bm$metric[j]), benchmark_lb = bm$value_lb[j],
               pred_p05 = q[1], pred_p50 = q[2], pred_p95 = q[3],
               PIT = sum(w[pred <= bm$value_lb[j]]), inside90 = bm$value_lb[j] >= q[1] & bm$value_lb[j] <= q[3],
               ratio_pred_to_benchmark = q[2] / bm$value_lb[j], ess = 1 / sum(w^2))
  }))
}

#' Retrospective (data-peel) stability of MMSY estimates
#'
#' Re-weights the prior draws using index data truncated at successively
#' earlier terminal years and reports the relative change of posterior median
#' MMSY by group (a Mohn's-rho analogue for reference points).
#' @param res `ttm_results`
#' @param peels number of years removed in each peel
#' @export
retrospective_peel <- function(res, peels = c(3, 6, 9)) {
  idx <- .fit_indices(res$proj); bm <- .sir_bm(res$proj); s1 <- res$stage1
  g <- res$proj$groups; grp <- g$group[g$group_type == "fmp" & g$harvest_allowed]
  base_w <- { n <- .total_nll(res, idx, bm); w <- exp(-(n - min(n))); w / sum(w) }
  base <- vapply(grp, function(gi) .wquantile(s1$MMSY_ref[, gi], base_w, 0.5), numeric(1))
  ty <- if (is.null(idx)) NA else max(idx$year)
  do.call(rbind, lapply(peels, function(pk) {
    ii <- if (is.null(idx)) NULL else idx[idx$year <= ty - pk, ]
    n <- .total_nll(res, ii, bm); w <- exp(-(n - min(n))); w <- w / sum(w)
    est <- vapply(grp, function(gi) .wquantile(s1$MMSY_ref[, gi], w, 0.5), numeric(1))
    data.frame(peel_years = pk, terminal_index_year = ty - pk, group = grp, rel_change_MMSY = est / base - 1,
               row.names = NULL)
  }))
}
