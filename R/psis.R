# ==============================================================================
# Pareto-smoothed importance sampling (Vehtari, Simpson, Gelman, Yao & Gabry 2024)
# ==============================================================================

.gpdfit <- function(x) {
  # Zhang & Stephens (2009) generalized Pareto fit with the weakly informative
  # prior on k used by the loo package; x sorted ascending exceedances
  n <- length(x); prior <- 3; m <- 30 + floor(sqrt(n))
  xstar <- x[max(1, floor(n / 4 + 0.5))]
  theta <- 1 / x[n] + (1 - sqrt(m / (seq_len(m) - 0.5))) / prior / xstar
  k <- vapply(theta, function(th) mean(log1p(-th * x)), numeric(1))
  l_theta <- n * (log(-theta / k) - k - 1)
  l_theta[!is.finite(l_theta)] <- -Inf
  w <- exp(l_theta - max(l_theta)); w <- w / sum(w)
  th <- sum(theta * w)
  k_hat <- mean(log1p(-th * x)); sigma <- -k_hat / th
  list(k = (n * k_hat + 10 * 0.5) / (n + 10), sigma = sigma)
}

.qgpd <- function(p, k, sigma) {
  if (abs(k) < 1e-12) return(-sigma * log1p(-p))
  sigma * expm1(-k * log1p(-p)) / k
}

#' Pareto-smoothed importance weights
#'
#' Replaces the largest importance weights by expected order statistics of a
#' generalized Pareto distribution fitted to the upper tail, which stabilizes
#' SIR when a few draws dominate. The shape estimate k diagnoses reliability:
#' k < 0.5 good, 0.5-0.7 acceptable, > 0.7 unreliable (increase draws).
#' @param lw log importance weights (unnormalized)
#' @return list(lw = smoothed log weights, k = Pareto shape estimate, tail_len)
#' @export
psis_smooth <- function(lw) {
  S <- length(lw); ok <- is.finite(lw)
  M <- ceiling(min(0.2 * S, 3 * sqrt(S)))
  if (sum(ok) < 50 || M < 5) return(list(lw = lw, k = NA_real_, tail_len = 0))
  lw <- lw - max(lw[ok])
  o <- order(lw, decreasing = TRUE)
  tail_ix <- o[seq_len(M)]; cut <- lw[o[M + 1]]
  ex <- sort(exp(lw[tail_ix]) - exp(cut))
  if (all(ex <= 0)) return(list(lw = lw, k = NA_real_, tail_len = M))
  fit <- .gpdfit(ex)
  if (!is.finite(fit$k)) return(list(lw = lw, k = NA_real_, tail_len = M))
  sm <- exp(cut) + .qgpd((seq_len(M) - 0.5) / M, fit$k, fit$sigma)
  sm <- pmin(sm, 1)                     # truncate at the largest raw weight (= 1 after scaling)
  rank_in_tail <- order(order(lw[tail_ix]))
  lw[tail_ix] <- log(sm[rank_in_tail])
  list(lw = lw, k = fit$k, tail_len = M)
}
