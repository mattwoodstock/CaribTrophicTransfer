# ==============================================================================
# Life-history (per-recruit) priors for the MMSY fraction x_msy
# ==============================================================================
# The TTM caps yield at MMSY = x * (P/B) * K. With logistic production x = 0.5
# (Gulland 1971; Schaefer). Age-structured theory shows MSY / (M B0) depends on
# growth, maturity, gear selectivity and stock-recruit steepness (Beddington &
# Cooke 1983; Clark 1991; Zhou et al. 2012). These functions derive a
# group-specific prior for x from FishBase-type traits:
#   L(a) = Linf (1 - exp(-K (a + 0.5)))         von Bertalanffy, mid-year length
#   W(a) = a L^b                                 length-weight
#   mat(a) = logistic(L; Lm, 0.1 Lm)             maturity (Lm from Froese & Binohlan 2000 if missing)
#   sel(a) = logistic(L; Lc, 0.1 Lc)             trap retention
#   R(S) Beverton-Holt with steepness h          (Mace & Doonan 1988 parameterization)
# Equilibrium yield Y(F) = R(F) YPR(F); B0 = R0 BPR(0) (total biomass, matching
# the TTM's K); x = max_F Y(F) / (M B0).
# ==============================================================================

#' Length at maturity from asymptotic length (Froese & Binohlan 2000)
#' @param linf asymptotic length (cm)
#' @export
lm_from_linf <- function(linf) 10^(0.8979 * log10(linf) - 0.0782)

#' Per-recruit MSY fraction x = MSY / (M B0)
#'
#' @param M natural mortality (yr-1)
#' @param K von Bertalanffy growth coefficient (yr-1)
#' @param linf asymptotic length (cm)
#' @param lc length at 50% gear retention (cm); 0 = knife-edge at recruitment
#' @param lm length at 50% maturity (cm); default from [lm_from_linf()]
#' @param h Beverton-Holt steepness
#' @param a,b length-weight parameters (W = a L^b; units cancel)
#' @param max_age plus-group age (default: age at which 1% of a cohort survives M)
#' @return list(x, Fmsy, Fmsy_over_M, Bmsy_over_B0)
#' @export
per_recruit_xmsy <- function(M, K, linf, lc = 0, lm = NULL, h = 0.75, a = 0.01, b = 3,
                             max_age = NULL) {
  if (is.null(lm) || is.na(lm)) lm <- lm_from_linf(linf)
  if (is.null(max_age)) max_age <- max(5, ceiling(-log(0.01) / M))
  ages <- 0:max_age
  L <- linf * (1 - exp(-K * (ages + 0.5)))
  W <- a * L^b
  mat <- stats::plogis((L - lm) / (0.1 * lm))
  sel <- if (lc > 0) stats::plogis((L - lc) / (0.1 * lc)) else rep(1, length(ages))
  pr <- function(F) {
    Z <- M + F * sel
    N <- numeric(length(ages)); N[1] <- 1
    for (i in 2:length(ages)) N[i] <- N[i - 1] * exp(-Z[i - 1])
    N[length(ages)] <- N[length(ages)] / (1 - exp(-Z[length(ages)]))   # plus group
    Nbar <- N * (1 - exp(-Z)) / Z
    c(spr = sum(N * W * mat), bpr = sum(Nbar * W), ypr = sum(Nbar * W * F * sel))
  }
  p0 <- pr(0)
  eq <- function(F) {
    p <- pr(F); phi <- p[["spr"]] / p0[["spr"]]
    R <- max(0, (4 * h * phi - (1 - h)) / ((5 * h - 1) * phi))
    c(Y = R * p[["ypr"]], B = R * p[["bpr"]])
  }
  # grid search then local refinement (yield is zero beyond the crash F)
  Fg <- seq(0, 6 * M + 1, length.out = 121)
  Yg <- vapply(Fg, function(F) eq(F)[["Y"]], numeric(1))
  k <- which.max(Yg); lo <- Fg[max(1, k - 1)]; hi <- Fg[min(length(Fg), k + 1)]
  Fmsy <- if (hi > lo) stats::optimize(function(F) -eq(F)[["Y"]], c(lo, hi))$minimum else Fg[k]
  e <- eq(Fmsy)
  list(x = e[["Y"]] / (M * p0[["bpr"]]), Fmsy = Fmsy, Fmsy_over_M = Fmsy / M,
       Bmsy_over_B0 = e[["B"]] / p0[["bpr"]])
}

#' Group-level x_msy prior by Monte Carlo over species, traits and steepness
#'
#' @param species data frame with columns `group`, `M`, `K`, `linf`, optional
#'   `lm`, `a`, `b`, `weight` (species weight within its group)
#' @param lc data frame `group`, `lc` (cm), `lc_cv` (trap retention length)
#' @param h_mean,h_sd steepness prior (truncated to 0.25-0.99); default 0.75 +- 0.1
#'   (Myers et al. 1999; Shertzer & Conn 2012 for reef fishes)
#' @param trait_cv lognormal CV applied to M and K in each draw
#' @param n draws per group
#' @return data frame group, x_msy (mean), x_msy_cv, x_p05, x_p95, Fmsy_over_M, Bmsy_over_B0
#' @export
life_history_xmsy <- function(species, lc = NULL, h_mean = 0.75, h_sd = 0.1, trait_cv = 0.2, n = 500) {
  if (!"weight" %in% names(species)) species$weight <- 1
  if (!"a" %in% names(species)) species$a <- 0.01
  if (!"b" %in% names(species)) species$b <- 3
  if (!"lm" %in% names(species)) species$lm <- NA
  do.call(rbind, lapply(split(species, species$group), function(sp) {
    sp <- sp[is.finite(sp$M) & is.finite(sp$K) & is.finite(sp$linf), , drop = FALSE]
    if (!nrow(sp)) return(NULL)
    gi <- sp$group[1]
    lcg <- if (!is.null(lc) && gi %in% lc$group) lc[lc$group == gi, ] else data.frame(lc = 0, lc_cv = 0)
    w <- ifelse(is.finite(sp$weight) & sp$weight > 0, sp$weight, 1)
    res <- t(vapply(seq_len(n), function(i) {
      j <- sample.int(nrow(sp), 1, prob = w)
      h <- min(0.99, max(0.25, stats::rnorm(1, h_mean, h_sd)))
      M <- rlnorm_cv(1, sp$M[j], trait_cv); K <- rlnorm_cv(1, sp$K[j], trait_cv)
      L <- rlnorm_cv(1, lcg$lc, lcg$lc_cv)
      r <- per_recruit_xmsy(M, K, sp$linf[j], lc = L, lm = sp$lm[j], h = h,
                            a = ifelse(is.finite(sp$a[j]), sp$a[j], 0.01), b = ifelse(is.finite(sp$b[j]), sp$b[j], 3))
      c(r$x, r$Fmsy_over_M, r$Bmsy_over_B0)
    }, numeric(3)))
    x <- res[, 1]
    data.frame(group = gi, x_msy = mean(x), x_msy_cv = stats::sd(x) / mean(x),
               x_p05 = stats::quantile(x, 0.05, names = FALSE), x_p95 = stats::quantile(x, 0.95, names = FALSE),
               Fmsy_over_M = stats::median(res[, 2]), Bmsy_over_B0 = stats::median(res[, 3]),
               n_species = nrow(sp))
  }))
}

#' Pella-Tomlinson shape parameter for a given B_MSY / K
#'
#' Solves n^(1/(1-n)) = bmsy_k (n = 2: Schaefer, B_MSY = K/2; n -> 1: Fox,
#' B_MSY = K/e). Used so that the biomass dynamics share the B_MSY/B0 implied by
#' the per-recruit analysis that sets x (Pella & Tomlinson 1969; Fletcher 1978).
#' @param bmsy_k B_MSY as a fraction of K (0.2-0.6)
#' @export
pt_shape <- function(bmsy_k) {
  vapply(bmsy_k, function(b) {
    if (!is.finite(b) || abs(b - 0.5) < 1e-9) return(2)
    if (abs(b - exp(-1)) < 1e-4) return(1 + 1e-4)
    f <- function(n) n^(1 / (1 - n)) - b
    if (b < exp(-1)) stats::uniroot(f, c(0.02, 1 - 1e-6), tol = 1e-12)$root else stats::uniroot(f, c(1 + 1e-6, 50), tol = 1e-12)$root
  }, numeric(1))
}
