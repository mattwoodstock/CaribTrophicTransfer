# ==============================================================================
# NCRMP / RVC reef fish survey ingestion
# ==============================================================================
# The National Coral Reef Monitoring Program (NCRMP) reef visual census uses a
# stratified random design: primary sample units (PSUs, 200 x 200 m) within
# habitat x depth (x protection) strata; each PSU is surveyed with one or more
# stationary point counts or belt transects (sampled area per station ~177 m2
# for a 7.5 m radius cylinder). Analysis-ready files list one row per species x
# length observation (count NUM at length LEN, cm).
#
# This module converts those records into model inputs:
#   * stratified mean biomass density (g m-2 = t km-2) by FMP group and year,
#     with standard errors (Cochran 1977 stratified estimator),
#   * relative-abundance indices for SIR / out-of-sample validation,
#   * partition priors (biomass density) for groups.csv.
# ==============================================================================

#' Read an NCRMP / RVC analysis-ready fish file
#'
#' Column names are matched case-insensitively; common NCRMP variants are
#' recognised (e.g., `PRIMARY_SAMPLE_UNIT`/`PSU`, `STRAT`/`STRATUM`,
#' `SPECIES_CD`, `NUM`/`NUMBER`, `LEN`/`LENGTH`).
#' @param file CSV path (or URL)
#' @param area_m2 sampled area per station (default 177 m2, 7.5 m radius)
#' @return data frame year, psu, station, strat, species_cd, num, len, area_m2
#' @export
read_ncrmp_fish <- function(file, area_m2 = 177) {
  x <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  nm <- toupper(names(x))
  pick <- function(opts, required = TRUE) {
    i <- match(opts, nm); i <- i[!is.na(i)]
    if (!length(i)) { if (required) stop("NCRMP file lacks column: ", paste(opts, collapse = "/")); return(NULL) }
    x[[i[1]]]
  }
  out <- data.frame(
    year = as.integer(pick(c("YEAR", "SURVEY_YEAR"))),
    psu = as.character(pick(c("PRIMARY_SAMPLE_UNIT", "PSU", "PSU_ID"))),
    station = as.character(if (is.null(s <- pick(c("STATION_NR", "STATION", "STATION_NUMBER"), FALSE))) 1 else s),
    strat = as.character(pick(c("STRAT", "STRATUM", "STRATA"))),
    species_cd = toupper(trimws(gsub("_", " ", pick(c("SPECIES_CD", "SPECIES_CODE", "SPECIES"))))),
    num = as.numeric(pick(c("NUM", "NUMBER", "COUNT", "ABUNDANCE"))),
    len = as.numeric(pick(c("LEN", "LENGTH", "LENGTH_CM", "TOTAL_LENGTH"))),
    stringsAsFactors = FALSE)
  out$area_m2 <- area_m2
  out[!is.na(out$num), ]
}

#' Stratified biomass density by group and year
#'
#' Station biomass = sum(num x a len^b) / area; PSU mean over stations; stratum
#' mean and variance over PSUs; domain estimate weighted by stratum size
#' (Cochran 1977): ybar = sum_h W_h ybar_h, var = sum_h W_h^2 s_h^2 / n_h.
#' @param fish output of [read_ncrmp_fish()]
#' @param species_map data frame with `species_cd`, `group`, `a`, `b`
#'   (FishBase length-weight, W[g] = a L[cm]^b)
#' @param strata data frame `strat`, `ntot` (number of PSUs or area per stratum);
#'   if NULL, strata are weighted equally
#' @return data frame year, group, density_g_m2, se, cv, n_psu
#' @export
ncrmp_stratified_biomass <- function(fish, species_map, strata = NULL) {
  sm <- species_map[!duplicated(species_map$species_cd), c("species_cd", "group", "a", "b")]
  f <- merge(fish, sm, by = "species_cd")
  f$w_g <- f$num * f$a * f$len^f$b
  # all surveyed stations (including zero catches) define the sampling frame
  st <- unique(fish[, c("year", "psu", "station", "strat", "area_m2")])
  groups <- unique(sm$group)
  out <- list()
  for (yr in sort(unique(st$year))) {
    sy <- st[st$year == yr, ]
    for (gr in groups) {
      fy <- f[f$year == yr & f$group == gr, ]
      b <- stats::aggregate(w_g ~ psu + station, data = fy, FUN = sum)
      sy2 <- merge(sy, b, by = c("psu", "station"), all.x = TRUE)
      sy2$w_g[is.na(sy2$w_g)] <- 0
      sy2$dens <- sy2$w_g / sy2$area_m2
      psu <- stats::aggregate(dens ~ psu + strat, data = sy2, FUN = mean)
      hs <- do.call(rbind, lapply(split(psu, psu$strat), function(z)
        data.frame(strat = z$strat[1], mean = mean(z$dens), var = if (nrow(z) > 1) stats::var(z$dens) else 0, n = nrow(z))))
      W <- if (is.null(strata)) rep(1, nrow(hs)) else strata$ntot[match(hs$strat, strata$strat)]
      W[is.na(W)] <- 0; W <- W / sum(W)
      m <- sum(W * hs$mean); v <- sum(W^2 * hs$var / pmax(hs$n, 1))
      out[[length(out) + 1]] <- data.frame(year = yr, group = gr, density_g_m2 = m, se = sqrt(v),
                                           cv = ifelse(m > 0, sqrt(v) / m, NA), n_psu = sum(hs$n))
    }
  }
  do.call(rbind, out)
}

#' Convert NCRMP biomass densities to model indices and partition priors
#'
#' @param dens output of [ncrmp_stratified_biomass()]
#' @param role "fit" (used in SIR) or "validate" (held out for out-of-sample
#'   validation)
#' @param min_cv floor applied to survey CVs
#' @return list(indices = rows for indices.csv, priors = group mean density)
#' @export
ncrmp_to_inputs <- function(dens, role = "validate", min_cv = 0.2) {
  d <- dens[is.finite(dens$density_g_m2) & dens$density_g_m2 > 0, ]
  idx <- data.frame(year = d$year, index = paste0("NCRMP_", d$group), group = d$group,
                    value = round(d$density_g_m2, 4), cv = round(pmax(min_cv, d$cv), 3), lambda = 1,
                    role = role, data_status = "API: NCRMP stratified biomass",
                    reference = "NCRMP reef visual census, stratified biomass density (g/m2)")
  pri <- stats::aggregate(density_g_m2 ~ group, data = d, FUN = mean)
  list(indices = idx, priors = pri)
}
