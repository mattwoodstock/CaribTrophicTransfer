# ==============================================================================
# Additional data sources: computed satellite NPP (VGPM ensemble) and CMIP6
# projections (delta method), plus ERDDAP helpers for 0-360 longitude datasets
# and multi-year requests.
# ==============================================================================

.meta_cache <- new.env(parent = emptyenv())

#' ERDDAP dataset metadata (time coverage, latitude order, longitude convention)
#' @param server ERDDAP base URL
#' @param dataset_id dataset id
#' @export
erddap_meta <- function(server, dataset_id) {
  key <- paste(server, dataset_id)
  if (!is.null(.meta_cache[[key]])) return(.meta_cache[[key]])
  i <- erddap_info(server, dataset_id)
  g <- function(nm) i$Value[i[["Row Type"]] == "attribute" & i[["Variable Name"]] == "NC_GLOBAL" & i[["Attribute Name"]] == nm]
  lat_dim <- i$Value[i[["Row Type"]] == "dimension" & i[["Variable Name"]] == "latitude"]
  lon_rng <- i$Value[i[["Row Type"]] == "attribute" & i[["Variable Name"]] == "longitude" & i[["Attribute Name"]] == "actual_range"]
  lon_max <- if (length(lon_rng)) max(as.numeric(strsplit(lon_rng, ",")[[1]])) else 180
  m <- list(coverage = as.Date(substr(c(g("time_coverage_start"), g("time_coverage_end")), 1, 10)),
            lat_descending = length(lat_dim) && grepl("averageSpacing=-", lat_dim),
            lon_0360 = lon_max > 180)
  .meta_cache[[key]] <- m
  m
}

#' Domain-mean time series from an ERDDAP griddap dataset
#'
#' Returns one row per time step: spatial (cos-latitude weighted) mean and SD
#' over the bounding box. Requests are split into chunks of `chunk_years`.
#' Handles 0-360 longitude datasets and descending latitude automatically.
#' @inheritParams erddap_griddap_url
#' @param years integer years
#' @param chunk_years years per request (use >1 for monthly products)
#' @param scale,offset transform applied to raw values
#' @param verbose progress messages
#' @return data frame time, year, month, mean, sd, n_cells
#' @export
fetch_erddap_timeseries <- function(server, dataset_id, variable, years, bbox, extra_dims = NULL,
                                    stride = c(time = 1, lat = 1, lon = 1), chunk_years = 1,
                                    scale = 1, offset = 0, verbose = TRUE) {
  m <- erddap_meta(server, dataset_id)
  b <- .bbox(bbox)
  if (m$lon_0360) b[c("lon_min", "lon_max")] <- b[c("lon_min", "lon_max")] %% 360
  yrs <- sort(unique(years)); out <- list()
  chunks <- split(yrs, ceiling(seq_along(yrs) / chunk_years))
  for (ch in chunks) {
    t0 <- max(as.Date(sprintf("%d-01-01", min(ch))), m$coverage[1])
    t1 <- min(as.Date(sprintf("%d-12-31", max(ch))), m$coverage[2])
    if (t0 > t1) next
    url <- erddap_griddap_url(server, dataset_id, variable, c(paste0(t0, "T00:00:00Z"), paste0(t1, "T23:59:59Z")),
                              b, extra_dims, stride, m$lat_descending)
    if (verbose) message("  ERDDAP ", dataset_id, " ", min(ch), if (length(ch) > 1) paste0("-", max(ch)))
    f <- tryCatch(.http_get(url), error = function(e) { warning(conditionMessage(e)); NULL })
    if (is.null(f)) next
    g <- parse_erddap_csv(f, variable); unlink(f)
    g <- g[is.finite(g$value) & as.integer(format(g$time, "%Y")) %in% ch, , drop = FALSE]
    if (!nrow(g)) next
    g$value <- g$value * scale + offset
    w <- cos(g$lat * pi / 180)
    sp <- split(seq_len(nrow(g)), as.character(g$time))
    out[[length(out) + 1]] <- do.call(rbind, lapply(sp, function(ix) {
      mu <- sum(g$value[ix] * w[ix]) / sum(w[ix])
      data.frame(time = g$time[ix[1]], mean = mu,
                 sd = if (length(ix) > 1) sqrt(sum(w[ix] * (g$value[ix] - mu)^2) / sum(w[ix])) else 0,
                 n_cells = length(ix))
    }))
  }
  if (!length(out)) return(data.frame(time = as.POSIXct(character()), year = integer(), month = integer(),
                                      mean = numeric(), sd = numeric(), n_cells = integer()))
  ts <- do.call(rbind, out); rownames(ts) <- NULL
  ts$year <- as.integer(format(ts$time, "%Y")); ts$month <- as.integer(format(ts$time, "%m"))
  ts[order(ts$time), ]
}

.annual_from_ts <- function(ts, time_agg = "mean") {
  if (!nrow(ts)) return(data.frame(year = integer(), value = numeric(), sd = numeric(), n = integer()))
  f <- switch(time_agg, mean = mean, max = max, min = min, sum = sum)
  out <- do.call(rbind, lapply(split(ts, ts$year), function(x)
    data.frame(year = x$year[1], value = f(x$mean), sd = mean(x$sd), n = nrow(x))))
  rownames(out) <- NULL
  out
}

# ---- VGPM net primary production ---------------------------------------------

#' Vertically Generalized Production Model (VGPM) net primary production
#'
#' Behrenfeld & Falkowski (1997) VGPM, as implemented by the Oregon State Ocean
#' Productivity group, with the original 7th-order temperature polynomial for
#' the maximum chlorophyll-specific carbon fixation rate (`model = "vgpm"`) or
#' the Eppley (1972) exponential temperature function (`model = "eppley"`).
#' Euphotic depth follows Morel & Berthon (1989).
#' @param chl surface chlorophyll-a (mg m-3)
#' @param sst sea surface temperature (deg C)
#' @param par photosynthetically available radiation (mol photons m-2 d-1)
#' @param daylength hours of daylight
#' @param model "vgpm" or "eppley"
#' @return NPP in mg C m-2 d-1
#' @export
vgpm_npp <- function(chl, sst, par, daylength, model = c("vgpm", "eppley")) {
  model <- match.arg(model)
  pb <- if (model == "vgpm") {
    t <- sst
    p <- 1.2956 + 2.749e-1 * t + 6.17e-2 * t^2 - 2.05e-2 * t^3 + 2.462e-3 * t^4 -
      1.348e-4 * t^5 + 3.4132e-6 * t^6 - 3.27e-8 * t^7
    p[t < -10] <- 0; p[t < -1] <- 1.13; p[t > 28.5] <- 4.0
    p
  } else 1.54 * 10^(0.0275 * sst - 0.07)
  ctot <- ifelse(chl < 1, 38.0 * chl^0.425, 40.2 * chl^0.507)
  zeu <- 200 * ctot^(-0.293)
  zeu <- ifelse(zeu > 102, 568.2 * ctot^(-0.746), zeu)
  0.66125 * pb * (par / (par + 4.1)) * zeu * chl * daylength
}

#' Day length (hours) from latitude and day of year (Forsythe et al. 1995 CBM model)
#' @param lat latitude (degrees)
#' @param doy day of year
#' @export
day_length <- function(lat, doy) {
  theta <- 0.2163108 + 2 * atan(0.9671396 * tan(0.00860 * (doy - 186)))
  phi <- asin(0.39795 * cos(theta))
  p <- 0.8333
  x <- (sin(p * pi / 180) + sin(lat * pi / 180) * sin(phi)) / (cos(lat * pi / 180) * cos(phi))
  24 - (24 / pi) * acos(pmin(1, pmax(-1, x)))
}

#' Satellite NPP ensemble computed from ocean colour, SST and PAR
#'
#' Computes monthly domain NPP with two members, VGPM and Eppley-VGPM, from:
#' ESA OC-CCI v6 chlorophyll (1997-present), Coral Reef Watch monthly SST
#' (1985-present) and SeaWiFS (1997-2010) / MODIS-Aqua (2002-2022) PAR (gaps
#' filled with the monthly PAR climatology), all served by NOAA PIFSC
#' OceanWatch ERDDAP. Annual values are the mean of monthly NPP converted to
#' g C m-2 yr-1. Uncertainty = between-model spread combined with the
#' propagated within-domain chlorophyll variability. This reproduces the
#' multi-algorithm ensemble logic of SEDAR103-DW-20 Appendix A when the
#' pre-computed products are not reachable.
#' @param bbox domain bounding box
#' @param years years
#' @param server ERDDAP server hosting the inputs
#' @param verbose progress messages
#' @export
fetch_vgpm_ensemble <- function(bbox, years, server = "https://oceanwatch.pifsc.noaa.gov/erddap",
                                verbose = TRUE) {
  b <- .bbox(bbox); lat0 <- mean(b[c("lat_min", "lat_max")])
  chl <- fetch_erddap_timeseries(server, "esa-cci-chla-monthly-v6-0", "chlor_a", years, b, chunk_years = 6, verbose = verbose)
  sst <- fetch_erddap_timeseries(server, "CRW_sst_v3_1_monthly", "sea_surface_temperature", years, b,
                                 stride = c(time = 1, lat = 2, lon = 2), chunk_years = 10, verbose = verbose)
  par1 <- fetch_erddap_timeseries(server, "sw_par_monthly_2018_0", "par", years, b, chunk_years = 14, verbose = verbose)
  par2 <- fetch_erddap_timeseries(server, "aqua_par_monthly_2018_0", "par", years, b, chunk_years = 10, verbose = verbose)
  par <- rbind(par2, par1[!paste(par1$year, par1$month) %in% paste(par2$year, par2$month), ])
  if (!nrow(chl) || !nrow(sst)) stop("Chlorophyll or SST inputs unavailable")
  par_clim <- tapply(par$mean, par$month, mean)
  ym <- chl[, c("year", "month")]
  key <- function(d) paste(d$year, d$month)
  ym$chl <- chl$mean; ym$chl_sd <- chl$sd
  ym$sst <- sst$mean[match(key(ym), key(sst))]
  ym$par <- par$mean[match(key(ym), key(par))]
  ym$par_source <- ifelse(is.na(ym$par), "climatology", "observed")
  ym$par[is.na(ym$par)] <- par_clim[as.character(ym$month[is.na(ym$par)])]
  ym <- ym[is.finite(ym$sst) & is.finite(ym$par), ]
  doy <- as.integer(format(as.Date(sprintf("%d-%02d-15", ym$year, ym$month)), "%j"))
  dl <- day_length(lat0, doy)
  ym$npp_vgpm <- vgpm_npp(ym$chl, ym$sst, ym$par, dl, "vgpm")
  ym$npp_eppley <- vgpm_npp(ym$chl, ym$sst, ym$par, dl, "eppley")
  # NPP scales ~ chl^0.6-0.7 through chl x Zeu: propagate within-domain chl variability
  ym$rel_sd_chl <- 0.65 * ym$chl_sd / ym$chl
  ann <- do.call(rbind, lapply(split(ym, ym$year), function(x) {
    v <- mean(x$npp_vgpm) * 365 / 1000; e <- mean(x$npp_eppley) * 365 / 1000
    m <- mean(c(v, e))
    spatial <- m * mean(x$rel_sd_chl) / sqrt(nrow(x))
    data.frame(year = x$year[1], value = m, sd = sqrt(stats::sd(c(v, e))^2 + spatial^2),
               vgpm = v, eppley = e, n_months = nrow(x), par_climatology_months = sum(x$par_source == "climatology"),
               products = "VGPM;Eppley-VGPM (OC-CCI chl, CRW SST, SeaWiFS/MODIS PAR)")
  }))
  rownames(ann) <- NULL
  attr(ann, "monthly") <- ym
  ann
}

# ---- CMIP6 projections (delta method) -----------------------------------------

#' CMIP6 models available on NOAA PIFSC OceanWatch ERDDAP
#' @export
cmip6_catalog <- function() {
  data.frame(model = c("cesm2", "access-esm1-5"),
             grid_id = c("grid_cesm2", "grid_access-esm1-5"),
             jdim = c("nlat", "j"), idim = c("nlon", "i"),
             latvar = c("lat", "latitude"), lonvar = c("lon", "longitude"),
             stringsAsFactors = FALSE)
}

.cmip6_cells <- function(server, model, bbox, min_cells = 4) {
  cc <- cmip6_catalog(); cc <- cc[cc$model == model, ]
  key <- paste("grid", server, model)
  if (is.null(.meta_cache[[key]])) {
    f <- .http_get(sprintf("%s/griddap/%s.csv?%s,%s", server, cc$grid_id, cc$latvar, cc$lonvar))
    x <- utils::read.csv(f, skip = 2, header = FALSE)
    names(x) <- c(cc$jdim, cc$idim, "lat", "lon")
    .meta_cache[[key]] <- x
  }
  g <- .meta_cache[[key]]
  b <- .bbox(bbox); cx <- mean(b[c("lon_min", "lon_max")]) %% 360; cy <- mean(b[c("lat_min", "lat_max")])
  d <- .haversine_km(cx, cy, g$lon %% 360, g$lat)
  pick <- g[order(d)[seq_len(max(min_cells * 4, 16))], ]
  list(j = range(pick[[cc$jdim]]), i = range(pick[[cc$idim]]), cells = pick, cc = cc)
}

#' Regional annual series from a CMIP6 model on PIFSC OceanWatch
#'
#' Selects the ocean cells nearest the domain centre on the model's native
#' (curvilinear) grid, averages them and aggregates to annual means.
#' @param model "cesm2" or "access-esm1-5"
#' @param experiment "hist", "ssp245", "ssp370" or "ssp585"
#' @param variable "tos" (degC), "intpp" (mol m-2 s-1), "chlos", "mlotst", ...
#' @param bbox domain bounding box
#' @param years years
#' @param server ERDDAP server
#' @export
fetch_cmip6_series <- function(model, experiment, variable, bbox, years,
                               server = "https://oceanwatch.pifsc.noaa.gov/erddap") {
  sel <- .cmip6_cells(server, model, bbox)
  ds <- sprintf("%s_3d_%s", model, experiment)
  m <- erddap_meta(server, ds)
  t0 <- max(as.Date(sprintf("%d-01-01", min(years))), m$coverage[1]); t1 <- min(as.Date(sprintf("%d-12-31", max(years))), m$coverage[2])
  if (t0 > t1) return(data.frame(year = integer(), value = numeric()))
  q <- sprintf("[(%sT00:00:00Z):1:(%sT23:59:59Z)][%d:1:%d][%d:1:%d]", t0, t1, sel$j[1], sel$j[2], sel$i[1], sel$i[2])
  f <- .http_get(paste0(server, "/griddap/", ds, ".csv?", variable, utils::URLencode(q, reserved = TRUE)))
  x <- utils::read.csv(f, skip = 2, header = FALSE, na.strings = c("NaN", "NA"))
  names(x) <- c("time", "j", "i", "value")
  keep <- paste(x$j, x$i) %in% paste(sel$cells[[sel$cc$jdim]], sel$cells[[sel$cc$idim]])
  x <- x[keep & is.finite(x$value), ]
  x$year <- as.integer(substr(x$time, 1, 4))
  out <- stats::aggregate(value ~ year, data = x, FUN = mean)
  out[out$year %in% years, ]
}

#' CMIP6 delta-method projection of an observed covariate
#'
#' future(y) = obs_baseline + [model(y) - model_baseline]   (`type = "additive"`, e.g. SST)
#' future(y) = obs_baseline * model(y) / model_baseline        (`type = "ratio"`, e.g. NPP index)
#' with the baseline = mean over `baseline_years` (model historical + scenario
#' runs spliced at 2014/2015). Projections are averaged over models; the
#' between-model SD is returned as `sd`. Historical years keep the observations.
#' A 5-year running mean is applied to model anomalies to remove unphased
#' internal variability (model years do not match observed weather).
#' @param obs data frame year, value (observed covariate)
#' @param models CMIP6 models
#' @param experiment SSP experiment
#' @param cmip_var CMIP6 variable
#' @param bbox domain bounding box
#' @param proj_years projection years
#' @param baseline_years baseline period
#' @param type "additive" or "ratio"
#' @param scale multiplier applied to model values (unit conversion)
#' @export
cmip6_delta_projection <- function(obs, models, experiment, cmip_var, bbox, proj_years,
                                   baseline_years = 2005:2014, type = c("additive", "ratio"), scale = 1) {
  type <- match.arg(type)
  ob <- mean(obs$value[obs$year %in% baseline_years])
  if (!is.finite(ob)) ob <- mean(utils::tail(obs$value, 10))
  yrs <- seq(min(baseline_years) - 2, max(proj_years) + 2)
  per <- lapply(models, function(mo) {
    h <- fetch_cmip6_series(mo, "hist", cmip_var, bbox, yrs[yrs <= 2014])
    s <- fetch_cmip6_series(mo, experiment, cmip_var, bbox, yrs[yrs >= 2013])
    s <- s[!s$year %in% h$year, ]
    x <- rbind(h, s); x <- x[order(x$year), ]
    x$value <- x$value * scale
    x$smooth <- stats::filter(x$value, rep(1 / 5, 5), sides = 2)
    x$smooth[is.na(x$smooth)] <- x$value[is.na(x$smooth)]
    mb <- mean(x$value[x$year %in% baseline_years])
    x$proj <- if (type == "additive") ob + (x$smooth - mb) else ob * x$smooth / mb
    data.frame(year = x$year, model = mo, proj = as.numeric(x$proj))
  })
  per <- do.call(rbind, per)
  per <- per[per$year %in% proj_years, ]
  out <- do.call(rbind, lapply(split(per, per$year), function(x)
    data.frame(year = x$year[1], value = mean(x$proj), sd = if (nrow(x) > 1) stats::sd(x$proj) else 0,
               models = paste(x$model, collapse = ";"))))
  rownames(out) <- NULL
  out
}

# ---- Bathymetry and light-limited benthic production ---------------------------

#' Bathymetry grid from NCEI ETOPO 2022 (ERDDAP)
#' @param bbox bounding box
#' @param server ERDDAP server (default PIFSC OceanWatch)
#' @param dataset_id ETOPO dataset (15, 30 or 60 arc-second)
#' @param stride grid stride
#' @return data frame lon, lat, depth (m, positive down; land negative)
#' @export
fetch_bathymetry <- function(bbox, server = "https://oceanwatch.pifsc.noaa.gov/erddap",
                             dataset_id = "ETOPO_2022_v1_15s", stride = 1) {
  b <- .bbox(bbox)
  m <- erddap_meta(server, dataset_id)
  lon <- c(b[["lon_min"]], b[["lon_max"]]); if (m$lon_0360) lon <- lon %% 360
  lat <- if (m$lat_descending) c(b[["lat_max"]], b[["lat_min"]]) else c(b[["lat_min"]], b[["lat_max"]])
  q <- sprintf("[(%s):%d:(%s)][(%s):%d:(%s)]", lat[1], stride, lat[2], lon[1], stride, lon[2])
  f <- .http_get(paste0(server, "/griddap/", dataset_id, ".csv?z", utils::URLencode(q, reserved = TRUE)))
  x <- utils::read.csv(f, skip = 2, header = FALSE)
  names(x) <- c("lat", "lon", "z")
  x$lon <- ifelse(x$lon > 180, x$lon - 360, x$lon)
  data.frame(lon = x$lon, lat = x$lat, depth = -x$z)
}

#' Light-limited benthic net primary production by depth
#'
#' Scales a reference shallow-habitat NPP to deeper bottoms using the light
#' reaching the seafloor: I(z) = PAR0 exp(-Kd_PAR z), with Kd_PAR from Kd490
#' (Morel et al. 2007: Kd_PAR = 0.0864 + 0.884 Kd490 - 0.00137 / Kd490) and a
#' saturating photosynthesis-irradiance curve P = Pmax tanh(I / Ik). The
#' relative production f(z) = tanh(I(z)/Ik) / tanh(I(z_ref)/Ik) is averaged over
#' the depth distribution of the habitat (from bathymetry) and Monte Carlo
#' over uncertain Kd490, PAR and Ik.
#' @param depths numeric vector of bottom depths (m) of the habitat cells
#' @param npp_ref reference NPP at depth `z_ref` (any unit; output same unit)
#' @param kd490 mean Kd490 (m-1) and `kd490_cv`
#' @param par0 surface PAR (mol photons m-2 d-1) and `par0_cv`
#' @param ik saturation irradiance (mol photons m-2 d-1) and `ik_cv`
#' @param z_ref reference depth of `npp_ref`
#' @param n Monte Carlo samples
#' @return list with mean, cv, quantiles of the scaled NPP and the depth summary
#' @export
light_limited_npp <- function(depths, npp_ref, kd490, par0, ik = 4, z_ref = 10,
                              kd490_cv = 0.25, par0_cv = 0.1, ik_cv = 0.4, n = 2000) {
  depths <- depths[is.finite(depths) & depths > 0]
  sims <- vapply(seq_len(n), function(i) {
    k490 <- rlnorm_cv(1, kd490, kd490_cv); p0 <- rlnorm_cv(1, par0, par0_cv); ikk <- rlnorm_cv(1, ik, ik_cv)
    kpar <- 0.0864 + 0.884 * k490 - 0.00137 / k490
    f <- tanh(p0 * exp(-kpar * depths) / ikk) / tanh(p0 * exp(-kpar * z_ref) / ikk)
    npp_ref * mean(f)
  }, numeric(1))
  list(mean = mean(sims), cv = stats::sd(sims) / mean(sims),
       q = stats::quantile(sims, c(0.05, 0.5, 0.95)),
       depth_summary = stats::quantile(depths, c(0.05, 0.25, 0.5, 0.75, 0.95)))
}
