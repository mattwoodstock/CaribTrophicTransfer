# ==============================================================================
# Environmental, habitat, disturbance and scenario forcing
# ==============================================================================
# Drivers are expanded to annual matrices (year x habitat / gear / group /
# covariate) once per scenario so that the annual simulation loop is cheap.
# ==============================================================================

.recovery_curve <- function(years, t0, loss, half_life) {
  # multiplicative impact: 1 - loss at t0, recovering exponentially thereafter
  m <- rep(1, length(years))
  hit <- years >= t0
  hl <- ifelse(is.na(half_life) | half_life <= 0, 1e-6, half_life)
  m[hit] <- 1 - pmin(1, loss) * 0.5^((years[hit] - t0) / hl)
  m
}

#' Scenario definitions present in a project
#' @param proj a `ttm_project`
#' @export
scenario_names <- function(proj) {
  s <- if (is.null(proj$scenarios)) character(0) else unique(proj$scenarios$scenario)
  unique(c("Baseline", s))
}

.scenario_rows <- function(proj, scenario) {
  sc <- proj$scenarios
  if (is.null(sc) || scenario == "Baseline") return(NULL)
  sc <- sc[sc$scenario == scenario, , drop = FALSE]
  if (!nrow(sc)) stop("Scenario not found: ", scenario)
  sc <- .fill_col(sc, "start_year", proj$settings$hist_end + 1)
  sc <- .fill_col(sc, "end_year", proj$settings$proj_end)
  sc <- .fill_col(sc, "target", "all")
  sc
}

#' Build annual covariate matrix (history + projection) for a scenario
#'
#' Historical values come from `env_covariates.csv` (linearly interpolated,
#' constant extrapolation at the ends). Projection years start from the mean
#' of the final five historical years, then scenario drivers `cov_offset`,
#' `cov_trend` and `cov_set` are applied. Covariates can be produced from
#' satellite products, GLORYS reanalysis or MOM6 projections with
#' [aggregate_gridded_covariate()].
#' @param proj a `ttm_project`
#' @param scenario scenario name
#' @param par optional parameter draw carrying perturbed covariates
#'   (observation error, see [draw_parameters()])
#' @export
build_covariates <- function(proj, scenario = "Baseline", par = NULL) {
  years <- proj$years; hy <- proj$hist_years
  ec <- if (!is.null(par$env_covariates)) par$env_covariates else proj$env_covariates
  vars <- if (is.null(ec)) character(0) else unique(ec$variable)
  sc <- .scenario_rows(proj, scenario)
  if (!is.null(sc)) vars <- unique(c(vars, sc$target[sc$driver %in% c("cov_offset", "cov_trend", "cov_set")],
                                     sub("=.*", "", sc$target[sc$driver == "cov_from"])))
  M <- matrix(NA_real_, length(years), length(vars), dimnames = list(years, vars))
  proj_years <- years[years > max(hy)]
  for (v in vars) {
    x <- if (is.null(ec)) NULL else ec[ec$variable == v, , drop = FALSE]
    if (!is.null(x) && nrow(x)) {
      x <- x[order(x$year), ]
      # explicit future values (e.g., MOM6 projection) are kept if supplied
      M[, v] <- if (nrow(x) == 1) x$value else
        stats::approx(x$year, x$value, xout = years, rule = 2)$y
      has_future <- any(x$year > max(hy))
      if (!has_future && length(proj_years)) {
        last <- x[x$year <= max(hy), ]
        base <- mean(utils::tail(last$value, 5))
        M[as.character(proj_years), v] <- base
      }
    } else M[, v] <- 0
  }
  if (!is.null(sc)) for (i in seq_len(nrow(sc))) {
    r <- sc[i, ]
    if (r$driver == "cov_from") {
      # replace a covariate by another series (e.g., sst=sst_ssp585 from a CMIP6 projection)
      parts <- strsplit(r$target, "=")[[1]]
      src <- ec[ec$variable == parts[2], , drop = FALSE]
      if (!nrow(src)) stop("cov_from source variable not found: ", parts[2])
      yy <- years[years >= r$start_year & years <= r$end_year]
      M[as.character(yy), parts[1]] <- stats::approx(src$year, src$value, xout = yy, rule = 2)$y
      next
    }
    if (!r$driver %in% c("cov_offset", "cov_trend", "cov_set")) next
    yy <- as.character(years[years >= r$start_year & years <= r$end_year])
    if (r$driver == "cov_offset") M[yy, r$target] <- M[yy, r$target] + r$value
    if (r$driver == "cov_set")    M[yy, r$target] <- r$value
    if (r$driver == "cov_trend")  M[yy, r$target] <- M[yy, r$target] +
      r$value * (as.numeric(yy) - r$start_year + 1)
  }
  M
}

#' Build all annual driver matrices for a scenario
#'
#' @param proj a `ttm_project`
#' @param scenario scenario name (see [scenario_names()])
#' @param par optional parameter draw; when supplied, covariate observation
#'   error and disturbance-intensity uncertainty of that draw are applied.
#' @return list with `cov`, `area_mult`, `p1_mult`, `quality_mult`,
#'   `coral_loss`, `coral_disease`, `npp_index`, `tte_mult`, `effort_mult`,
#'   `prod_mult`, `imm_mult`.
#' @export
build_drivers <- function(proj, scenario = "Baseline", par = NULL) {
  years <- proj$years; yc <- as.character(years)
  h <- proj$habitats; g <- proj$groups; gears <- proj$gear_names
  ny <- length(years)
  mk <- function(cols) matrix(1, ny, length(cols), dimnames = list(yc, cols))
  drv <- list(years = years, scenario = scenario)
  drv$cov <- build_covariates(proj, scenario, par)
  drv$area_mult <- mk(h$habitat); drv$p1_mult <- mk(h$habitat)
  drv$quality_mult <- mk(h$habitat); drv$npp_index <- mk(h$habitat)
  drv$tte_mult <- mk(h$habitat)
  drv$coral_loss <- mk(h$habitat) * 0; drv$coral_disease <- mk(h$habitat) * 0
  drv$effort_mult <- mk(if (length(gears)) gears else "none")
  drv$prod_mult <- mk(g$group); drv$imm_mult <- mk(g$group)

  # NPP index from covariate (relative to historical mean)
  for (i in seq_len(nrow(h))) {
    v <- h$npp_covariate[i]
    if (!is.na(v) && v %in% colnames(drv$cov)) {
      x <- drv$cov[, v]; base <- mean(x[years %in% proj$hist_years], na.rm = TRUE)
      drv$npp_index[, h$habitat[i]] <- x / base
    }
  }
  # coral disease mortality covariate (optional, hist)
  if ("coral_disease_mortality" %in% colnames(drv$cov))
    for (hb in h$habitat[h$quality_model == "coral"])
      drv$coral_disease[, hb] <- drv$cov[, "coral_disease_mortality"]

  apply_dist <- function(dd, t0_override = NULL, scale = 1) {
    for (j in seq_len(nrow(dd))) {
      r <- dd[j, ]; t0 <- if (is.null(t0_override)) r$year else t0_override
      inten <- r$intensity * scale
      hab <- if (r$habitat %in% c("all_benthic", "all")) h$habitat[h$type == "benthic"]
             else strsplit(r$habitat, ";")[[1]]
      for (hb in intersect(hab, h$habitat)) {
        drv$p1_mult[, hb] <<- drv$p1_mult[, hb] *
          .recovery_curve(years, t0, r$p1_loss * inten, r$half_life_yr)
        if (h$quality_model[h$habitat == hb] == "coral") {
          drv$coral_loss[yc[years == t0], hb] <<- 1 - (1 - drv$coral_loss[yc[years == t0], hb]) *
            (1 - min(1, r$quality_loss * inten))
        } else {
          drv$quality_mult[, hb] <<- drv$quality_mult[, hb] *
            .recovery_curve(years, t0, r$quality_loss * inten, r$half_life_yr)
        }
      }
      if (!is.na(r$gear) && length(gears)) {
        gg <- if (r$gear == "all") gears else intersect(strsplit(r$gear, ";")[[1]], gears)
        for (ge in gg) drv$effort_mult[, ge] <<- drv$effort_mult[, ge] *
          .recovery_curve(years, t0, r$effort_loss * inten, r$effort_half_life_yr)
      }
      if (!is.na(r$group)) {
        gr <- if (r$group == "all") g$group else intersect(strsplit(r$group, ";")[[1]], g$group)
        for (gi in gr) drv$prod_mult[, gi] <<- drv$prod_mult[, gi] *
          .recovery_curve(years, t0, r$production_loss * inten, r$half_life_yr)
      }
    }
  }
  dist <- proj$disturbances
  if (!is.null(dist) && !is.null(par$dist_intensity_mult))
    dist$intensity <- dist$intensity * par$dist_intensity_mult
  if (!is.null(dist) && !is.null(par$dist_loss_mult) && par$dist_loss_mult != 1)
    for (cc in c("p1_loss", "quality_loss", "effort_loss", "production_loss"))
      dist[[cc]] <- pmin(0.95, dist[[cc]] * par$dist_loss_mult)
  if (!is.null(dist)) {
    hist_dist <- dist[!is.na(dist$year), , drop = FALSE]
    if (nrow(hist_dist)) apply_dist(hist_dist)
  }

  sc <- .scenario_rows(proj, scenario)
  if (!is.null(sc)) for (i in seq_len(nrow(sc))) {
    r <- sc[i, ]
    yy <- yc[years >= r$start_year & years <= r$end_year]
    tgt_h <- if (r$target == "all") h$habitat else strsplit(r$target, ";")[[1]]
    switch(r$driver,
      effort_mult = {
        ge <- if (r$target == "all") gears else strsplit(r$target, ";")[[1]]
        drv$effort_mult[yy, ge] <- drv$effort_mult[yy, ge] * r$value
      },
      npp_mult = { drv$npp_index[yy, tgt_h] <- drv$npp_index[yy, tgt_h] * r$value },
      npp_trend = {
        k <- as.numeric(yy) - r$start_year + 1
        drv$npp_index[yy, tgt_h] <- drv$npp_index[yy, tgt_h] * (1 + r$value)^k
      },
      area_trend = {
        k <- as.numeric(yy) - r$start_year + 1
        drv$area_mult[yy, tgt_h] <- drv$area_mult[yy, tgt_h] * pmax(0, (1 + r$value)^k)
      },
      area_mult = { drv$area_mult[yy, tgt_h] <- drv$area_mult[yy, tgt_h] * r$value },
      tte_mult = { drv$tte_mult[yy, tgt_h] <- drv$tte_mult[yy, tgt_h] * r$value },
      coral_disease = { drv$coral_disease[yy, tgt_h] <- drv$coral_disease[yy, tgt_h] + r$value },
      immigration_mult = {
        gr <- if (r$target == "all") g$group else strsplit(r$target, ";")[[1]]
        drv$imm_mult[yy, gr] <- drv$imm_mult[yy, gr] * r$value
      },
      disturbance = {
        if (is.null(dist)) stop("Scenario uses 'disturbance' but disturbances.csv is missing")
        tmpl <- dist[dist$event == r$target, , drop = FALSE]
        if (!nrow(tmpl)) stop("Disturbance template not found: ", r$target)
        apply_dist(tmpl, t0_override = r$start_year, scale = ifelse(is.na(r$value), 1, r$value))
      },
      cov_offset = NULL, cov_trend = NULL, cov_set = NULL, cov_from = NULL,
      stop("Unknown scenario driver: ", r$driver))
  }
  drv
}

#' Environmental production multipliers for each group and year
#'
#' Response types (env_response.csv):
#' * `linear`: 1 + sensitivity * (x - baseline), floored at 0
#' * `thermal_performance`: Gaussian performance curve with optimum `t_opt` and
#'   width `t_width`, scaled to 1 at `baseline`
#' * `threshold`: 1 - sensitivity * max(0, x - threshold), floored at 0
#' * `loglinear`: exp(sensitivity * (x - baseline))
#'
#' `baseline` defaults to the historical mean of the covariate. The response
#' column `target` selects whether the multiplier acts on production
#' (`production`, default) or carrying capacity (`K`).
#' @param proj a `ttm_project`
#' @param drv drivers from [build_drivers()]
#' @param par parameter set from [draw_parameters()]
#' @export
env_multipliers <- function(proj, drv, par) {
  g <- proj$groups; years <- drv$years
  out <- list(production = matrix(1, length(years), nrow(g), dimnames = list(years, g$group)))
  out$K <- out$production
  er <- proj$env_response
  if (is.null(er) || !nrow(er)) return(out)
  er <- .fill_col(er, "target", "production")
  sens <- if (!is.null(par$env_sens)) par$env_sens else er$sensitivity
  for (j in seq_len(nrow(er))) {
    v <- er$variable[j]
    if (!v %in% colnames(drv$cov)) { warning("Covariate not found for env_response: ", v); next }
    x <- drv$cov[, v]
    base <- if (is.na(er$baseline[j])) mean(x[years %in% proj$hist_years]) else er$baseline[j]
    s <- sens[j]
    m <- switch(er$type[j],
      linear = pmax(0, 1 + s * (x - base)),
      loglinear = exp(s * (x - base)),
      threshold = pmax(0, 1 - s * pmax(0, x - er$threshold[j])),
      thermal_performance = exp(-0.5 * ((x - er$t_opt[j]) / er$t_width[j])^2) /
                            exp(-0.5 * ((base - er$t_opt[j]) / er$t_width[j])^2),
      stop("Unknown env_response type: ", er$type[j]))
    gr <- if (er$group[j] == "all") g$group else er$group[j]
    tg <- if (er$target[j] %in% c("K", "k", "capacity")) "K" else "production"
    for (gi in gr) out[[tg]][, gi] <- out[[tg]][, gi] * m
  }
  out
}

#' Aggregate a gridded covariate to an annual regional series
#'
#' Works on a long data frame (lon, lat, time/year, value) such as one
#' extracted from GLORYS12 reanalysis, MOM6-NWA12 projections or the satellite
#' NPP ensemble (Ryan-Keogh et al. 2023). Cells are area-weighted by cos(lat)
#' unless an `area` column is supplied.
#' @param df data frame with columns `lon`, `lat`, `value` and either `year`
#'   or `time` (Date/POSIXct/numeric decimal year).
#' @param lon_range,lat_range numeric length-2 bounding box.
#' @param polygon optional data frame with `lon`,`lat` vertices (point-in-polygon).
#' @param fun aggregation over the year: "mean" or "sum" (sum is useful for
#'   area-integrated NPP when `value` is already per-cell production).
#' @param variable name for the output `variable` column.
#' @return data frame year, variable, value suitable for env_covariates.csv.
#' @export
aggregate_gridded_covariate <- function(df, lon_range = NULL, lat_range = NULL,
                                        polygon = NULL, fun = c("mean", "sum"),
                                        variable = "covariate") {
  fun <- match.arg(fun)
  if (!"year" %in% names(df)) {
    t <- df$time
    df$year <- if (inherits(t, c("Date", "POSIXt"))) as.integer(format(t, "%Y")) else floor(t)
  }
  keep <- rep(TRUE, nrow(df))
  if (!is.null(lon_range)) keep <- keep & df$lon >= min(lon_range) & df$lon <= max(lon_range)
  if (!is.null(lat_range)) keep <- keep & df$lat >= min(lat_range) & df$lat <= max(lat_range)
  if (!is.null(polygon)) keep <- keep & .point_in_polygon(df$lon, df$lat, polygon$lon, polygon$lat)
  df <- df[keep & !is.na(df$value), , drop = FALSE]
  if (!nrow(df)) stop("No grid cells inside the requested domain")
  w <- if ("area" %in% names(df)) df$area else cos(df$lat * pi / 180)
  sp <- split(seq_len(nrow(df)), df$year)
  val <- vapply(sp, function(ix) {
    if (fun == "mean") sum(df$value[ix] * w[ix]) / sum(w[ix]) else sum(df$value[ix])
  }, numeric(1))
  data.frame(year = as.integer(names(sp)), variable = variable, value = unname(val))
}

.point_in_polygon <- function(x, y, px, py) {
  inside <- rep(FALSE, length(x)); j <- length(px)
  for (i in seq_along(px)) {
    cond <- ((py[i] > y) != (py[j] > y)) &
      (x < (px[j] - px[i]) * (y - py[i]) / (py[j] - py[i] + 1e-300) + px[i])
    inside <- xor(inside, cond); j <- i
  }
  inside
}

#' Read a gridded NetCDF variable into a long data frame
#'
#' Thin wrapper around \pkg{ncdf4} for GLORYS / MOM6 / satellite products.
#' Subsets to a bounding box before expanding to limit memory use.
#' @param file path to NetCDF file
#' @param var variable name (e.g., "thetao", "tos", "npp")
#' @param lon_name,lat_name,time_name dimension names
#' @param lon_range,lat_range bounding box
#' @param depth_index index of the depth level to keep for 4-D variables
#' @export
read_gridded_netcdf <- function(file, var, lon_name = "longitude", lat_name = "latitude",
                                time_name = "time", lon_range = NULL, lat_range = NULL,
                                depth_index = 1) {
  if (!requireNamespace("ncdf4", quietly = TRUE)) stop("Package 'ncdf4' is required")
  nc <- ncdf4::nc_open(file); on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, lon_name); lat <- ncdf4::ncvar_get(nc, lat_name)
  tm <- ncdf4::ncvar_get(nc, time_name)
  tunits <- ncdf4::ncatt_get(nc, time_name, "units")$value
  origin <- sub(".*since ", "", tunits)
  step <- sub(" since.*", "", tunits)
  mult <- switch(step, seconds = 1, minutes = 60, hours = 3600, days = 86400, 86400)
  time <- as.POSIXct(origin, tz = "UTC") + tm * mult
  li <- if (is.null(lon_range)) seq_along(lon) else which(lon >= min(lon_range) & lon <= max(lon_range))
  la <- if (is.null(lat_range)) seq_along(lat) else which(lat >= min(lat_range) & lat <= max(lat_range))
  ndim <- nc$var[[var]]$ndims
  start <- c(min(li), min(la)); count <- c(length(li), length(la))
  if (ndim == 4) { start <- c(start, depth_index, 1); count <- c(count, 1, -1) }
  else { start <- c(start, 1); count <- c(count, -1) }
  v <- ncdf4::ncvar_get(nc, var, start = start, count = count, collapse_degen = FALSE)
  dim(v) <- c(length(li), length(la), length(time))
  grid <- expand.grid(lon = lon[li], lat = lat[la], time = time)
  grid$value <- as.vector(v)
  grid
}
