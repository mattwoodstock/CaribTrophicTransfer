# ==============================================================================
# Automated data acquisition (APIs) with user override
# ==============================================================================
# Sources (all public; Copernicus requires a free account):
#   * NOAA ERDDAP griddap  - OISST v2.1 SST, Coral Reef Watch SST/DHW, satellite
#                            NPP (MODIS VGPM, VIIRS SNPP / NOAA-20), chlorophyll ...
#   * THREDDS / OPeNDAP    - MOM6-NWA12 (NOAA CEFI) hindcasts/projections, any CF NetCDF
#   * Copernicus Marine    - GLORYS12 reanalysis via the `copernicusmarine` CLI
#   * NOAA NCCOS ArcGIS    - benthic habitat map areas by class
#   * NOAA NHC HURDAT2     - Atlantic hurricane best tracks -> disturbance events
#
# Acquired data are written to <project>/data/acquired/ and merged by
# read_ttm_project(): values entered by the user in the main CSV files always
# take precedence (blank cells / missing variables are filled from acquired data).
# Every acquired value carries an observation uncertainty (sd or cv) that is
# propagated through the Monte Carlo (see draw_parameters()).
# ==============================================================================

.ua <- "CaribTTM (R package; data-limited fisheries model)"

.http_get <- function(url, tries = 4, timeout = 300) {
  old <- options(timeout = max(timeout, getOption("timeout"))); on.exit(options(old))
  tf <- tempfile()
  for (i in seq_len(tries)) {
    ok <- tryCatch({
      utils::download.file(url, tf, quiet = TRUE, mode = "wb", method = "libcurl",
                           headers = c("User-Agent" = .ua))
      TRUE
    }, error = function(e) { if (i == tries) stop("Download failed: ", url, "\n", conditionMessage(e)); FALSE },
       warning = function(w) { if (i == tries) stop("Download failed: ", url, "\n", conditionMessage(w)); FALSE })
    if (ok) return(tf)
    Sys.sleep(2^i)
  }
}

.bbox <- function(x) {
  if (is.character(x)) x <- as.numeric(strsplit(x, "[,;]")[[1]])
  stopifnot(length(x) == 4)
  c(lon_min = min(x[1], x[3]), lat_min = min(x[2], x[4]), lon_max = max(x[1], x[3]), lat_max = max(x[2], x[4]))
}

#' Project domain bounding box from settings
#' @param proj a `ttm_project`
#' @export
domain_bbox <- function(proj) {
  st <- proj$settings
  b <- c(st$domain_lon_min, st$domain_lat_min, st$domain_lon_max, st$domain_lat_max)
  if (any(is.na(b))) stop("Set domain_lon_min/max and domain_lat_min/max in settings.csv")
  .bbox(b)
}

# ---- spatial-temporal aggregation ------------------------------------------------

#' Aggregate gridded values to an annual regional series with uncertainty
#'
#' Stage 1 averages cells within each time step (cos-latitude area weights).
#' Stage 2 aggregates time steps within each year with `time_agg` (mean, max,
#' min, sum). The returned `sd` is the annual mean of the within-domain spatial
#' standard deviation, i.e. how much the covariate differs across the domain,
#' used as the observation uncertainty of the regional value.
#' @param df data frame with `time` (POSIXct/Date) or `year`, `lat`, `value`
#' @param time_agg aggregation over time steps within a year
#' @param scale,offset linear transform applied to values before aggregation
#' @export
annualize_grid <- function(df, time_agg = c("mean", "max", "min", "sum"), scale = 1, offset = 0) {
  time_agg <- match.arg(time_agg)
  df <- df[is.finite(df$value), , drop = FALSE]
  if (!nrow(df)) return(data.frame(year = integer(), value = numeric(), sd = numeric(), n = integer()))
  df$value <- df$value * scale + offset
  if (!"time" %in% names(df)) df$time <- df$year
  w <- cos(df$lat * pi / 180)
  key <- as.character(df$time)
  sp <- split(seq_len(nrow(df)), key)
  ts <- do.call(rbind, lapply(sp, function(ix) {
    m <- sum(df$value[ix] * w[ix]) / sum(w[ix])
    v <- if (length(ix) > 1) sum(w[ix] * (df$value[ix] - m)^2) / sum(w[ix]) else 0
    data.frame(time = df$time[ix[1]], mean = m, sd = sqrt(v))
  }))
  ts$year <- if (inherits(ts$time, c("Date", "POSIXt"))) as.integer(format(ts$time, "%Y")) else as.integer(floor(ts$time))
  f <- switch(time_agg, mean = mean, max = max, min = min, sum = sum)
  out <- do.call(rbind, lapply(split(ts, ts$year), function(x)
    data.frame(year = x$year[1], value = f(x$mean), sd = mean(x$sd), n = nrow(x))))
  rownames(out) <- NULL
  out
}

# ---- ERDDAP -----------------------------------------------------------------------

#' Build an ERDDAP griddap CSV request URL
#'
#' @param server ERDDAP base URL (e.g., "https://coastwatch.pfeg.noaa.gov/erddap")
#' @param dataset_id griddap dataset id
#' @param variable variable name
#' @param time_range length-2 character/Date (ISO 8601)
#' @param bbox c(lon_min, lat_min, lon_max, lat_max)
#' @param extra_dims named list of single values for dimensions between time
#'   and latitude (e.g., list(zlev = 0) for OISST, list(altitude = 0) for MODIS)
#' @param stride named integer vector (time, lat, lon)
#' @param lat_descending TRUE if the dataset stores latitude north-to-south
#' @export
erddap_griddap_url <- function(server, dataset_id, variable, time_range, bbox, extra_dims = NULL,
                               stride = c(time = 1, lat = 1, lon = 1), lat_descending = FALSE) {
  b <- .bbox(bbox)
  st <- stride; st[is.na(st)] <- 1
  lat <- if (lat_descending) c(b[["lat_max"]], b[["lat_min"]]) else c(b[["lat_min"]], b[["lat_max"]])
  q <- sprintf("[(%s):%d:(%s)]", time_range[1], st[["time"]], time_range[2])
  for (nm in names(extra_dims)) q <- paste0(q, sprintf("[(%s)]", extra_dims[[nm]]))
  q <- paste0(q, sprintf("[(%s):%d:(%s)][(%s):%d:(%s)]", lat[1], st[["lat"]], lat[2],
                         b[["lon_min"]], st[["lon"]], b[["lon_max"]]))
  paste0(sub("/$", "", server), "/griddap/", dataset_id, ".csv?", variable, utils::URLencode(q, reserved = TRUE))
}

#' Parse an ERDDAP griddap CSV response (2 header rows: names, units)
#' @param file path to the downloaded CSV
#' @param variable variable to return as `value`
#' @export
parse_erddap_csv <- function(file, variable) {
  hdr <- utils::read.csv(file, nrows = 1, header = FALSE, stringsAsFactors = FALSE)
  units <- unlist(hdr)
  x <- utils::read.csv(file, skip = 2, header = FALSE, stringsAsFactors = FALSE, na.strings = c("NaN", "NA", ""))
  names(x) <- names(utils::read.csv(file, nrows = 0, check.names = FALSE))
  data.frame(time = as.POSIXct(x$time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
             lat = as.numeric(x$latitude), lon = as.numeric(x$longitude),
             value = as.numeric(x[[variable]]))
}

#' Fetch an annual regional series from an ERDDAP griddap dataset
#'
#' Requests are made one year at a time (with optional strides) and reduced
#' immediately, so long daily records can be processed without large downloads.
#' @inheritParams erddap_griddap_url
#' @param years integer vector of years
#' @param time_agg aggregation within year ("mean", "max", "min", "sum")
#' @param scale,offset transform applied to raw values
#' @param verbose print progress
#' @param chunk_years years per request (use > 1 for monthly products)
#' @return data frame year, value, sd, n
#' @export
fetch_erddap_series <- function(server, dataset_id, variable, years, bbox, extra_dims = NULL,
                                stride = c(time = 1, lat = 1, lon = 1), lat_descending = NA,
                                time_agg = "mean", scale = 1, offset = 0, verbose = TRUE, chunk_years = 1) {
  ts <- fetch_erddap_timeseries(server, dataset_id, variable, years, bbox, extra_dims, stride,
                                chunk_years = chunk_years, scale = scale, offset = offset, verbose = verbose)
  .annual_from_ts(ts, time_agg)
}

#' ERDDAP dataset metadata helpers
#' @param server ERDDAP base URL
#' @param dataset_id dataset id
#' @export
erddap_info <- function(server, dataset_id) {
  f <- .http_get(paste0(sub("/$", "", server), "/info/", dataset_id, "/index.csv"))
  utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
}
#' @rdname erddap_info
#' @export
erddap_time_coverage <- function(server, dataset_id) {
  i <- erddap_info(server, dataset_id)
  g <- function(nm) i$Value[i[["Row Type"]] == "attribute" & i[["Variable Name"]] == "NC_GLOBAL" & i[["Attribute Name"]] == nm]
  as.Date(substr(c(g("time_coverage_start"), g("time_coverage_end")), 1, 10))
}
#' @rdname erddap_info
#' @export
erddap_lat_descending <- function(server, dataset_id) {
  i <- erddap_info(server, dataset_id)
  r <- i$Value[i[["Row Type"]] == "dimension" & i[["Variable Name"]] == "latitude"]
  grepl("averageSpacing=-", r)
}

#' Pre-configured data sources
#'
#' ERDDAP datasets verified against the live services (November 2026).
#' NOAA PIFSC OceanWatch (oceanwatch.pifsc.noaa.gov) hosts monthly Coral Reef
#' Watch SST, CRW DHW, AVHRR Pathfinder SST, ESA OC-CCI ocean colour, SeaWiFS /
#' MODIS PAR and CMIP6 projections (default sources). NESDIS CoastWatch
#' (coastwatch.noaa.gov) hosts daily CRW SST/DHW and VIIRS ocean colour; PFEG
#' CoastWatch (coastwatch.pfeg.noaa.gov) hosts OISST and pre-computed NPP. Use the `dataset_id` / `variable` columns as
#' a template for additional products (any ERDDAP griddap dataset works).
#' @export
ocean_source_catalog <- function() {
  pfeg <- "https://coastwatch.pfeg.noaa.gov/erddap"; nesdis <- "https://coastwatch.noaa.gov/erddap"
  pifsc <- "https://oceanwatch.pifsc.noaa.gov/erddap"
  data.frame(
    key = c("crw_sst_monthly", "pathfinder_sst_monthly", "crw_dhw_max_pifsc", "occci_chla_monthly",
            "occci_kd490_monthly", "crw_sst", "crw_dhw_max", "oisst_sst", "viirs_chla", "viirs_kd490",
            "modis_vgpm_npp", "viirs_snpp_npp", "viirs_noaa20_npp"),
    method = "erddap",
    server = c(pifsc, pifsc, pifsc, pifsc, pifsc, nesdis, nesdis, pfeg, nesdis, nesdis, pfeg, pfeg, pfeg),
    dataset_id = c("CRW_sst_v3_1_monthly", "pf5-3-monthly", "CRW_dhw_v1_0", "esa-cci-chla-monthly-v6-0",
                   "esa-cci-kd-monthly-v6-0", "noaacrwsstDaily", "noaacrwdhwDaily", "ncdcOisst21Agg_LonPM180",
                   "noaacwNPPVIIRSSQchlaMonthly", "noaacwNPPVIIRSSQkd490Monthly", "erdMH1ppmday",
                   "productivity_viirs_snpp_monthly", "productivity_viirs_noaa20_monthly"),
    variable = c("sea_surface_temperature", "sea_surface_temperature", "degree_heating_week", "chlor_a",
                 "kd_490", "analysed_sst", "degree_heating_week", "sst", "chlor_a", "kd_490",
                 "productivity", "productivity", "productivity"),
    extra_dims = c("", "", "", "", "", "", "", "zlev=0", "altitude=0", "altitude=0", "altitude=0", "", ""),
    time_agg = c("mean", "mean", "max", "mean", "mean", "mean", "max", "mean", "mean", "mean", "mean", "mean", "mean"),
    units_out = c("degC", "degC", "degC-weeks", "mg m-3", "m-1", "degC", "degC-weeks", "degC", "mg m-3", "m-1",
                  "mg C m-2 d-1", "mg C m-2 d-1", "mg C m-2 d-1"),
    chunk_years = c(10, 10, 3, 6, 6, 1, 1, 1, 6, 6, 6, 6, 6),
    coverage = c("1985-present monthly 5 km", "1981-2018 monthly 4 km", "1985-present daily 5 km",
                 "1997-present monthly 4 km", "1997-present monthly 4 km",
                 "1985-present daily 5 km", "1985-present daily 5 km", "1981-present daily 0.25 deg",
                 "2012-present monthly 4 km", "2012-present monthly 4 km", "2003-present monthly 4 km",
                 "2012-2024 monthly 4 km", "2018-2025 monthly 4 km"),
    stringsAsFactors = FALSE)
}

#' Satellite NPP ensemble for the pelagic habitat
#'
#' Fetches several satellite NPP products, converts each to g C m-2 yr-1 and
#' combines them per year. The ensemble mean is the value; the ensemble spread
#' (between-product SD, or `min_cv` x mean when only one product covers a year)
#' plus the within-domain spatial SD is the observation uncertainty. This
#' mirrors the multi-model ensemble approach of SEDAR103-DW-20 Appendix A
#' (Ryan-Keogh et al. 2023).
#' @param bbox domain bounding box
#' @param years years
#' @param keys catalog keys of NPP products
#' @param min_cv CV used when a single product is available
#' @param stride spatial stride (cells)
#' @export
fetch_npp_ensemble <- function(bbox, years, keys = c("modis_vgpm_npp", "viirs_snpp_npp", "viirs_noaa20_npp"),
                               min_cv = 0.25, stride = 1) {
  cat <- ocean_source_catalog(); cat <- cat[cat$key %in% keys, ]
  per <- lapply(seq_len(nrow(cat)), function(i) {
    r <- cat[i, ]
    x <- fetch_erddap_series(r$server, r$dataset_id, r$variable, years, bbox,
                             extra_dims = .parse_dims(r$extra_dims), stride = c(time = 1, lat = stride, lon = stride),
                             time_agg = "mean", scale = 365 / 1000)  # mg C m-2 d-1 -> g C m-2 yr-1
    if (nrow(x)) x$product <- r$key
    x
  })
  per <- do.call(rbind, per[vapply(per, nrow, 1L) > 0])
  if (is.null(per) || !nrow(per)) stop("No NPP data returned")
  ens <- do.call(rbind, lapply(split(per, per$year), function(x) {
    m <- mean(x$value)
    between <- if (nrow(x) > 1) stats::sd(x$value) else min_cv * m
    data.frame(year = x$year[1], value = m, sd = sqrt(between^2 + mean(x$sd)^2 / max(1, sum(x$n))),
               n_products = nrow(x), products = paste(x$product, collapse = ";"))
  }))
  attr(ens, "per_product") <- per
  ens
}

.parse_dims <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return(NULL)
  kv <- strsplit(strsplit(x, "[;|]")[[1]], "=")
  setNames(lapply(kv, function(z) z[2]), vapply(kv, `[`, "", 1))
}

# ---- THREDDS / OPeNDAP (MOM6, any CF NetCDF) --------------------------------

#' Fetch an annual regional series from an OPeNDAP / NetCDF source
#'
#' Works with THREDDS OPeNDAP endpoints (e.g., NOAA CEFI MOM6-NWA12 hindcasts
#' and projections at psl.noaa.gov/thredds/dodsC/...) and local NetCDF files
#' (e.g., GLORYS subsets). Requires \pkg{ncdf4} built with OPeNDAP support for
#' remote URLs.
#' @param url OPeNDAP URL or file path
#' @param variable variable name (e.g., "tos", "tob", "thetao", "bottomT")
#' @param bbox domain bounding box
#' @param years years to keep (NULL = all)
#' @param time_agg aggregation within year
#' @param lon_name,lat_name,time_name dimension names (MOM6 regridded: "lon","lat","time")
#' @param lon_0360 TRUE if the source uses 0-360 longitudes
#' @param scale,offset transform (e.g., offset = -273.15 for Kelvin)
#' @export
fetch_opendap_series <- function(url, variable, bbox, years = NULL, time_agg = "mean",
                                 lon_name = "lon", lat_name = "lat", time_name = "time",
                                 lon_0360 = FALSE, scale = 1, offset = 0) {
  b <- .bbox(bbox)
  lr <- c(b[["lon_min"]], b[["lon_max"]]); if (lon_0360) lr <- lr %% 360
  g <- read_gridded_netcdf(url, variable, lon_name, lat_name, time_name,
                           lon_range = lr, lat_range = c(b[["lat_min"]], b[["lat_max"]]))
  names(g)[names(g) == "lat"] <- "lat"
  a <- annualize_grid(g, time_agg, scale, offset)
  if (!is.null(years)) a <- a[a$year %in% years, ]
  a
}

#' Fetch a GLORYS12 (or any Copernicus Marine) series with the copernicusmarine CLI
#'
#' Requires the Copernicus Marine Toolbox (`pip install copernicusmarine`) and
#' credentials in COPERNICUSMARINE_SERVICE_USERNAME / _PASSWORD (or a prior
#' `copernicusmarine login`).
#' @param dataset_id e.g. "cmems_mod_glo_phy_my_0.083deg_P1M-m" (monthly reanalysis)
#' @param variable e.g. "bottomT", "thetao", "so"
#' @param bbox domain bounding box
#' @param years years
#' @param depth_range for 3-D variables (m)
#' @param time_agg aggregation within year
#' @export
fetch_copernicus_series <- function(dataset_id, variable, bbox, years, depth_range = c(0, 1),
                                    time_agg = "mean") {
  exe <- Sys.which("copernicusmarine")
  if (!nzchar(exe)) stop("copernicusmarine CLI not found: pip install copernicusmarine")
  b <- .bbox(bbox); out <- tempfile(fileext = ".nc")
  args <- c("subset", "--dataset-id", dataset_id, "--variable", variable,
            "--minimum-longitude", b[["lon_min"]], "--maximum-longitude", b[["lon_max"]],
            "--minimum-latitude", b[["lat_min"]], "--maximum-latitude", b[["lat_max"]],
            "--start-datetime", sprintf("%d-01-01T00:00:00", min(years)),
            "--end-datetime", sprintf("%d-12-31T23:59:59", max(years)),
            "--minimum-depth", depth_range[1], "--maximum-depth", depth_range[2],
            "--output-directory", dirname(out), "--output-filename", basename(out), "--force-download")
  status <- system2(exe, args)
  if (status != 0 || !file.exists(out)) stop("copernicusmarine subset failed")
  g <- read_gridded_netcdf(out, variable, "longitude", "latitude", "time",
                           lon_range = c(b[["lon_min"]], b[["lon_max"]]), lat_range = c(b[["lat_min"]], b[["lat_max"]]))
  annualize_grid(g, time_agg)
}

# ---- NCCOS benthic habitat maps (ArcGIS REST) ----------------------------------

#' Benthic habitat areas from the NOAA NCCOS benthic habitat map service
#'
#' Sums the polygon `AREA` attribute (m^2, computed by NCCOS in a projected
#' coordinate system) by habitat class for polygons intersecting `bbox`.
#' Layer 6 = U.S. Virgin Islands (2002) habitat boundaries; layer 11 = Puerto Rico (2002).
#' @param bbox bounding box (lon_min, lat_min, lon_max, lat_max)
#' @param layer MapServer layer id
#' @param group_field attribute to group by ("DESCRIPTOR", "HABITAT", "ZONE")
#' @param service MapServer URL
#' @export
fetch_nccos_habitat_areas <- function(bbox, layer = 6, group_field = "DESCRIPTOR",
                                      service = "https://gis.ngdc.noaa.gov/arcgis/rest/services/nccos/BenthicMapping_BenthicHabitats/MapServer") {
  b <- .bbox(bbox)
  stat <- sprintf('[{"statisticType":"sum","onStatisticField":"AREA","outStatisticFieldName":"area_m2"},{"statisticType":"count","onStatisticField":"OBJECTID","outStatisticFieldName":"n_polygons"}]')
  q <- paste0(service, "/", layer, "/query?where=1%3D1",
              "&geometry=", paste(b[c("lon_min", "lat_min", "lon_max", "lat_max")], collapse = ","),
              "&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects",
              "&outStatistics=", utils::URLencode(stat, reserved = TRUE),
              "&groupByFieldsForStatistics=", group_field, "&f=json")
  f <- .http_get(q)
  txt <- paste(readLines(f, warn = FALSE), collapse = "")
  feats <- regmatches(txt, gregexpr('\\{"attributes":\\{[^}]*\\}', txt))[[1]]
  if (!length(feats)) stop("No features returned from NCCOS service: ", substr(txt, 1, 300))
  get <- function(s, key) {
    m <- regmatches(s, regexpr(sprintf('"%s":("[^"]*"|[-0-9.eE+]+|null)', key), s))
    gsub('^"|"$', "", sub(sprintf('"%s":', key), "", m))
  }
  data.frame(class = vapply(feats, get, "", key = group_field),
             area_km2 = as.numeric(vapply(feats, get, "", key = "area_m2")) / 1e6,
             n_polygons = as.integer(vapply(feats, get, "", key = "n_polygons")),
             row.names = NULL, stringsAsFactors = FALSE)
}

#' Default mapping of NCCOS 2002 USVI/PR habitat descriptors to model habitats
#'
#' Regular expressions are matched in order against the DESCRIPTOR attribute.
#' Unmatched classes (e.g., Land, Artificial) are dropped.
#' @export
nccos_class_map <- function() {
  data.frame(
    pattern = c("^Reef/Colonized Pavement", "^Reef/", "^Hardbottom/Uncolonized|^Hardbottom/Reef Rubble",
                "^Seagrass", "^Macroalgae", "^Sand|^Mud", "^Mangrove", "^Unknown"),
    habitat = c("colonized_pavement", "coral_reef", "sand", "seagrass", "macroalgae", "sand",
                "mangrove", "deep_hardbottom"),
    area_cv = c(0.15, 0.15, 0.3, 0.15, 0.25, 0.25, 0.2, 0.5),
    stringsAsFactors = FALSE)
}

#' Map habitat-class areas to model habitats
#' @param areas output of [fetch_nccos_habitat_areas()]
#' @param class_map mapping table with `pattern`, `habitat`, `area_cv`
#' @export
map_habitat_classes <- function(areas, class_map = nccos_class_map()) {
  areas$habitat <- NA_character_; areas$area_cv <- NA_real_
  for (i in seq_len(nrow(class_map))) {
    hit <- is.na(areas$habitat) & grepl(class_map$pattern[i], areas$class)
    areas$habitat[hit] <- class_map$habitat[i]; areas$area_cv[hit] <- class_map$area_cv[i]
  }
  a <- areas[!is.na(areas$habitat), ]
  out <- do.call(rbind, lapply(split(a, a$habitat), function(x)
    data.frame(habitat = x$habitat[1], area_km2 = sum(x$area_km2),
               area_cv = sum(x$area_km2 * x$area_cv) / sum(x$area_km2),
               classes = paste(sprintf("%s (%.2f)", x$class, x$area_km2), collapse = "; "))))
  rownames(out) <- NULL
  out
}

# ---- Hurricanes (HURDAT2) -----------------------------------------------------

#' Download and parse the latest NHC HURDAT2 Atlantic best-track file
#' @param url file URL (default: newest `hurdat2-1851-*.txt` listed at NHC)
#' @export
fetch_hurdat2 <- function(url = NULL) {
  base <- "https://www.nhc.noaa.gov/data/hurdat/"
  if (is.null(url)) {
    idx <- readLines(.http_get(base), warn = FALSE)
    files <- unique(regmatches(idx, regexpr("hurdat2-1851-[0-9]{4}-[0-9]+\\.txt", idx)))
    files <- files[nzchar(files)]
    if (!length(files)) stop("No HURDAT2 files found at ", base)
    # newest by date stamp (MMDDYY or MMDDYYYY)
    stamp <- sub(".*-([0-9]+)\\.txt$", "\\1", files)
    d <- as.Date(ifelse(nchar(stamp) == 8, stamp, paste0(substr(stamp, 1, 4), "20", substr(stamp, 5, 6))), "%m%d%Y")
    url <- paste0(base, files[which.max(d)])
  }
  parse_hurdat2(readLines(.http_get(url), warn = FALSE), source = url)
}

#' Parse HURDAT2 text lines into a track-point data frame
#' @param lines character vector (HURDAT2 format)
#' @param source label
#' @export
parse_hurdat2 <- function(lines, source = NA) {
  out <- vector("list", length(lines)); k <- 0; id <- name <- NA
  for (ln in lines) {
    f <- trimws(strsplit(ln, ",")[[1]])
    if (length(f) >= 3 && grepl("^[A-Z]{2}[0-9]{6}$", f[1])) { id <- f[1]; name <- f[2]; next }
    if (length(f) < 7) next
    lat <- as.numeric(sub("[NS]$", "", f[5])) * ifelse(grepl("S$", f[5]), -1, 1)
    lon <- as.numeric(sub("[EW]$", "", f[6])) * ifelse(grepl("W$", f[6]), -1, 1)
    k <- k + 1
    out[[k]] <- list(id, name, f[1], f[2], f[4], lat, lon, as.numeric(f[7]))
  }
  x <- do.call(rbind.data.frame, c(out[seq_len(k)], stringsAsFactors = FALSE))
  names(x) <- c("storm_id", "name", "date", "time", "status", "lat", "lon", "wind_kt")
  x$year <- as.integer(substr(x$date, 1, 4))
  attr(x, "source") <- source
  x
}

.haversine_km <- function(lon1, lat1, lon2, lat2) {
  r <- pi / 180; dl <- (lon2 - lon1) * r; dp <- (lat2 - lat1) * r
  a <- sin(dp / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin(dl / 2)^2
  6371 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

#' Convert best tracks into disturbance events for a domain
#'
#' For each storm passing within `radius_km` of the domain centre, intensity =
#' wind factor x distance factor, where wind factor = (Vmax - 34)/(137 - 34)
#' (tropical-storm to Category-5 winds, clipped to 0-1) at closest approach and
#' distance factor = 1 inside `core_km`, decaying as exp(-(d - core_km)/decay_km)
#' beyond. Losses are copied from a template event in disturbances.csv and an
#' `intensity_cv` is attached so intensity uncertainty is propagated.
#' @param tracks output of [fetch_hurdat2()]
#' @param bbox domain bounding box
#' @param years years to keep
#' @param template one-row data frame of disturbance losses (from disturbances.csv)
#' @param radius_km,core_km,decay_km distance parameters
#' @param min_intensity drop events below this intensity
#' @param intensity_cv uncertainty attached to each event
#' @export
hurricane_disturbances <- function(tracks, bbox, years, template, radius_km = 200, core_km = 50,
                                   decay_km = 60, min_intensity = 0.05, intensity_cv = 0.3) {
  b <- .bbox(bbox); cx <- mean(b[c("lon_min", "lon_max")]); cy <- mean(b[c("lat_min", "lat_max")])
  tr <- tracks[tracks$year %in% years, ]
  tr$d_km <- .haversine_km(cx, cy, tr$lon, tr$lat)
  tr <- tr[tr$d_km <= radius_km, ]
  if (!nrow(tr)) return(NULL)
  ev <- do.call(rbind, lapply(split(tr, tr$storm_id), function(x) {
    i <- which.min(x$d_km); d <- x$d_km[i]; v <- max(x$wind_kt[x$d_km <= d + 25])
    wf <- min(1, max(0, (v - 34) / (137 - 34)))
    df <- if (d <= core_km) 1 else exp(-(d - core_km) / decay_km)
    data.frame(year = x$year[1], storm = paste0(tools::toTitleCase(tolower(x$name[1])), " (", x$storm_id[1], ")"),
               closest_km = round(d), wind_kt = v, intensity = round(wf * df, 3))
  }))
  ev <- ev[ev$intensity >= min_intensity, ]
  if (!nrow(ev)) return(NULL)
  tmpl <- template[1, , drop = FALSE]
  rows <- lapply(seq_len(nrow(ev)), function(i) {
    r <- tmpl; r$year <- ev$year[i]; r$event <- paste("Hurricane", ev$storm[i])
    r$intensity <- ev$intensity[i]; r$intensity_cv <- intensity_cv
    r$data_status <- "API: NHC HURDAT2"
    r$notes <- sprintf("closest approach %d km; max wind %d kt", ev$closest_km[i], ev$wind_kt[i]); r
  })
  out <- do.call(rbind, rows); attr(out, "events") <- ev
  out
}

# ---- Orchestration --------------------------------------------------------------

#' Acquire all API-available inputs for a project
#'
#' Reads `data_sources.csv` (one row per acquisition task) and writes results to
#' `<project>/data/acquired/`:
#' `env_covariates.csv` (with `sd`), `habitats.csv` (areas with `area_cv`,
#' pelagic NPP), `disturbances.csv` (hurricanes with `intensity_cv`) and
#' `acquisition_log.csv`. Nothing in the user's own input files is modified.
#' Precedence applied by [read_ttm_project()]:
#' * covariates: user rows win for any (variable, year) they supply, except
#'   rows whose `data_status` contains "PLACEHOLDER", which acquired data replace;
#' * habitats: acquired area / NPP fill only blank `area_km2` / `npp_value` cells;
#' * disturbances: acquired events are added only for years the user did not list.
#'
#' `data_sources.csv` columns: `target`, `method`, `enabled`, and method
#' arguments in `args` as `key=value` pairs separated by `;`.
#' Supported methods: `erddap`, `npp_ensemble`, `opendap`, `copernicus`,
#' `nccos_habitat`, `hurdat2`.
#' @param project_dir project directory
#' @param only optional subset of targets
#' @param verbose print progress
#' @export
acquire_project_data <- function(project_dir, only = NULL, verbose = TRUE) {
  ddir <- if (dir.exists(file.path(project_dir, "data"))) file.path(project_dir, "data") else project_dir
  proj <- read_ttm_project(project_dir, use_acquired = FALSE, validate = FALSE)
  src <- .read_csv(file.path(ddir, "data_sources.csv"))
  src <- .fill_col(src, "enabled", TRUE); src <- src[as.logical(src$enabled), , drop = FALSE]
  if (!is.null(only)) src <- src[src$target %in% only, , drop = FALSE]
  adir <- file.path(ddir, "acquired"); dir.create(adir, showWarnings = FALSE)
  rd <- function(f) { p <- file.path(adir, f); if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE) else NULL }
  cov <- rd("env_covariates.csv"); hab <- rd("habitats.csv"); dist <- rd("disturbances.csv")
  logs <- rd("acquisition_log.csv")
  years <- seq(proj$settings$hist_start, proj$settings$hist_end)
  for (i in seq_len(nrow(src))) {
    r <- src[i, ]; a <- .parse_args(r$args)
    bbox <- if (!is.null(a$bbox)) .bbox(a$bbox) else domain_bbox(proj)
    yrs <- if (!is.null(a$years)) eval(parse(text = a$years)) else years
    if (verbose) message("Acquiring ", r$target, " via ", r$method)
    res <- tryCatch(switch(r$method,
      erddap = {
        cat <- ocean_source_catalog()
        cc <- if (!is.null(a$key)) cat[cat$key == a$key, ] else NULL
        x <- fetch_erddap_series(
          server = .pick(a$server, cc$server, "https://coastwatch.pfeg.noaa.gov/erddap"),
          dataset_id = .pick(a$dataset_id, cc$dataset_id), variable = .pick(a$variable, cc$variable),
          years = yrs, bbox = bbox, extra_dims = .parse_dims(.pick(a$extra_dims, cc$extra_dims, "")),
          stride = c(time = as.integer(.pick(a$stride_time, 1)), lat = as.integer(.pick(a$stride_space, 1)),
                     lon = as.integer(.pick(a$stride_space, 1))),
          time_agg = .pick(a$time_agg, cc$time_agg, "mean"), scale = as.numeric(.pick(a$scale, 1)),
          offset = as.numeric(.pick(a$offset, 0)), verbose = verbose,
          chunk_years = as.integer(.pick(a$chunk_years, cc$chunk_years, 1)))
        if (!is.null(a$fill_key)) {   # fill earlier/missing years from a second product (e.g., Pathfinder before CRW)
          c2 <- cat[cat$key == a$fill_key, ]
          x2 <- fetch_erddap_series(c2$server, c2$dataset_id, c2$variable, setdiff(yrs, x$year), bbox,
                                    .parse_dims(c2$extra_dims), stride = c(time = 1, lat = 2, lon = 2),
                                    time_agg = c2$time_agg, chunk_years = c2$chunk_years, verbose = verbose)
          if (nrow(x2)) {
            x2$sd <- pmax(x2$sd, stats::sd(x$value) * 0.25)
            x <- rbind(x2, x); x <- x[order(x$year), ]
          }
        }
        list(cov = data.frame(year = x$year, variable = r$target, value = x$value, sd = x$sd,
                              units = .pick(a$units, cc$units_out, ""),
                              source = paste("ERDDAP", .pick(a$dataset_id, cc$dataset_id)),
                              data_status = "API"))
      },
      npp_ensemble = {
        keys <- if (!is.null(a$keys)) strsplit(a$keys, "\\|")[[1]] else c("modis_vgpm_npp", "viirs_snpp_npp", "viirs_noaa20_npp")
        e <- tryCatch(fetch_npp_ensemble(bbox, yrs, keys, stride = as.integer(.pick(a$stride_space, 1))),
                      error = function(err) NULL)
        if (is.null(e) && !is.null(a$fallback_key)) {
          # Fallback: chlorophyll-based relative productivity proxy (sd doubled)
          warning("NPP products unavailable; using ", a$fallback_key, " as a relative productivity proxy")
          cc <- ocean_source_catalog(); cc <- cc[cc$key == a$fallback_key, ]
          x <- fetch_erddap_series(cc$server, cc$dataset_id, cc$variable, yrs, bbox, .parse_dims(cc$extra_dims),
                                   stride = c(time = 1, lat = as.integer(.pick(a$stride_space, 1)),
                                              lon = as.integer(.pick(a$stride_space, 1))))
          b0 <- mean(x$value)
          return_list <- list(cov = data.frame(year = x$year, variable = r$target, value = x$value / b0,
                                               sd = 2 * x$sd / b0, units = "relative",
                                               source = paste("Chlorophyll proxy:", cc$dataset_id),
                                               data_status = "API (proxy)"))
          return_list
        } else {
        if (is.null(e)) stop("No NPP products could be retrieved")
        base <- mean(e$value)
        hb <- .pick(a$habitat, "pelagic")
        list(cov = rbind(
               data.frame(year = e$year, variable = paste0(r$target), value = e$value / base, sd = e$sd / base,
                          units = "relative", source = paste("NPP ensemble:", e$products), data_status = "API"),
               data.frame(year = e$year, variable = paste0(r$target, "_abs"), value = e$value, sd = e$sd,
                          units = "g C m-2 yr-1", source = paste("NPP ensemble:", e$products), data_status = "API")),
             hab = data.frame(habitat = hb, area_km2 = NA, area_cv = NA, npp_value = round(base, 2),
                              npp_unit = "g_C_m2_yr", npp_cv = round(stats::sd(e$value) / base + 0.1, 3),
                              classes = "", source = "API: satellite NPP ensemble (ERDDAP)"))
        }
      },
      vgpm_ensemble = {
        e <- fetch_vgpm_ensemble(bbox, yrs, verbose = verbose)
        base <- mean(e$value)
        # index uncertainty: spread of each algorithm's own anomaly + spatial chl component
        iv <- e$vgpm / mean(e$vgpm); ie <- e$eppley / mean(e$eppley)
        mon <- attr(e, "monthly")
        sp <- tapply(mon$rel_sd_chl, mon$year, mean)[as.character(e$year)] / sqrt(e$n_months)
        idx_sd <- sqrt(apply(cbind(iv, ie), 1, stats::sd)^2 + sp^2)
        utils::write.csv(attr(e, "monthly"), file.path(adir, "vgpm_monthly_inputs.csv"), row.names = FALSE)
        list(cov = rbind(
               data.frame(year = e$year, variable = r$target, value = (iv + ie) / 2, sd = as.numeric(idx_sd),
                          units = "relative", source = paste("Computed:", e$products), data_status = "API (computed)"),
               data.frame(year = e$year, variable = paste0(r$target, "_abs"), value = e$value, sd = e$sd,
                          units = "g C m-2 yr-1", source = paste("Computed:", e$products), data_status = "API (computed)")),
             hab = data.frame(habitat = .pick(a$habitat, "pelagic"), area_km2 = NA, area_cv = NA,
                              npp_value = round(base, 2), npp_unit = "g_C_m2_yr",
                              npp_cv = round(sqrt(mean(e$sd^2)) / base + 0.1, 3), classes = "",
                              source = "API: computed VGPM/Eppley-VGPM ensemble"))
      },
      cmip6_delta = {
        if (is.null(cov) || !a$obs_variable %in% cov$variable)
          stop("cmip6_delta needs the observed covariate '", a$obs_variable, "' acquired first (list it earlier in data_sources.csv)")
        obs <- cov[cov$variable == a$obs_variable, c("year", "value")]
        exps <- strsplit(.pick(a$experiments, "ssp245|ssp585"), "\\|")[[1]]
        mods <- strsplit(.pick(a$models, "cesm2|access-esm1-5"), "\\|")[[1]]
        py <- seq(max(obs$year) + 1, as.integer(.pick(a$proj_end, proj$settings$proj_end)))
        bl <- eval(parse(text = .pick(a$baseline, "2005:2014")))
        do.call(rbind, lapply(exps, function(ex) {
          pr <- cmip6_delta_projection(obs, mods, ex, a$cmip_var, bbox, py, bl, .pick(a$type, "additive"),
                                       as.numeric(.pick(a$scale, 1)))
          rbind(data.frame(year = obs$year, variable = paste0(a$obs_variable, "_", ex), value = obs$value,
                           sd = cov$sd[cov$variable == a$obs_variable], units = "", source = "observed",
                           data_status = "API"),
                data.frame(year = pr$year, variable = paste0(a$obs_variable, "_", ex), value = pr$value, sd = pr$sd,
                           units = "", source = paste("CMIP6 delta:", pr$models, ex), data_status = "API (CMIP6)"))
        })) -> cv
        list(cov = cv)
      },
      opendap = {
        x <- fetch_opendap_series(a$url, a$variable, bbox, yrs, .pick(a$time_agg, "mean"),
                                  .pick(a$lon_name, "lon"), .pick(a$lat_name, "lat"), .pick(a$time_name, "time"),
                                  isTRUE(as.logical(.pick(a$lon_0360, FALSE))),
                                  as.numeric(.pick(a$scale, 1)), as.numeric(.pick(a$offset, 0)))
        list(cov = data.frame(year = x$year, variable = r$target, value = x$value, sd = x$sd,
                              units = .pick(a$units, ""), source = paste("OPeNDAP", a$url), data_status = "API"))
      },
      copernicus = {
        x <- fetch_copernicus_series(a$dataset_id, a$variable, bbox, yrs,
                                     as.numeric(strsplit(.pick(a$depth_range, "0,1"), ",")[[1]]), .pick(a$time_agg, "mean"))
        list(cov = data.frame(year = x$year, variable = r$target, value = x$value, sd = x$sd,
                              units = .pick(a$units, ""), source = paste("Copernicus", a$dataset_id), data_status = "API"))
      },
      nccos_habitat = {
        ar <- fetch_nccos_habitat_areas(bbox, layer = as.integer(.pick(a$layer, 6)), group_field = .pick(a$group_field, "DESCRIPTOR"))
        cm <- if (!is.null(a$class_map)) utils::read.csv(file.path(ddir, a$class_map), stringsAsFactors = FALSE) else nccos_class_map()
        mh <- map_habitat_classes(ar, cm)
        utils::write.csv(ar, file.path(adir, "nccos_habitat_classes.csv"), row.names = FALSE)
        list(hab = data.frame(habitat = mh$habitat, area_km2 = round(mh$area_km2, 3), area_cv = mh$area_cv,
                              npp_value = NA, npp_unit = NA, npp_cv = NA, classes = mh$classes,
                              source = "API: NOAA NCCOS benthic habitat map"))
      },
      hurdat2 = {
        tr <- fetch_hurdat2(a$url)
        tmpl <- proj$disturbances[proj$disturbances$event == .pick(a$template, "major_hurricane"), , drop = FALSE]
        if (!nrow(tmpl)) stop("Template event not found in disturbances.csv: ", .pick(a$template, "major_hurricane"))
        ev <- hurricane_disturbances(tr, bbox, yrs, tmpl, radius_km = as.numeric(.pick(a$radius_km, 200)),
                                     core_km = as.numeric(.pick(a$core_km, 50)), decay_km = as.numeric(.pick(a$decay_km, 60)),
                                     intensity_cv = as.numeric(.pick(a$intensity_cv, 0.3)))
        extra <- NULL
        if (!is.null(a$template_other)) {
          t2 <- proj$disturbances[proj$disturbances$event == a$template_other, , drop = FALSE]
          if (nrow(t2)) {
            extra <- hurricane_disturbances(tr, bbox, yrs, t2, radius_km = as.numeric(.pick(a$radius_km, 200)),
                                            core_km = as.numeric(.pick(a$core_km, 50)), decay_km = as.numeric(.pick(a$decay_km, 60)),
                                            intensity_cv = as.numeric(.pick(a$intensity_cv, 0.3)))
            if (!is.null(extra)) extra$event <- paste(extra$event, "(other gears)")
          }
        }
        list(dist = rbind(ev, extra))
      },
      stop("Unknown acquisition method: ", r$method)),
      error = function(e) { warning(sprintf("Acquisition of %s failed: %s", r$target, conditionMessage(e))); NULL })
    status <- if (is.null(res)) "FAILED" else "ok"
    if (!is.null(res$cov)) {
      if (!is.null(cov)) cov <- cov[!cov$variable %in% unique(res$cov$variable), , drop = FALSE]
      cov <- rbind(cov, res$cov)
    }
    if (!is.null(res$hab)) {
      if (!is.null(hab)) {
        keep <- hab[!hab$habitat %in% res$hab$habitat, , drop = FALSE]
        upd <- hab[hab$habitat %in% res$hab$habitat, , drop = FALSE]
        for (j in seq_len(nrow(res$hab))) {
          k <- match(res$hab$habitat[j], upd$habitat)
          if (!is.na(k)) for (nm in names(res$hab)) if (!is.na(res$hab[[nm]][j]) && res$hab[[nm]][j] != "") upd[[nm]][k] <- res$hab[[nm]][j]
        }
        hab <- rbind(keep, upd, res$hab[!res$hab$habitat %in% upd$habitat, , drop = FALSE])
      } else hab <- res$hab
    }
    if (!is.null(res$dist)) dist <- res$dist
    logs <- rbind(logs, data.frame(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), target = r$target,
                                   method = r$method, args = r$args, status = status))
  }
  wr <- function(x, f) if (!is.null(x)) utils::write.csv(x, file.path(adir, f), row.names = FALSE, na = "")
  wr(cov, "env_covariates.csv"); wr(hab, "habitats.csv"); wr(dist, "disturbances.csv"); wr(logs, "acquisition_log.csv")
  invisible(list(env_covariates = cov, habitats = hab, disturbances = dist, log = logs))
}

.pick <- function(...) {
  for (x in list(...)) if (!is.null(x) && length(x) && !is.na(x[1]) && x[1] != "") return(x[1])
  NULL
}

.parse_args <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(list())
  parts <- strsplit(x, ";(?=[A-Za-z_]+=)", perl = TRUE)[[1]]
  kv <- regmatches(parts, regexpr("=", parts), invert = TRUE)
  setNames(lapply(kv, function(z) trimws(z[2])), vapply(kv, function(z) trimws(z[1]), ""))
}

# ---- Merge acquired data with user inputs (user overrides) -----------------------

.merge_acquired <- function(proj, adir) {
  rd <- function(f) { p <- file.path(adir, f); if (file.exists(p)) .read_csv(p, FALSE) else NULL }
  prov <- list()
  # covariates: user rows win for any (variable, year) they supply
  ac <- rd("env_covariates.csv")
  if (!is.null(ac)) {
    uc <- proj$env_covariates
    if (is.null(uc)) uc <- ac[0, ]
    # user rows flagged PLACEHOLDER are replaced by acquired data for the same variable
    if ("data_status" %in% names(uc)) {
      ph <- grepl("PLACEHOLDER", uc$data_status, ignore.case = TRUE) & uc$variable %in% ac$variable
      uc <- uc[!ph, , drop = FALSE]
    }
    key_u <- paste(uc$variable, uc$year); key_a <- paste(ac$variable, ac$year)
    add <- ac[!key_a %in% key_u, , drop = FALSE]
    for (nm in setdiff(union(names(uc), names(add)), names(uc))) uc[[nm]] <- if (nrow(uc)) NA else logical(0)
    for (nm in setdiff(names(uc), names(add))) add[[nm]] <- if (nrow(add)) NA else logical(0)
    proj$env_covariates <- rbind(uc, add[, names(uc), drop = FALSE])
    prov$env_covariates <- sprintf("%d acquired rows added (%s); %d user rows kept",
                                   nrow(add), paste(unique(add$variable), collapse = ", "), nrow(uc) - 0)
  }
  # habitats: fill blank area_km2 / npp_value from acquired (user values win)
  ah <- rd("habitats.csv")
  if (!is.null(ah)) {
    h <- proj$habitats; filled <- character(0)
    for (j in seq_len(nrow(ah))) {
      k <- match(ah$habitat[j], h$habitat); if (is.na(k)) next
      if (is.na(h$area_km2[k]) && !is.na(ah$area_km2[j])) {
        h$area_km2[k] <- ah$area_km2[j]
        if (!"area_cv" %in% names(h)) h$area_cv <- NA
        if (is.na(h$area_cv[k])) h$area_cv[k] <- ah$area_cv[j]
        filled <- c(filled, paste0(h$habitat[k], ":area"))
      }
      if (is.na(h$npp_value[k]) && !is.na(ah$npp_value[j])) {
        h$npp_value[k] <- ah$npp_value[j]; h$npp_unit[k] <- ah$npp_unit[j]
        if (!"npp_cv" %in% names(h)) h$npp_cv <- NA
        if (is.na(h$npp_cv[k])) h$npp_cv[k] <- ah$npp_cv[j]
        filled <- c(filled, paste0(h$habitat[k], ":npp"))
      }
    }
    proj$habitats <- h
    prov$habitats <- paste("filled from acquired:", paste(filled, collapse = ", "))
  }
  # disturbances: acquired events used only for years the user did not specify
  ad <- rd("disturbances.csv")
  if (!is.null(ad) && isTRUE(proj$settings$use_acquired_disturbances %in% c(TRUE, NA) || is.null(proj$settings$use_acquired_disturbances))) {
    ud <- proj$disturbances
    if (is.null(ud)) ud <- ad[0, ]
    user_years <- unique(ud$year[!is.na(ud$year)])
    add <- ad[!ad$year %in% user_years, , drop = FALSE]
    for (nm in setdiff(names(add), names(ud))) ud[[nm]] <- if (nrow(ud)) NA else logical(0)
    for (nm in setdiff(names(ud), names(add))) add[[nm]] <- if (nrow(add)) NA else logical(0)
    proj$disturbances <- rbind(ud, add[, names(ud), drop = FALSE])
    prov$disturbances <- sprintf("%d acquired events added (user-specified years kept: %s)", nrow(add),
                                 paste(user_years, collapse = ", "))
  }
  proj$provenance <- prov
  proj
}
