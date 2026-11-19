# ==============================================================================
# Project input files: reading, defaults and validation
# ==============================================================================
# A project is a directory containing CSV files (see docs/input_dictionary.md):
#   settings.csv            key,value[,notes]               REQUIRED
#   habitats.csv            one row per habitat              REQUIRED
#   groups.csv              one row per functional group     REQUIRED
#   diet.csv                group x energy-pool fractions    REQUIRED
#   habitat_preference.csv  group x benthic habitat affinity REQUIRED
#   gears.csv               gear list                        optional
#   landings.csv            year,gear,group,landings_lb      optional
#   effort.csv              year,gear,effort                 optional
#   env_covariates.csv      year,variable,value              optional
#   env_response.csv        group,variable,type,...          optional
#   disturbances.csv        year,event,...                   optional
#   indices.csv             year,index,group,value,cv        optional (SIR)
#   benchmarks.csv          group,metric,value_lb,...        optional
#   scenarios.csv           scenario,driver,target,...       optional
# ==============================================================================

#' Energy pool names used by the diet matrix
#'
#' B_ = benthic pathway, P_ = pelagic pathway, TL1-TL4 = trophic level of the
#' prey pool (TL1 = primary producers + detritus).
#' @export
ttm_pools <- function() c(paste0("B_TL", 1:4), paste0("P_TL", 1:4))

#' Default model settings
#'
#' Every setting can be overridden in `settings.csv`.
#' @export
ttm_default_settings <- function() {
  list(
    project_name          = "TTM project",
    hist_start            = 2000,
    hist_end              = 2024,
    proj_end              = 2045,
    ref_years             = 3,      # years at end of history used for reference points
    substeps              = 12,     # sub-annual integration steps for biomass dynamics
    c_to_ww               = 9,      # g WW per g C (Pauly & Christensen 1995)
    c_to_ww_cv            = 0.1,
    n_draws               = 500,
    seed                  = 1,
    diet_concentration    = 50,     # Dirichlet concentration (0 = no diet uncertainty)
    catch_expansion       = 1.0,    # multiplier on reported landings (under-reporting)
    catch_expansion_cv    = 0.0,
    fishing_feedback      = TRUE,   # remove last-year catch from TL pools
    pstar                 = 0.40,   # percentile of MMSY used as target (paper: 40%)
    gear_alloc_method     = "catch_share",  # or "slide" (SEDAR103 slide 11)
    q_years               = 3,      # years used to estimate catchability
    eu_safe               = 0.60,   # energy-utilization thresholds
    eu_caution            = 0.90,
    coral_growth          = 0.10,   # intrinsic coral cover growth (yr^-1)
    coral_max_cover       = 0.30,   # carrying capacity of coral cover (proportion)
    bleach_dhw_threshold  = 4,      # DHW (deg C-weeks) above which mortality begins
    bleach_mort_per_dhw   = 0.03,   # proportional cover loss per DHW above threshold
    bleach_mort_cv        = 0.3,
    sir_resample          = 0,      # 0 = no SIR; otherwise number of resamples
    sir_adaptive_rounds   = 0,      # adaptive importance-sampling rounds (proposal refit on partition priors)
    sir_feasibility_groups = "all", # groups constraining calibration: all, trap, or a ;-separated list
    cores                 = 1,      # parallel cores for Stage-1 draws (Unix; mclapply)
    sir_catch_feasibility = TRUE,   # SRA-style penalty when landings exceed what the stock supports
    catch_feasibility_cv  = 0.1,
    bbmsy_overfished      = 0.5,    # B/BMSY below which a group is flagged Overfished
    index_min_cv          = 0.2,
    landings_cv           = 0,      # default observation CV of each landings record
    effort_cv             = 0,      # default observation CV of each effort record
    pref_cv               = 0,      # CV on habitat preferences
    coral_growth_cv       = 0,
    disturbance_loss_cv   = 0,      # CV of a common multiplier on disturbance loss magnitudes
    holdout_from_year     = NA,     # index observations from this year onward are held out (validation)
    ryther_low            = 1,      # Ryther index thresholds (t km-2 yr-1): practical limit ~1, extreme 3 (Link & Watson 2019)
    ryther_high           = 3,
    fogarty_low           = 0.22,   # Fogarty ratio thresholds (per mil; Link & Watson 2019)
    fogarty_high          = 0.92,
    use_acquired          = TRUE,   # merge API-acquired data (data/acquired/) where user left blanks
    domain_lon_min        = NA,     # domain bounding box used by data acquisition
    domain_lon_max        = NA,
    domain_lat_min        = NA,
    domain_lat_max        = NA,
    effort_creep          = 0,      # annual catchability increase applied to fishery-dependent (CPUE) indices
    effort_response       = 0,      # elasticity of projected effort to catch rate (0 = effort set by scenario only)
    ghost_efficiency      = 0,      # catch rate of a lost (ghost) trap relative to an active trap
    ghost_efficiency_cv   = 0.5,
    ghost_half_life       = 1.5,    # years for half of lost traps to stop fishing
    ghost_gears           = NA,     # ;-separated trap gears that ghost-fish when lost
    ghost_background_loss = 0,      # annual fraction of traps lost outside storms (chronic ghost fishing)
    herbivore_groups      = NA,     # ;-separated grazers whose biomass controls coral recovery
    herbivory_elasticity  = 0,      # gamma in coral growth x (grazing / reference)^gamma (projections)
    herbivory_elasticity_cv = 0.5,
    psis                  = TRUE    # Pareto-smoothed importance weights (Vehtari et al. 2024)
  )
}

.as_setting <- function(x) {
  if (is.na(x) || x == "") return(NA)
  if (toupper(x) %in% c("TRUE", "FALSE")) return(as.logical(toupper(x)))
  n <- suppressWarnings(as.numeric(x))
  if (!is.na(n)) return(n)
  x
}

.read_csv <- function(path, required = TRUE) {
  if (!file.exists(path)) {
    if (required) stop("Required input file not found: ", path)
    return(NULL)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                        comment.char = "#", strip.white = TRUE, na.strings = c("NA", ""))
  names(df) <- trimws(names(df))
  df
}

.fill_col <- function(df, col, default) {
  if (!col %in% names(df)) df[[col]] <- default
  df[[col]][is.na(df[[col]])] <- default
  df
}

#' Read a TTM project directory
#'
#' @param dir directory containing the project CSV files (or a `data/`
#'   sub-directory containing them).
#' @param use_acquired merge API-acquired data from `data/acquired/`
#'   ([acquire_project_data()]) where the user left blanks. Default:
#'   `settings$use_acquired` (TRUE). User-entered values always take precedence.
#' @param validate validate and complete the project (FALSE returns raw tables).
#' @return a `ttm_project` list.
#' @export
read_ttm_project <- function(dir, use_acquired = NULL, validate = TRUE) {
  if (dir.exists(file.path(dir, "data")) && !file.exists(file.path(dir, "settings.csv")))
    dir <- file.path(dir, "data")
  f <- function(x, req = FALSE) .read_csv(file.path(dir, x), required = req)

  st <- ttm_default_settings()
  s_df <- f("settings.csv", TRUE)
  for (i in seq_len(nrow(s_df))) st[[s_df$key[i]]] <- .as_setting(as.character(s_df$value[i]))

  proj <- list(
    dir = normalizePath(dir),
    settings = st,
    habitats = f("habitats.csv", TRUE),
    groups = f("groups.csv", TRUE),
    diet = f("diet.csv", TRUE),
    habitat_preference = f("habitat_preference.csv", TRUE),
    gears = f("gears.csv"),
    landings = f("landings.csv"),
    effort = f("effort.csv"),
    env_covariates = f("env_covariates.csv"),
    env_response = f("env_response.csv"),
    disturbances = f("disturbances.csv"),
    indices = f("indices.csv"),
    benchmarks = f("benchmarks.csv"),
    scenarios = f("scenarios.csv")
  )
  class(proj) <- "ttm_project"
  if (is.null(use_acquired)) use_acquired <- !isFALSE(st$use_acquired)
  adir <- file.path(dir, "acquired")
  if (use_acquired && dir.exists(adir)) proj <- .merge_acquired(proj, adir)
  if (validate) validate_ttm_project(proj) else proj
}

#' Validate and complete a TTM project
#'
#' Fills optional columns with defaults, drops excluded groups, checks diet
#' rows sum to one, and aligns habitat-preference and diet tables with groups.
#' @param proj a `ttm_project` (list) as returned by [read_ttm_project()] or
#'   assembled manually.
#' @export
validate_ttm_project <- function(proj) {
  st <- proj$settings
  # ---- habitats -------------------------------------------------------------
  h <- proj$habitats
  need <- c("habitat", "type", "area_km2", "npp_value", "npp_unit")
  miss <- setdiff(need, names(h))
  if (length(miss)) stop("habitats.csv missing columns: ", paste(miss, collapse = ", "))
  if (!all(h$type %in% c("benthic", "pelagic"))) stop("habitats$type must be 'benthic' or 'pelagic'")
  if (sum(h$type == "pelagic") > 1) stop("Only one pelagic habitat row is supported")
  for (cc in c("area_km2", "npp_value"))
    if (any(is.na(h[[cc]]))) stop("habitats.csv: ", cc, " is blank for ", paste(h$habitat[is.na(h[[cc]])], collapse = ", "),
                                  ". Enter a value or run acquire_project_data() to fill it from an API.")
  for (cc in list(c("area_cv", 0), c("npp_cv", 0.3), c("food_web_fraction", 1),
                  c("food_web_fraction_cv", 0), c("tte", 0.10), c("tte_cv", 0.25),
                  c("quality_covariate", NA), c("quality_ref", 1),
                  c("quality_model", "none"), c("p1_quality_elasticity", 0),
                  c("npp_covariate", NA), c("init_quality", 1), c("closed_fraction", 0)))
    h <- .fill_col(h, cc[1], if (is.na(cc[2])) NA else
      if (!is.na(suppressWarnings(as.numeric(cc[2])))) as.numeric(cc[2]) else cc[2])
  h$npp_t_C_km2_yr <- convert_productivity(h$npp_value, h$npp_unit)
  proj$habitats <- h
  benthic <- h$habitat[h$type == "benthic"]

  # ---- groups ---------------------------------------------------------------
  g <- proj$groups
  need <- c("group", "pb")
  miss <- setdiff(need, names(g))
  if (length(miss)) stop("groups.csv missing columns: ", paste(miss, collapse = ", "))
  g <- .fill_col(g, "include", TRUE)
  g$include <- as.logical(g$include)
  g <- g[g$include, , drop = FALSE]
  defaults <- list(group_type = "fmp", fmp_complex = NA, pb_cv = 0.2, x_msy = 0.5,
                   x_msy_cv = 0.2, partition_method = "uniform_pb", partition_value = 1,
                   partition_cv = 0.3, B0_fixed_t = NA, B0_fixed_cv = 0.3,
                   init_depletion = 0.6, init_depletion_cv = 0.2, refuge_dependence = 0,
                   immigration_t = 0, immigration_cv = 0, process_sd = 0.1, energy_density_kJ_g = 5,
                   harvest_allowed = TRUE, trap_relevant = FALSE, data_tier = 3,
                   m_nat = NA, m_nat_cv = 0.25, bmsy_k = 0.5, open_recruitment = 0, open_recruitment_cv = 0.25,
                   recruit_covariate = NA, nursery_habitat = NA, nursery_dependence = 0)
  for (nm in names(defaults)) g <- .fill_col(g, nm, defaults[[nm]])
  g$harvest_allowed <- as.logical(g$harvest_allowed)
  g$trap_relevant <- as.logical(g$trap_relevant)
  ok_methods <- c("uniform_pb", "biomass", "catch_proportion", "landings_proportion",
                  "weight", "fixed_biomass")
  bad <- setdiff(unique(g$partition_method), ok_methods)
  if (length(bad)) stop("Unknown partition_method: ", paste(bad, collapse = ", "))
  if (any(g$partition_method == "fixed_biomass" & is.na(g$B0_fixed_t)))
    stop("partition_method 'fixed_biomass' requires B0_fixed_t")
  rownames(g) <- g$group
  proj$groups <- g

  # ---- diet -----------------------------------------------------------------
  d <- proj$diet
  pools <- ttm_pools()
  for (p in pools) if (!p %in% names(d)) d[[p]] <- 0
  d[is.na(d)] <- 0
  missing_g <- setdiff(g$group, d$group)
  if (length(missing_g)) stop("diet.csv missing groups: ", paste(missing_g, collapse = ", "))
  d <- d[match(g$group, d$group), , drop = FALSE]
  D <- as.matrix(d[, pools]); rownames(D) <- g$group
  rs <- rowSums(D)
  if (any(rs <= 0)) stop("Diet rows must have positive sums: ", paste(names(rs)[rs <= 0], collapse = ", "))
  if (any(abs(rs - 1) > 0.02))
    warning("Diet rows not summing to 1 were normalized: ",
            paste(names(rs)[abs(rs - 1) > 0.02], collapse = ", "))
  proj$diet_matrix <- D / rs

  # ---- habitat preference ---------------------------------------------------
  hp <- proj$habitat_preference
  for (hb in benthic) if (!hb %in% names(hp)) hp[[hb]] <- 0
  hp[is.na(hp)] <- 0
  missing_g <- setdiff(g$group, hp$group)
  if (length(missing_g)) {
    # groups without benthic diet do not need preferences
    need_pref <- missing_g[rowSums(proj$diet_matrix[missing_g, grep("^B_", pools), drop = FALSE]) > 0]
    if (length(need_pref)) stop("habitat_preference.csv missing groups with benthic diet: ",
                                paste(need_pref, collapse = ", "))
    add <- as.data.frame(matrix(0, length(missing_g), length(benthic),
                                dimnames = list(NULL, benthic)))
    hp <- rbind(hp[, c("group", benthic)], cbind(group = missing_g, add))
  }
  hp <- hp[match(g$group, hp$group), , drop = FALSE]
  HP <- as.matrix(hp[, benthic, drop = FALSE]); storage.mode(HP) <- "double"
  rownames(HP) <- g$group
  bdiet <- rowSums(proj$diet_matrix[, grep("^B_", pools), drop = FALSE])
  if (any(bdiet > 0 & rowSums(HP) <= 0))
    stop("Groups with benthic diet need at least one positive habitat preference: ",
         paste(g$group[bdiet > 0 & rowSums(HP) <= 0], collapse = ", "))
  proj$pref_matrix <- HP

  # ---- optional tables ------------------------------------------------------
  if (!is.null(proj$landings)) {
    proj$landings <- proj$landings[proj$landings$group %in% g$group, , drop = FALSE]
    proj$landings <- .fill_col(proj$landings, "landings_lb", 0)
  }
  if (!is.null(proj$gears)) proj$gear_names <- proj$gears$gear
  else if (!is.null(proj$landings)) proj$gear_names <- sort(unique(proj$landings$gear))
  else proj$gear_names <- character(0)
  if (!is.null(proj$env_response)) {
    er <- proj$env_response
    er <- er[er$group %in% g$group | er$group == "all", , drop = FALSE]
    for (cc in c("baseline", "sensitivity", "t_opt", "t_width", "threshold"))
      er <- .fill_col(er, cc, NA_real_)
    er <- .fill_col(er, "sensitivity_cv", 0)
    proj$env_response <- er
  }
  if (!is.null(proj$disturbances)) {
    dd <- proj$disturbances
    defs <- list(habitat = "all_benthic", intensity = 1, p1_loss = 0, quality_loss = 0,
                 half_life_yr = 1, gear = NA, effort_loss = 0, effort_half_life_yr = 1,
                 group = NA, production_loss = 0)
    for (nm in names(defs)) dd <- .fill_col(dd, nm, defs[[nm]])
    proj$disturbances <- dd
  }
  if (!is.null(proj$indices)) {
    ix <- .fill_col(proj$indices, "cv", st$index_min_cv)
    ix <- .fill_col(ix, "type", "relative")
    ix <- .fill_col(ix, "hyperstability", 1)
    ix <- .fill_col(ix, "creep", if (is.null(st$effort_creep)) 0 else st$effort_creep)
    if (!"fishery_dependent" %in% names(ix)) ix$fishery_dependent <- grepl("CPUE", ix$index, ignore.case = TRUE)
    ix$creep[!as.logical(ix$fishery_dependent)] <- 0
    ix <- .fill_col(ix, "q_prior", 1); ix <- .fill_col(ix, "q_cv", 0.5)
    proj$indices <- ix
  }

  years <- seq(st$hist_start, st$proj_end)
  proj$years <- years
  proj$hist_years <- seq(st$hist_start, st$hist_end)
  proj
}

#' @export
print.ttm_project <- function(x, ...) {
  cat("<ttm_project>", x$settings$project_name, "\n")
  cat("  habitats:", nrow(x$habitats), "(", paste(x$habitats$habitat, collapse = ", "), ")\n")
  cat("  groups:  ", nrow(x$groups), " (FMP:", sum(x$groups$group_type == "fmp"), ")\n")
  cat("  gears:   ", paste(x$gear_names, collapse = ", "), "\n")
  cat("  years:   ", x$settings$hist_start, "-", x$settings$hist_end, "(history),",
      x$settings$hist_end + 1, "-", x$settings$proj_end, "(projection)\n")
  invisible(x)
}
