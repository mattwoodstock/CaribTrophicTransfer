# ==============================================================================
# Minimal example project and input templates
# ==============================================================================

#' Build a small in-memory example project
#'
#' Two benthic habitats + pelagic, three FMP groups, two non-fishery groups,
#' two gears. Useful for tests, the quick start and as a template.
#' @param n_draws Monte Carlo draws
#' @export
ttm_example_project <- function(n_draws = 50) {
  st <- ttm_default_settings()
  st$project_name <- "Example island"; st$hist_start <- 2010; st$hist_end <- 2020
  st$proj_end <- 2030; st$n_draws <- n_draws; st$sir_resample <- 0
  habitats <- data.frame(
    habitat = c("reef", "seagrass", "pelagic"), type = c("benthic", "benthic", "pelagic"),
    area_km2 = c(50, 30, 500), area_cv = 0.1,
    npp_value = c(1500, 4.2, 85), npp_unit = c("t_C_km2_yr", "g_DW_m2_d", "g_C_m2_yr"),
    npp_cv = 0.3, food_web_fraction = c(0.35, 0.35, 0.4), food_web_fraction_cv = 0.2,
    tte = 0.15, tte_cv = 0.2, quality_model = c("coral", "none", "none"),
    quality_covariate = c("coral_cover", NA, NA), quality_ref = c(0.1, 1, 1),
    init_quality = c(0.1, 1, 1), p1_quality_elasticity = 0, npp_covariate = NA,
    stringsAsFactors = FALSE)
  groups <- data.frame(
    group = c("lobster", "parrotfish", "snapper", "grazers", "zooplankton"),
    fmp_complex = c("Spiny lobster", "Parrotfish", "Snapper", NA, NA),
    guild = c("Invertivores", "Herbivores", "Piscivores", "Non-fishery", "Non-fishery"),
    group_type = c("fmp", "fmp", "fmp", "nonfishery", "nonfishery"),
    harvest_allowed = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    trap_relevant = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    pb = c(0.8, 1.0, 0.6, 3, 30), partition_method = "biomass",
    partition_value = c(0.6, 3, 1, 40, 6), init_depletion = c(0.5, 0.6, 0.5, 0.95, 0.95),
    init_depletion_cv = c(0.2, 0.2, 0.2, 0.02, 0.02), refuge_dependence = c(0.5, 0.5, 0.3, 0, 0),
    stringsAsFactors = FALSE)
  diet <- data.frame(group = groups$group,
                     B_TL1 = c(0.1, 1, 0, 1, 0.1), B_TL2 = c(0.7, 0, 0.4, 0, 0),
                     B_TL3 = c(0.2, 0, 0.4, 0, 0), B_TL4 = 0, P_TL1 = c(0, 0, 0, 0, 0.9),
                     P_TL2 = 0, P_TL3 = c(0, 0, 0.2, 0, 0), P_TL4 = 0)
  pref <- data.frame(group = groups$group, reef = c(1, 1, 1, 1, 1), seagrass = c(0.6, 0.4, 0.5, 1, 1))
  gears <- data.frame(gear = c("trap", "dive"))
  yrs <- 2010:2020
  landings <- rbind(
    data.frame(year = yrs, gear = "trap", group = "lobster", landings_lb = 40000),
    data.frame(year = yrs, gear = "dive", group = "lobster", landings_lb = 30000),
    data.frame(year = yrs, gear = "trap", group = "parrotfish", landings_lb = 30000),
    data.frame(year = yrs, gear = "trap", group = "snapper", landings_lb = 10000))
  effort <- rbind(data.frame(year = yrs, gear = "trap", effort = 1000),
                  data.frame(year = yrs, gear = "dive", effort = 500))
  cov <- rbind(data.frame(year = yrs, variable = "sst", value = 27.5 + 0.02 * (yrs - 2010)),
               data.frame(year = yrs, variable = "dhw", value = c(0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 2)),
               data.frame(year = c(2010, 2015, 2020), variable = "coral_cover", value = c(0.1, 0.07, 0.06)))
  env_response <- data.frame(group = "lobster", variable = "sst", type = "linear", sensitivity = -0.2)
  dist <- data.frame(year = c(2017, NA), event = c("Hurricane", "hurricane_template"), intensity = 1,
                     habitat = "all_benthic", p1_loss = 0.25, quality_loss = 0.15, half_life_yr = 2,
                     gear = "trap", effort_loss = 0.5, effort_half_life_yr = 1, group = NA, production_loss = 0)
  scen <- data.frame(scenario = c("Effort_up", "Warming", "Hurricane_2025"),
                     driver = c("effort_mult", "cov_trend", "disturbance"),
                     target = c("trap", "sst", "hurricane_template"),
                     start_year = c(2021, 2021, 2025), end_year = c(2030, 2030, 2025), value = c(1.3, 0.05, 1))
  indices <- data.frame(year = 2012:2020, index = "lobster_cpue", group = "lobster",
                        value = c(1, 1.05, 0.95, 0.9, 0.92, 0.85, 0.8, 0.9, 0.95), cv = 0.3)
  benchmarks <- data.frame(group = "lobster", metric = "OFL", value_lb = 80000, use_in_sir = TRUE, sir_cv = 0.4)
  proj <- list(dir = NA, settings = st, habitats = habitats, groups = groups, diet = diet,
               habitat_preference = pref, gears = gears, landings = landings, effort = effort,
               env_covariates = cov, env_response = env_response, disturbances = dist,
               indices = indices, benchmarks = benchmarks, scenarios = scen)
  class(proj) <- "ttm_project"
  validate_ttm_project(proj)
}

#' Write a project's input tables as CSV templates
#'
#' Writes every input table of `proj` (default: [ttm_example_project()]) to
#' `dir`, giving a complete, runnable set of input files to edit.
#' @param dir output directory
#' @param proj a `ttm_project`
#' @export
write_ttm_templates <- function(dir, proj = ttm_example_project()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  st <- proj$settings
  utils::write.csv(data.frame(key = names(st), value = vapply(st, as.character, ""), notes = ""),
                   file.path(dir, "settings.csv"), row.names = FALSE)
  tabs <- c("habitats", "groups", "diet", "habitat_preference", "gears", "landings", "effort",
            "env_covariates", "env_response", "disturbances", "indices", "benchmarks", "scenarios")
  drop_cols <- c("npp_t_C_km2_yr")
  for (tb in tabs) if (!is.null(proj[[tb]])) {
    x <- proj[[tb]]; x <- x[, setdiff(names(x), drop_cols), drop = FALSE]
    utils::write.csv(x, file.path(dir, paste0(tb, ".csv")), row.names = FALSE, na = "")
  }
  invisible(list.files(dir, full.names = TRUE))
}
