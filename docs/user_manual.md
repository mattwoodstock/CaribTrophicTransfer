# User Manual

## 1. Installation

`CaribTTM` is an R package (R ≥ 4.1). Only base R is required for the model. `ggplot2` is needed for figures, `ncdf4` for reading NetCDF ocean or satellite products, and `testthat` for tests.

```r
# from a local clone of the repository
install.packages(c("ggplot2", "ncdf4", "testthat"))
devtools::install(".")          # or: devtools::load_all(".") during development
library(CaribTTM)
```

Run the test suite:

```r
devtools::test(".")
```

## 2. Quick start

```r
library(CaribTTM)
proj <- ttm_example_project(n_draws = 200)  # small in-memory example
res  <- run_ttm(proj)                       # Monte Carlo over all scenarios
reference_points(res)                       # group MMSY, OFL proxy, target, status
aggregate_mmsy(res, by = "guild")           # guild MMSY
gear_allocation(res)$effort_ceilings        # gear effort ceilings + choke groups
write_spf_inputs(res, "output/tables")      # SPF hand-off tables
save_standard_plots(res, "output/figures")
```

To start a new project from editable CSV templates:

```r
write_ttm_templates("Projects/MyIsland/data")  # writes a complete runnable set of CSVs
proj <- read_ttm_project("Projects/MyIsland")
```

Download API-available inputs (SST, DHW, satellite NPP, habitat areas, hurricanes), then run:

```r
acquire_project_data("Projects/StCroix_Trap")   # writes Projects/StCroix_Trap/data/acquired/
```

St. Croix case study (from the repository root):

```bash
Rscript Projects/StCroix_Trap/run_stcroix.R
```

Add a number of draws for a quick test run, e.g. `Rscript Projects/StCroix_Trap/run_stcroix.R 100`.

## 3. Workflow

```
 habitats.csv ─┐            env_covariates.csv ─┐    disturbances.csv / scenarios.csv
 (area, NPP)   │            (SST, DHW, NPP idx, │    (hurricanes, bleaching, effort)
               ▼             coral cover)       ▼               ▼
        Primary production by habitat ──► TL energy pools (TTE, fishing feedback)
                                                      │
 groups.csv, diet.csv, habitat_preference.csv ───────►│ demand-weighted partitioning
                                                      ▼
                     Group production → carrying capacity K (habitat refuge, environment)
                                                      │
 landings.csv / effort.csv ──────────► logistic biomass dynamics (history: landings; projection: effort)
                                                      │
          Monte Carlo draws ──► SIR (indices.csv, benchmarks.csv, catch feasibility)
                                                      ▼
   MMSY (group, guild, gear) · OFL proxy · P* target · EU · B/BMSY · choke groups · indicators
                                                      ▼
                               SPF hand-off tables (write_spf_inputs)
```

Recommended steps for a new island or region:

1. **Define the domain.** Choose the island platform (FMP unit) and the pelagic integration area. For trap fisheries this is the insular shelf plus a buffer. For pelagic-dominated fisheries a larger EEZ polygon may be appropriate.
2. **Habitats.** Leave `area_km2` blank to fill it from the NCCOS benthic habitat map API, or tabulate mapped area yourself in an equal-area projection. Assign **net** primary production per habitat from the literature, in any supported unit (`npp_unit`).
3. **Pelagic NPP and oceanography.** Set the domain bounding box in `settings.csv` and list sources in `data_sources.csv`; then run `acquire_project_data()`. The satellite NPP ensemble provides the index (`npp_covariate`) and, if `npp_value` is blank, the long-term mean. See [Data Acquisition](data_acquisition.md).
4. **Functional groups.** Use one row per FMP stock or stock complex. Add non-fishery groups that compete for the same pools (zooplankton, benthic grazers, detritivores, predatory invertebrates, small reef fish, other predators). Then set P/B, x_msy, partitioning information, refuge dependence and `data_tier`.
5. **Diet and habitat preference.** Enter trophic-level diet fractions and habitat affinities.
6. **Fishery data.** Map trip-ticket species to FMP groups and enter landings by year × gear × group (lb) and effort by gear.
7. **Environment and disturbances.** Add covariates (API or Section 5) and response functions. Hurricanes are generated from HURDAT2; add rows for other events or to override a year, and define templates for scenarios.
8. **Conditioning data.** Add relative indices (CPUE, survey densities) and assessment OFL/MSY benchmarks.
9. **Scenarios.** Define the management and environmental scenarios to project.
10. **Run, check, iterate.** Check the SIR effective sample size, catch-feasibility shortfalls (`landings_shortfall`), benchmark ratios, and the prior vs. posterior figure. Revise priors that the data reject.

## 4. Running the model

| Function | Purpose |
|---|---|
| `read_ttm_project(dir)` | read and validate a project folder |
| `validate_ttm_project(proj)` | validate and fill defaults (after editing `proj` in R) |
| `draw_parameters(proj, stochastic)` | one parameter set (deterministic if `FALSE`) |
| `build_drivers(proj, scenario)` | annual driver matrices for a scenario |
| `simulate_ttm(proj, par, drv)` | one deterministic simulation |
| `run_ttm(proj, n_draws, scenarios, sir)` | full Monte Carlo + SIR + scenarios |
| `apply_sir(res)`, `prior_results(res)` | (re)condition draws or revert to the prior |
| `acquire_project_data(dir)` | download API inputs to `data/acquired/` |
| `fetch_erddap_series()`, `fetch_npp_ensemble()`, `fetch_opendap_series()`, `fetch_copernicus_series()`, `fetch_nccos_habitat_areas()`, `fetch_hurdat2()` | individual API helpers |

**Run time.** One simulation of the 29-group, 8-habitat St. Croix model over 46 years takes about 0.07 s. `run_ttm` simulates the Baseline for every prior draw, applies SIR, and then runs the other scenarios only for the retained draws. A 1,000-draw, 7-scenario run takes a few minutes. Use ≥ 5,000 draws for final analyses with SIR.

**Changing settings in R:**

```r
proj <- read_ttm_project("Projects/StCroix_Trap")
proj$settings$n_draws <- 5000
proj$habitats$tte <- 0.10                 # sensitivity run
proj <- validate_ttm_project(proj)        # re-validate after edits to tables
res <- run_ttm(proj)
```

## 5. Oceanographic inputs: satellite, GLORYS and MOM6

Any annual covariate can enter through `env_covariates.csv`. To derive one from gridded products:

```r
# GLORYS12 monthly bottom temperature (or MOM6-NWA12 'tob'/'tos', OISST 'sst', CRW 'degree_heating_week')
g <- read_gridded_netcdf("glorys_monthly.nc", var = "bottomT",
                         lon_range = c(-65.1, -64.4), lat_range = c(17.6, 17.9))
sst <- aggregate_gridded_covariate(g, polygon = stx_shelf_polygon, fun = "mean",
                                   variable = "bottom_temp")
# satellite NPP ensemble (mg C m-2 d-1 per cell): integrate cell production first, then sum
```

* **Historical:** use reanalysis (GLORYS12) or observations (OISST, CRW DHW, satellite NPP ensemble).
* **Projections:** append future years of a downscaled projection (e.g., MOM6-NWA12 under SSP2-4.5 or SSP5-8.5) to the same variable. Future values supplied in `env_covariates.csv` are used as-is. Otherwise covariates are held at their recent mean and modified by scenario drivers (`cov_trend`, `cov_offset`, `cov_set`).
* **NPP index:** set `npp_covariate` on the habitat row. The model uses the covariate divided by its historical mean as a multiplier.
* **Bias correction:** if model and observation periods overlap, apply delta-method bias correction before entry, so that the historical mean of the covariate matches the observations.

## 6. Outputs

`write_spf_inputs(res, dir)` writes:

| File | Content | Key columns |
|---|---|---|
| `spf_group_reference_points.csv` | one row per scenario × group | `MMSY_p05..p95_t/_lb`, `OFL_proxy`, `target`, `MMSY0_p50`, `B0_t`, `B_over_BMSY`, `catch`, `EU`, `U_catch_over_MMSY`, `P_overfishing`, `P_overfished`, `landings_shortfall`, `status`, `TL`, `data_tier` |
| `spf_guild_mmsy.csv` | guild and FMP-complex aggregates | MMSY quantiles, target, catch, P(overfishing) |
| `spf_gear_targeting_matrix.csv` | gear × group | `catch_t`, `composition_W` (share of gear catch), `share_of_group_catch` |
| `spf_gear_mmsy.csv` | gear × group MMSY allocation | `MMSY_gear_p05/p50/p95/target`, catch, ratio |
| `spf_gear_effort_ceilings.csv` | per gear | `effort_ref`, `effort_multiplier_*`, `effort_ceiling_*`, `choke_group`, `P_choke`, alternatives |
| `spf_mmsy_draws.csv` | draw × group MMSY (t) | full distribution for MSE propagation |
| `spf_mmsy_timeseries.csv` | scenario × year × group | MMSY quantiles through time |
| `ecosystem_indicators.csv` | scenario × year × indicator | total catch, mean TL of catch, pelagic:demersal ratio, total MMSY, catch/MMSY, biomass, P1 |
| `benchmark_comparison.csv` | benchmark vs MMSY | ratio of median MMSY to OFL/ABC/ACL |
| `sir_diagnostics.csv` | draw weights and NLL components | |

**How an SPF analyst uses these tables**

* **Capacity vs. ecosystem cap.** For each gear *g*, compare the SPF capacity output (catch at full efficiency, maximum inputs) for group *i* with `MMSY_gear_*`. Capacity above `MMSY_gear_p50` indicates overcapacity relative to the OFL proxy. Capacity above `MMSY_gear_target` indicates overcapacity relative to the P* target.
* **Effort reference points.** The SPF maps effort to capacity catch. Combining that mapping with the gear MMSY gives effort limits. `effort_ceiling_*` gives the simpler proportional-catch version, which is useful as a cross-check.
* **Choke groups.** The choke group sets the binding constraint for a gear. `P_choke` measures how robust that identification is.
* **Uncertainty.** Sample rows of `spf_mmsy_draws.csv` jointly with the SPF parameter uncertainty in the MSE.

Figures (`save_standard_plots`):

| Figure | Content |
|---|---|
| `fig01` | energy budget by habitat |
| `fig02` | group MMSY intervals with catch and benchmarks |
| `fig03` | scenario status stoplight |
| `fig04`, `fig05` | B/BMSY and MMSY trajectories |
| `fig06` | coral habitat quality |
| `fig07` | gear effort ceilings |
| `fig08` | prior vs. SIR posterior |

## 7. Re-estimation triggers

SEDAR103-DW-20 recommends re-running the model when indicators exceed predefined rates of change. `ecosystem_indicators.csv` provides the model-side indicators. A practical rule is to re-run when any of the following occurs:

1. the NPP index or coral cover falls outside the historical range;
2. any FMP group index declines ≥ 25%;
3. mean TL of catch or the pelagic:demersal ratio changes by more than the 90% interval simulated under Baseline;
4. a disturbance event occurs.

The frequency of re-estimation can be tuned in closed-loop MSE.

## 8. Troubleshooting

| Symptom | Likely cause / remedy |
|---|---|
| `SIR effective sample size is low` | Priors conflict with indices, benchmarks or landings. Increase `n_draws`, widen CVs, check `sir_diagnostics.csv` to see which component dominates, and re-centre priors flagged by `landings_shortfall`. |
| Group crashes (`B_over_BMSY` ≈ 0, `Overfished`) in history | Landings exceed what the prior energy share can support. Check `landings_shortfall`, raise the partition prior or P/B, check `catch_expansion`, or the data may be correct and the stock overfished. |
| MMSY far above ACL/OFL for unassessed groups | Partition weights too high relative to non-fishery competitors. Add missing competitor groups, use survey densities, or add a benchmark with `use_in_sir`. |
| `Diet rows not summing to 1` warning | Rows are normalized automatically; check the input. |
| `habitat_preference.csv missing groups with benthic diet` | Add a row for every group that feeds on `B_` pools. |
| A covariate is reported as not found | The variable name in `env_response.csv` must match `env_covariates.csv` or a scenario `cov_*` target. |
