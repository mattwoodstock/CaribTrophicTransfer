# Automated Data Acquisition and Uncertainty Propagation

`CaribTTM` can download every model input that has a public API. Acquired values arrive with an observation uncertainty that is propagated through the Monte Carlo. Any value the user enters overrides the acquired value.

## 1. What is automated

| Input | API / service | Function | Uncertainty attached |
|---|---|---|---|
| Sea-surface temperature | NOAA Coral Reef Watch CoralTemp (NESDIS ERDDAP `noaacrwsstDaily`) or OISST v2.1 (PFEG ERDDAP `ncdcOisst21Agg_LonPM180`) | `fetch_erddap_series()` | within-domain spatial SD |
| Degree heating weeks (annual max) | Coral Reef Watch (NESDIS ERDDAP `noaacrwdhwDaily`) | `fetch_erddap_series(time_agg = "max")` | spatial SD |
| Pelagic NPP (index and absolute) | satellite ensemble: MODIS VGPM (`erdMH1ppmday`), VIIRS SNPP and NOAA-20 (`productivity_viirs_*_monthly`) on PFEG ERDDAP | `fetch_npp_ensemble()` | between-product (structural) spread + spatial SD |
| Chlorophyll-a, Kd490 (water clarity) | VIIRS SNPP science quality (NESDIS ERDDAP) | `fetch_erddap_series()` | spatial SD |
| Ocean-model hindcasts / projections | MOM6-NWA12 via NOAA CEFI THREDDS OPeNDAP; any CF NetCDF | `fetch_opendap_series()` | spatial SD |
| Reanalysis | GLORYS12 via Copernicus Marine Toolbox (free account) | `fetch_copernicus_series()` | spatial SD |
| Benthic habitat area | NOAA NCCOS benthic habitat maps (ArcGIS REST; USVI 2002 = layer 6, Puerto Rico 2002 = layer 11) | `fetch_nccos_habitat_areas()`, `map_habitat_classes()` | per-class mapping CV |
| Hurricane disturbances | NOAA NHC HURDAT2 Atlantic best tracks | `fetch_hurdat2()`, `hurricane_disturbances()` | intensity CV |

**Not automated (no public API):**

* Commercial trip-ticket landings and effort, which are confidential.
* SEDAR assessment indices and benchmarks: these are published in reports, not served by an API.
* NCRMP coral cover and fish density summaries: these are distributed as data packages.

These remain CSV inputs. Their observation error can still be specified (`cv` columns; Section 4).

## 2. Configuring acquisition: `data_sources.csv`

One row per acquisition task:

| Column | Meaning |
|---|---|
| `target` | covariate name to create (e.g., `sst`, `dhw`, `npp_pelagic_index`), or `habitat_areas` / `hurricanes` |
| `method` | `erddap`, `npp_ensemble`, `opendap`, `copernicus`, `nccos_habitat`, `hurdat2` |
| `enabled` | TRUE/FALSE |
| `args` | `key=value` pairs separated by `;` (see below) |

Common arguments:

| Argument | Applies to | Meaning |
|---|---|---|
| `key` | erddap | preset from `ocean_source_catalog()` (`crw_sst`, `crw_dhw_max`, `oisst_sst`, `viirs_chla`, `viirs_kd490`, `modis_vgpm_npp`, `viirs_snpp_npp`, `viirs_noaa20_npp`) |
| `server`, `dataset_id`, `variable`, `extra_dims` | erddap | any ERDDAP griddap dataset (overrides `key`) |
| `time_agg` | erddap, opendap, copernicus | `mean` (default), `max`, `min`, `sum` within year |
| `stride_time`, `stride_space` | erddap | sub-sampling to reduce download size (e.g., weekly samples of daily data) |
| `scale`, `offset`, `units` | erddap, opendap | transform raw values, e.g. `offset=-273.15` for Kelvin |
| `bbox` | all | `lon_min;lat_min;lon_max;lat_max` (default = settings `domain_*`) |
| `years` | all | R expression, e.g. `2012:2024` (default = historical years) |
| `url`, `lon_name`, `lat_name`, `time_name`, `lon_0360` | opendap | OPeNDAP endpoint and dimension names |
| `dataset_id`, `variable`, `depth_range` | copernicus | Copernicus Marine product, e.g. `cmems_mod_glo_phy_my_0.083deg_P1M-m`, `bottomT` |
| `layer`, `group_field`, `class_map` | nccos_habitat | MapServer layer, attribute to group by, optional CSV mapping classes to model habitats |
| `template`, `template_other`, `radius_km`, `core_km`, `decay_km`, `intensity_cv` | hurdat2 | disturbance template(s) and the distance/intensity model |

Set the project domain in `settings.csv` (`domain_lon_min`, `domain_lon_max`, `domain_lat_min`, `domain_lat_max`).

Run:

```r
acquire_project_data("Projects/StCroix_Trap")                 # all enabled sources
acquire_project_data("Projects/StCroix_Trap", only = "dhw")   # refresh one source
```

Results are written to `data/acquired/`:

* `env_covariates.csv` (with `sd`)
* `habitats.csv` (areas with `area_cv`; pelagic NPP)
* `nccos_habitat_classes.csv` (raw class areas)
* `disturbances.csv` (with `intensity_cv`)
* `acquisition_log.csv` (time, source and status of every request)

Commit these files to make runs reproducible offline.

### Hurricane intensity model

For each HURDAT2 storm passing within `radius_km` of the domain centre, the intensity of the disturbance event is

$$
I = \underbrace{\min\!\Big(1, \max\!\big(0, \tfrac{V_{max}-34}{137-34}\big)\Big)}_{\text{tropical storm} \to \text{Cat 5}} \times
\underbrace{\begin{cases}1 & d \le d_{core} \\ e^{-(d-d_{core})/d_{decay}} & d > d_{core}\end{cases}}_{\text{distance at closest approach}}
$$

$V_{max}$ is the maximum sustained wind (kt) near closest approach, and $d$ is the distance (km). The NPP, habitat-quality, effort and production losses are copied from the template event and scaled by $I$. For St. Croix this gives the following intensities, over 2000–2024. The case study uses 2010–2024, so Omar 2008 and Jeanne 2004 are not part of it:

| Storm | Intensity |
|---|---|
| Maria 2017 (47 km, 150 kt) | 1.00 |
| Irma 2017 | 0.54 |
| Omar 2008 | 0.36 |
| Dorian 2019 | 0.30 |
| Irene 2011 | 0.25 |
| Jeanne 2004 | 0.20 |

Weaker events are also included.

## 3. User override rules

Acquired data never overwrite the user's own CSV files. `read_ttm_project()` merges them with these precedence rules:

| Table | Rule |
|---|---|
| `env_covariates.csv` | User rows win for every (variable, year) they supply, **except** rows whose `data_status` contains `PLACEHOLDER`: those are replaced by acquired data for that variable. Acquired rows fill all other (variable, year) combinations. |
| `habitats.csv` | Acquired `area_km2` / `npp_value` (and their CVs) fill only **blank** cells. Enter a number to override. |
| `disturbances.csv` | Acquired events are added only for years the user did not list. Add a row for a year to override that year's events (e.g., a field-surveyed damage estimate). |
| all | `settings.csv: use_acquired = FALSE`, or `read_ttm_project(dir, use_acquired = FALSE)`, ignores acquired data entirely. |

The merge is recorded in `proj$provenance`.

## 4. Uncertainty propagation: every input can carry error

Each Monte Carlo draw samples every uncertain input independently, and all scenarios share that draw. Draws are then conditioned with SIR (Model Description §10). The full list:

| Source | Where specified | Distribution |
|---|---|---|
| Habitat area | `habitats.csv: area_cv` (API: mapping CV) | lognormal |
| Net primary production | `npp_cv` (API: ensemble spread) | lognormal |
| Food-web fraction, TTE | `food_web_fraction_cv`, `tte_cv` | beta |
| C:WW conversion | `settings: c_to_ww_cv` | lognormal |
| P/B, x_msy | `groups.csv: pb_cv`, `x_msy_cv` | lognormal / beta |
| Partition weights, fixed B₀ | `partition_cv`, `B0_fixed_cv` | lognormal |
| Initial depletion | `init_depletion_cv` | beta |
| Immigration | `immigration_cv` | lognormal |
| Process error | `process_sd` | lognormal (annual) |
| Diet composition | `settings: diet_concentration` | Dirichlet |
| Habitat preferences | `settings: pref_cv` | lognormal |
| Environmental sensitivities | `env_response.csv: sensitivity_cv` | lognormal |
| Coral growth, bleaching mortality | `settings: coral_growth_cv`, `bleach_mort_cv` | lognormal |
| **Covariate observations** | `env_covariates.csv: sd` (API: spatial / ensemble SD) | normal, per observation |
| **Landings observations** | `landings.csv: cv` or `settings: landings_cv` | lognormal, per record |
| Reporting expansion | `settings: catch_expansion_cv` | lognormal |
| **Effort observations** | `effort.csv: cv` or `settings: effort_cv` | lognormal, per record |
| **Disturbance intensity** | `disturbances.csv: intensity_cv` (API: 0.3) | lognormal, per event |
| Index observations | `indices.csv: cv` (floored at `index_min_cv`) | lognormal likelihood (SIR) |
| Benchmarks | `benchmarks.csv: sir_cv` | lognormal likelihood (SIR) |
| Catch feasibility | `settings: catch_feasibility_cv` | likelihood (SIR) |

When covariate `sd` or disturbance `intensity_cv` values are present, the annual driver matrices (covariates, NPP index, coral dynamics, disturbance multipliers) are rebuilt for each draw. This costs about 16 ms per draw per scenario. Run `has_driver_uncertainty(proj)` to check whether this is active.

## 5. Network access and credentials

* **ERDDAP, NCCOS, NHC and THREDDS** are public and need no credentials. Some servers throttle repeated requests. PFEG CoastWatch reset connections during development, and PSL THREDDS returned HTTP 429. Downloads are therefore retried with exponential back-off, and failures are logged without stopping the other sources.
* **Copernicus Marine** needs a free account. Set `COPERNICUSMARINE_SERVICE_USERNAME` / `COPERNICUSMARINE_SERVICE_PASSWORD`, or run `copernicusmarine login` once, and install the toolbox with `pip install copernicusmarine`.
* **MOM6-NWA12 (CEFI).** Browse the NOAA CEFI data portal for the desired variable, run (hindcast or SSP projection) and frequency, and paste its OPeNDAP URL into `data_sources.csv`. Projections can be appended as future years of a covariate. They are then used directly in the projection period.
