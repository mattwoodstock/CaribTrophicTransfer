# Automated Data Acquisition and Uncertainty Propagation

`CaribTTM` can download every model input that has a public API. Acquired values arrive with an observation uncertainty that is propagated through the Monte Carlo. Any value the user enters overrides the acquired value.

## 1. What is automated

| Input | API / service (default source first) | Function | Uncertainty attached |
|---|---|---|---|
| Sea-surface temperature | Coral Reef Watch CoralTemp **monthly** (PIFSC OceanWatch ERDDAP `CRW_sst_v3_1_monthly`, 1985–); AVHRR Pathfinder 5.3 (`pf5-3-monthly`) fills earlier years. Alternatives: CRW daily (NESDIS), OISST (PFEG) | `fetch_erddap_series()` | within-domain spatial SD |
| Degree heating weeks (annual max) | Coral Reef Watch (PIFSC `CRW_dhw_v1_0`; NESDIS `noaacrwdhwDaily`) | `fetch_erddap_series(time_agg = "max")` | spatial SD |
| Pelagic NPP (index and absolute) | **computed** VGPM + Eppley-VGPM ensemble from ESA OC-CCI v6 chlorophyll, CRW SST and SeaWiFS/MODIS PAR (all PIFSC), 1998–. Alternative: pre-computed MODIS/VIIRS NPP on PFEG ERDDAP | `fetch_vgpm_ensemble()`, `vgpm_npp()`; `fetch_npp_ensemble()` | spread of the two algorithms' anomalies + propagated chlorophyll spatial SD |
| Chlorophyll-a, Kd490 | ESA OC-CCI v6 monthly (PIFSC), 1998–; VIIRS (NESDIS) 2012– | `fetch_erddap_series()` | spatial SD |
| SST and NPP projections | **CMIP6** CESM2 + ACCESS-ESM1-5 (`tos`, `intpp`), historical + SSP2-4.5 / SSP3-7.0 / SSP5-8.5 (PIFSC), delta method relative to 2005–2014 | `fetch_cmip6_series()`, `cmip6_delta_projection()` | between-model SD |
| Regional ocean-model hindcasts/projections | MOM6-NWA12 via NOAA CEFI THREDDS OPeNDAP; any CF NetCDF | `fetch_opendap_series()` | spatial SD |
| Reanalysis | GLORYS12 via Copernicus Marine Toolbox (free account) | `fetch_copernicus_series()` | spatial SD |
| Benthic habitat area | NOAA NCCOS benthic habitat maps (ArcGIS REST; USVI 2002 = layer 6, Puerto Rico 2002 = layer 11) | `fetch_nccos_habitat_areas()`, `map_habitat_classes()` | per-class mapping CV |
| Hurricane disturbances | NOAA NHC HURDAT2 Atlantic best tracks | `fetch_hurdat2()`, `hurricane_disturbances()` | intensity CV |

**Server availability during development (Nov 2026).** PFEG CoastWatch reset connections and PSL THREDDS returned HTTP 429 from the cloud container. The defaults were therefore switched to equivalent, commonly used products on NOAA PIFSC OceanWatch and NESDIS CoastWatch, all of which were tested live. The PFEG, MOM6 and GLORYS fetchers remain available. In the St. Croix `data_sources.csv` they are disabled.

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
| `method` | `erddap`, `vgpm_ensemble`, `npp_ensemble`, `cmip6_delta`, `opendap`, `copernicus`, `nccos_habitat`, `hurdat2` |
| `enabled` | TRUE/FALSE |
| `args` | `key=value` pairs separated by `;` (see below) |

Common arguments:

| Argument | Applies to | Meaning |
|---|---|---|
| `key` | erddap | preset from `ocean_source_catalog()` (`crw_sst_monthly`, `pathfinder_sst_monthly`, `crw_dhw_max_pifsc`, `occci_chla_monthly`, `occci_kd490_monthly`, `crw_sst`, `crw_dhw_max`, `oisst_sst`, `viirs_chla`, `viirs_kd490`, `modis_vgpm_npp`, `viirs_snpp_npp`, `viirs_noaa20_npp`) |
| `fill_key` | erddap | second product used for years the first does not cover (e.g., Pathfinder before 1985) |
| `chunk_years` | erddap | years per request (monthly products: 6–10) |
| `obs_variable`, `cmip_var`, `type`, `experiments`, `models`, `baseline` | cmip6_delta | observed covariate to project, CMIP6 variable, `additive`/`ratio`, SSPs, models, baseline years; creates `<obs_variable>_<ssp>` variables |
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

$V_{max}$ is the maximum sustained wind (kt) near closest approach, and $d$ is the distance (km). The NPP, habitat-quality, effort and production losses are copied from the template event and scaled by $I$. For St. Croix (1983–2024) this gives the following intensities:

| Storm | Intensity |
|---|---|
| Maria 2017 (47 km, 150 kt) | 1.00 |
| Lenny 1999 | 0.98 |
| Hugo 1989 | 0.84 |
| Georges 1998 | 0.54 |
| Marilyn 1995 | 0.54 |
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
* **Using projections in scenarios.** The `cmip6_delta` method creates variables such as `sst_ssp585` (observations up to the last historical year, delta-projected afterwards). A scenario switches the model onto them with the `cov_from` driver, e.g. `scenario = Climate_SSP585, driver = cov_from, target = sst=sst_ssp585`. The Baseline keeps the recent climatology.

### Computed NPP (VGPM ensemble)

Monthly domain-mean chlorophyll *C* (mg m⁻³), SST *T* (°C) and PAR *E₀* (mol photons m⁻² d⁻¹) feed the Behrenfeld & Falkowski (1997) VGPM:

$$
NPP = 0.66125\,P^B_{opt}(T)\,\frac{E_0}{E_0+4.1}\,Z_{eu}\,C\,D_{irr}
$$

* $P^B_{opt}$ is either the VGPM 7th-order temperature polynomial or the Eppley exponential $1.54\cdot10^{0.0275T-0.07}$.
* $Z_{eu}$ is the euphotic depth from total chlorophyll (Morel & Berthon 1989).
* $D_{irr}$ is the day length.

PAR gaps (before SeaWiFS, after the MODIS record) are filled with the monthly climatology. The pelagic **index** is the mean of each algorithm's own anomaly (value / its long-term mean), so differences in absolute level between algorithms do not inflate index uncertainty. The absolute level is also reported (`npp_pelagic_index_abs`, about 130–240 g C m⁻² yr⁻¹ for the St. Croix shelf + buffer). It is higher than the regional open-ocean value of about 85 used in SEDAR103-DW-20, partly because bottom reflectance in optically shallow water biases satellite chlorophyll upward. The case study therefore keeps 85 as the user-specified absolute level.
