# Data and parameterization

## Principles

Inputs follow four rules:

1. **Real data first.** Where a public data product exists, it is acquired automatically through its API, processed reproducibly and stored with its provenance.
2. **The user overrides.** Values entered by the user always take precedence:
   * acquired data fill only blank cells, or rows the user marked `PLACEHOLDER`;
   * acquired disturbances are added only for years the user did not list;
   * `use_acquired = FALSE` ignores acquired data entirely.
3. **Every value has a status and a source.** Every row of every input table carries:
   * a `data_status`: API, derived, published, ASSUMPTION or PLACEHOLDER;
   * a `reference`: a citation, data product or explicit statement of expert judgement.
4. **Every uncertain value has an uncertainty.** A CV, standard deviation or distribution is attached and propagated (Section 9).

## Automated data acquisition

`acquire_project_data()` reads the project's `data_sources.csv` and runs each enabled acquisition method.

| Method | What it acquires |
|---|---|
| `erddap` | Gridded time series from any ERDDAP server (spatial mean and SD over the domain, aggregated to annual values). For St. Croix: Coral Reef Watch CoralTemp monthly SST (1985–), AVHRR Pathfinder 5.3 SST (1983–84 fill), Coral Reef Watch degree heating weeks (annual maximum), and ESA OC-CCI v6 chlorophyll and Kd490 (1998–), all from the NOAA PIFSC OceanWatch server. |
| `vgpm_ensemble` | Pelagic NPP index computed from chlorophyll, SST and PAR with VGPM and Eppley-VGPM (Section 4). |
| `cmip6_delta` | CMIP6 CESM2 and ACCESS-ESM1-5 SST and integrated NPP for SSP2-4.5 and SSP5-8.5, as delta-method projections. |
| `nccos_habitat` | NOAA NCCOS 2002 USVI benthic habitat polygons (ArcGIS REST). Areas are summed into model habitats by `nccos_class_map()`, with class-mapping CVs of 0.15–0.5. |
| `deep_shelf_light` | ETOPO 2022 bathymetry (30–100 m band) and light-limited production. |
| `hurdat2` | NHC HURDAT2 Atlantic best tracks, converted to disturbance events (Section 6). |
| `sargassum_afai` | USF AFAI floating-algae index, 7-day composites on the AOML ERDDAP server (Section 6). |
| `ncrmp_fish` | NCRMP reef visual census, stratified biomass by group (Cochran 1977 stratified estimator). Implemented and tested; NCEI downloads were unavailable during development. |
| Optional | `opendap` (MOM6-NWA12 via NOAA CEFI) and `copernicus` (GLORYS12 bottom temperature), for other regions or deeper habitats. |

Species traits were acquired with a separate data-raw script. It uses the FishBase and SeaLifeBase v24.07 tables (Froese & Pauly 2024; Palomares & Bailly 2024), distributed as Parquet files, to obtain for every FMP species:
* growth (K, L∞);
* length–weight;
* trophic level;
* the Then et al. (2015) M estimator;
* AquaMaps temperature preferences.

{{AUTO:in_sources}}

{{AUTO:fig09}}

{{AUTO:fig10}}

## Habitats

{{AUTO:in_habitats}}

Sources and status by habitat:

{{AUTO:in_habitat_sources}}

## Functional groups

{{AUTO:in_groups}}

Biomass densities (partition values) and P/B ratios were derived from the Opitz (1996) Virgin Islands Ecopath model:
* **Biomass.** For FMP fishes, the species biomasses in Opitz Table 3.6 were summed into FMP complexes and multiplied by 0.239. That factor is the ratio the balanced 50-box model (Table 3.22) applied to the raw survey biomass.
* **P/B.** The geometric mean of the Opitz P/B and the FishBase M estimate. Its CV is the larger of 0.25 and half the absolute log ratio, so disagreement between sources widens the prior.
* **Groups with no direct data.** Queen conch, pelagics, deepwater snappers, wrasses and porgies have partition CV 1.0.

Natural mortality, x and B_MSY/K come from the life-history analysis (Section 7). Nursery and open-recruitment parameters are placeholders (Section 6). The source and status of every group are listed below.

{{AUTO:in_group_sources}}

{{AUTO:in_competitors}}

## Diets and habitat preferences

{{AUTO:in_diet}}

{{AUTO:in_pref}}

## Environmental covariates and responses

{{AUTO:in_covariates}}

{{AUTO:in_env}}

## Fishery data

**Landings and effort** by year, gear and complex for 1983–2024 are **placeholders**. They were built to match recent ACL-era magnitudes, with plausible historical trends and gear splits. Two published values are used directly:
* the 2021 queen conch landings (30,535 lb);
* the expansion factor magnitude, informed by Puerto Rico (≈2.0; SEDAR103-DW-20).

Replacing these with the SEFSC Caribbean Commercial Landings trip-ticket data is the highest-priority data need (Section 13).

{{AUTO:in_landings}}

**Abundance indices.** The SEDAR 57 lobster trap CPUE, SEDAR 80 queen triggerfish trap CPUE and NCRMP parrotfish density series are placeholders. They are all assigned role = validate (held out), so the calibration uses only real information.

{{AUTO:in_indices}}

**Management benchmarks.**

{{AUTO:in_bench}}

## Disturbances and scenarios

{{AUTO:in_hurricanes}}

{{AUTO:in_dist}}

{{AUTO:in_scen}}

## Settings

{{AUTO:in_settings}}
