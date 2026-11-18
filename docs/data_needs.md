# Data Needs: Placeholders to Replace (St. Croix Case Study)

Every input in the St. Croix project has a value, so the model runs end to end. The items below are **placeholders** (flagged `PLACEHOLDER` in the `data_status` column) or **approximations** that should be replaced with real data before results are used for management. They are grouped by data type, with the file and columns to update and a suggested source.

Priority: **H** = strongly affects MMSY scale or status; **M** = affects dynamics, scenarios or allocation; **L** = minor or sensitivity only.

## A. Fishery-dependent data (confidential; no public API)

| # | Item | File / columns | Current placeholder | Suggested source | Priority |
|---|---|---|---|---|---|
| A1 | Commercial landings by year × gear × FMP group, 1983–2024 | `landings.csv: landings_lb` | synthetic series scaled to recent ACL-era magnitudes (only queen conch 2021 = 30,535 lb is real) | SEFSC Caribbean Commercial Landings (CCL) / St. Croix trip tickets, species mapped to FMP complexes | **H** |
| A2 | Landings observation error | `landings.csv: cv` or `settings: landings_cv` (0.2) | assumed 0.2 | CCL vs TIP or MER Consultants catch-validation comparisons | M |
| A3 | Reporting expansion factor | `settings: catch_expansion` (1.5 ± 25%) | assumed (Puerto Rico ≈ 2.0; USVI unquantified) | SEFSC expansion studies; MER Consultants Sampling and Catch Validation Project | **H** |
| A4 | Effort by gear (trips; ideally trap-hauls, soak days, dive hours) | `effort.csv: effort` | synthetic trips per gear | CCL trip tickets; TIP | **H** (projections, gear allocation) |
| A5 | Effort observation error | `effort.csv: cv` or `settings: effort_cv` (0.1) | assumed | trip-ticket audits | L |
| A6 | Gear split of each group's landings (fish trap vs lobster trap vs dive vs hook-and-line) | implicit in A1 | assumed splits (e.g., lobster 30/15/55 lobster-trap/fish-trap/dive) | CCL gear codes; TIP | **H** (choke groups) |
| A7 | Recreational / subsistence removals | add to `landings.csv` (gear = `recreational`) | none | MRIP-equivalent surveys if available; household surveys | M |

## B. Assessment indices and benchmarks (published; transcribe from reports)

| # | Item | File / columns | Current placeholder | Suggested source | Priority |
|---|---|---|---|---|---|
| B1 | St. Croix spiny lobster standardized trap CPUE | `indices.csv` (`SEDAR57_lobster_trap_CPUE`) | synthetic 2012–2021 | SEDAR 57 / SEDAR 57 Update (SEDAR91-RD20) | **H** |
| B2 | St. Croix queen triggerfish commercial trap index | `indices.csv` (`SEDAR80_QT_trap_CPUE`) | synthetic 1985–2019 | SEDAR 80 Stock Assessment Report | **H** |
| B3 | Yellowtail snapper index / reference points | `indices.csv`, `benchmarks.csv` | none | SEDAR 84 (St. Croix yellowtail snapper) | M |
| B4 | Spiny lobster and queen triggerfish B₀ / SSB₀ (for `fixed_biomass` partitioning) | `groups.csv: B0_fixed_t` | not used (biomass priors instead) | SEDAR 57 Update; SEDAR 80 | M |
| B5 | Natural mortality / P/B for assessed stocks | `groups.csv: pb` | lobster 0.8, queen triggerfish 0.6 | SEDAR 57 (M ≈ 0.34 + F); SEDAR 80 | M |

Already real: lobster OFL/ABC/ACL, queen triggerfish OFL/ABC/ACL, Parrotfish 2, Snapper 1–3, queen conch and dolphinfish ACLs (`benchmarks.csv`).

## C. Fishery-independent biological data

| # | Item | File / columns | Current placeholder | Suggested source | Priority |
|---|---|---|---|---|---|
| C1 | Biomass density (t km⁻² = g m⁻²) by FMP group and habitat, for partitioning | `groups.csv: partition_value` | **now derived** from Opitz (1996) VI species biomass × mass-balance factor; conch, pelagics and deepwater snappers still placeholders | NCRMP St. Croix fish surveys: the ingestion pipeline (`ncrmp_fish` method, stratified estimator) is built and tested; NCEI downloads were unavailable (HTTP 503) during development | **H** |
| C2 | Parrotfish biomass density index | `indices.csv` (`NCRMP_parrotfish_density`) | synthetic biennial 2013–2023 | NCRMP St. Croix Scaridae density | M |
| C3 | Queen conch density / abundance surveys | `indices.csv`, `groups.csv` | none | St. Croix conch surveys (DPNR / SEFSC) | M |
| C4 | Non-fishery competitor biomass | `groups.csv: partition_value` | **now from** Opitz (1996) balanced 50-box model | local invertebrate surveys to update | M |
| C5 | P/B for unassessed groups | `groups.csv: pb`, `pb_cv` | **now** geometric mean of Opitz (1996) P/B and FishBase M (Then et al. 2015) | SEDAR-specific M where available | L |
| C6 | Diet composition by trophic-level pool | `diet.csv` | **now** matched to FishBase trophic levels | stable isotopes for St. Croix | L |
| C7 | Habitat preferences | `habitat_preference.csv` | expert-informed | NCRMP occupancy by habitat stratum; telemetry studies | M |
| C8 | Refuge dependence on coral habitat quality | `groups.csv: refuge_dependence` | expert guesses (0–0.7) | NCRMP fish–rugosity relationships | M |
| C9 | x_msy (MMSY fraction) by group | `groups.csv: x_msy` | 0.5 (0.4 conch) | literature on reef MSY/M·B₀ ratios; sensitivity runs | M |
| C10 | Initial depletion in 1983 | `groups.csv: init_depletion` | assumed 0.4–0.8 | historical CPUE; earliest SEDAR biomass estimates | M |
| C11 | Immigration / connectivity | `groups.csv: immigration_t` | 25 t yr⁻¹ for pelagics only | larval connectivity models; Greater Caribbean model | L |

## D. Habitat and primary production

| # | Item | File / columns | Current value | Suggested source | Priority |
|---|---|---|---|---|---|
| D1 | Deeper-shelf habitat | `habitats.csv` (`deep_hardbottom`) | **resolved**: 41.4 km² at 30–100 m from ETOPO 2022 (the 388 km² NCCOS "Unknown" class was mostly off-shelf); NPP light-limited (~165 t C km⁻² yr⁻¹, CV 0.6). The 2017 NCCOS shelf map covers St. Thomas/St. John only | mesophotic surveys of Lang Bank for composition | M |
| D2 | St. Croix-specific benthic net production by habitat | `habitats.csv: npp_value` | Caribbean literature values (Adey & Steneck 1985 is St. Croix) | local metabolism studies; Buck Island / East End measurements | M |
| D3 | Food-web fraction of NPP by habitat | `food_web_fraction` | 0.2–0.5 assumed (tested in the structural ensemble ×0.67/×1.5) | Ecopath detritus/EE flows (Opitz 1996) | **H** |
| D4 | Trophic transfer efficiency | `tte` | 0.15 ± 20% (SEDAR103-DW-20 default) | sensitivity 0.10–0.20 | **H** |
| D5 | Coral cover before 2000 | `env_covariates.csv: coral_cover` | PLACEHOLDER 1983–1999 (0.22 → 0.13 after Hugo/Marilyn) | historical St. Croix reef surveys (Buck Island NPS, Rogers et al.), TCRMP baseline | M |
| D6 | Coral cover 2000–2024 (exact values) | `coral_cover` | approximate (TCRMP / NCRMP magnitudes) | TCRMP and NCRMP St. Croix benthic data | M |
| D7 | Coral growth, bleaching mortality per DHW, SCTLD mortality | `settings`, `scenarios.csv` | literature-typical | NCRMP / TCRMP before–after bleaching surveys | M |

## E. Environmental drivers and disturbances

All environmental covariates are now acquired from APIs (Section F). What remains are the *effects* of the drivers:

| # | Item | File / columns | Current placeholder | Suggested source | Priority |
|---|---|---|---|---|---|
| E1 | Thermal sensitivities of groups | `env_response.csv` | **now** AquaMaps temperature preferences (FishBase); lobster and conch have none (SeaLifeBase pending) | physiological thermal-performance studies | M |
| E2 | Hurricane loss magnitudes (NPP, coral, trap effort, lobster production) and recovery half-lives | `disturbances.csv` templates | 25% NPP, 15% coral, 60% trap loss, half-lives 1.5–2 years | post-Hugo/Marilyn/Maria damage surveys; trap-loss reports; disaster-assistance records | M |
| E3 | Future bleaching frequency and DHW magnitude | `scenarios.csv` (`cov_set dhw`) | assumed event years | Coral Reef Watch / CMIP6-based bleaching projections (e.g., van Hooidonk et al.) | M |
| E4 | Pelagic NPP absolute level | `habitats.csv: npp_value` (pelagic) | 85 g C m⁻² yr⁻¹ (SEDAR103-DW-20 regional magnitude); the computed shelf ensemble is ~130–240 (shelf chlorophyll is likely biased high by bottom reflectance) | St. Croix-domain extraction of the Ryan-Keogh et al. (2023) ensemble | M |

## F. Already acquired automatically (no action needed; refresh with `acquire_project_data()`)

| Data | Source | Years |
|---|---|---|
| SST (annual mean) | NOAA Coral Reef Watch CoralTemp monthly; AVHRR Pathfinder 5.3 for 1983–84 (PIFSC OceanWatch ERDDAP) | 1983–2024 |
| Degree heating weeks (annual max) | NOAA Coral Reef Watch (PIFSC) | 1985–2024 |
| Pelagic NPP index | computed VGPM + Eppley-VGPM from ESA OC-CCI chlorophyll, CRW SST and SeaWiFS/MODIS PAR (PIFSC) | 1998–2024 (earlier years held at the mean) |
| Chlorophyll-a, Kd490 | ESA OC-CCI v6 (PIFSC) | 1998–2024 |
| SST and NPP projections | CMIP6 CESM2 + ACCESS-ESM1-5, SSP2-4.5 and SSP5-8.5, delta method (PIFSC) | 2025–2045 |
| Benthic habitat areas | NOAA NCCOS USVI 2002 benthic habitat map (ArcGIS REST) | static |
| Hurricane disturbances | NOAA NHC HURDAT2 | 1983–2024 |

Optional sources that are configured but disabled, because they need credentials or were unreachable from the development container:

* GLORYS12 bottom temperature (Copernicus Marine; free account).
* MOM6-NWA12 (NOAA CEFI THREDDS).
* Pre-computed PFEG NPP products.
