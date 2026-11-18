# Case Study: The St. Croix Trap Fishery

> **Read this first.** This case study demonstrates the full CaribTTM workflow with the best *public* data available for St. Croix, USVI. Several key inputs are flagged placeholders: confidential trip-ticket landings and effort, standardized assessment indices, and most biological priors. The structure, data flows and diagnostics are real. The **numbers are illustrative and are not management advice** until the placeholders listed in Section 6 are replaced.

## 1. Why St. Croix and why traps

* **An isolated platform.** St. Croix is a single insular shelf, separated from the Puerto Rico–Virgin Islands platform by deep trenches. Under the island-based Fishery Management Plans it is its own management unit (St. Croix FMP, effective 2022). The model domain is therefore the St. Croix shelf, including Lang Bank, Buck Island Reef National Monument and East End Marine Park.
* **A trap-dependent, multispecies fishery.** Fish and lobster traps catch dozens of species simultaneously, among them spiny lobster, queen triggerfish, parrotfishes, surgeonfishes, grunts, snappers, groupers, squirrelfishes, boxfishes and angelfishes. Single-species ACLs are hard to apply to such a gear. Gear-level effort controls informed by multispecies caps (SEDAR103-DW-20) are a natural fit.
* **Two assessed stocks provide anchors.** SEDAR 57 (spiny lobster) and SEDAR 80 (queen triggerfish) give assessment-based OFLs. These anchor the absolute scale of the trophic model, while the energy budget constrains the many unassessed complexes.

## 2. Domain and data

| Component | St. Croix configuration | Source / status |
|---|---|---|
| Period | history 1983–2024 (landings-driven; start of the commercial landings series used in SEDAR 80), projection 2025–2045 (effort-driven); reference years 2022–2024 | user choice |
| Benthic habitat areas | coral reef 41.7 km², colonized pavement 192.0, seagrass 59.4, macroalgae 13.1, sand/mud 12.7, mangrove 1.33, deeper shelf (30–100 m) 41.4 | **API**: NOAA NCCOS USVI 2002 benthic habitat map (ArcGIS REST), class-mapping CVs 0.15–0.5; deeper shelf from ETOPO 2022 (the NCCOS "Unknown" class was mostly off-shelf and is dropped) |
| Benthic NPP | habitat-specific Caribbean net production: reef 1500 t C km⁻² yr⁻¹, pavement 181 mmol O₂ m⁻² d⁻¹, seagrass 4.2 g DW m⁻² d⁻¹, macroalgae 582 mmol O₂, sand 7 mmol O₂, mangrove 400 mmol O₂; deeper shelf light-limited ≈165 t C km⁻² yr⁻¹ (CV 0.6) | literature synthesis (Input Dictionary); **API** deeper-shelf value from ETOPO depth × OC-CCI Kd490 × SeaWiFS/MODIS PAR |
| Pelagic area and NPP | shelf + ~10 km buffer (1,500 km²); 85 g C m⁻² yr⁻¹; annual index 1998–2024 | SEDAR103-DW-20 App. A magnitude; **API** index computed with VGPM + Eppley-VGPM from OC-CCI chlorophyll, CRW SST and SeaWiFS/MODIS PAR |
| SST, DHW | annual mean SST 1983–2024 (CRW monthly + Pathfinder); annual max DHW 1985–2024 | **API**: NOAA Coral Reef Watch / Pathfinder (PIFSC OceanWatch ERDDAP), spatial SD as observation error |
| Climate projections | SSP2-4.5 and SSP5-8.5 SST and NPP index, 2025–2045 | **API**: CMIP6 CESM2 + ACCESS-ESM1-5 (PIFSC), delta method |
| Coral cover | ≈22% (1983, placeholder) → 15% (2000) → 8% (2013) → 4–4.5% (2019–2021) → 3% (2024) | approximate from 2000 (NCRMP/TCRMP; SCTLD from 2021), sd 0.01; placeholder before 2000, sd 0.03 |
| Hurricanes | 23 HURDAT2 storms within 200 km, 1983–2024 (Maria 2017 = 1.00, Lenny 1999 = 0.98, Hugo 1989 = 0.84, …), intensity CV 0.3 | **API**: NHC HURDAT2; per-event loss magnitudes are placeholders |
| Functional groups | 19 FMP groups (Parrotfish 1 prohibited) + 10 non-fishery competitor groups. Biomass and P/B priors from the Opitz (1996) Virgin Islands Ecopath model × mass-balance factor, combined with FishBase M (Then et al. 2015), L∞ and trophic level | **derived** (Opitz 1996; FishBase v24.07); conch, pelagics, deepwater snappers remain placeholders |
| Fishery focus | 13 trap groups (≥ 25% of landings from traps) + the two assessed stocks are SIR-calibrated (`sir_feasibility_groups = trap`); conch, yellowtail, deepwater snappers, jacks and pelagics are carried in the food web but not calibrated and are reported as *Outside focus* | settings |
| Gears | fish traps, lobster traps, diving, hook and line | — |
| Landings / effort | by year × gear × group (lb, trips), landings CV 0.2, effort CV 0.1, expansion factor 1.5 ± 25% | **placeholders** (queen conch 2021 = 30,535 lb is published) |
| Indices | SEDAR 57 lobster trap CPUE, SEDAR 80 queen triggerfish trap CPUE, NCRMP parrotfish density | **placeholders**, all set to `role = validate` (held out of calibration, used only for out-of-sample checks) |
| Benchmarks | lobster OFL 163,823 lb, queen triggerfish OFL 22,773 lb (used in SIR, CV 0.4); ACLs for comparison only | published |
| Uncertainty | 3 adaptive importance-sampling rounds × 12,000 = 48,000 prior draws (4 cores); every input CV and observation error sampled; SIR on OFLs and catch feasibility (CV 0.25) | — |

### Prior-predictive check (catch feasibility)

Under wide priors (partition CV 0.5, or 1.0 for poorly known groups), many prior draws cannot supply the observed (expanded) landings: the simulated stock collapses under the historical catch. The catch-feasibility likelihood (stock-reduction-analysis logic) down-weights those draws, and is what bounds productivity from below for unassessed groups. Plain SIR on 5,000 draws collapsed to an effective sample size (ESS) of 1, because few draws satisfied all 15 focus groups simultaneously. Adaptive importance sampling fixes this: after each round, proposals are re-centred only for the groups in conflict, and the final weights are corrected exactly for the mixture of proposals (Owen & Zhou 2000).

## 3. Running the case study

```r
devtools::load_all(".")
acquire_project_data("Projects/StCroix_Trap")   # optional: refresh API data (results are committed)
source("Projects/StCroix_Trap/run_stcroix.R")   # main run + robustness + validation (≈ 26 min on 4 cores)
source("Projects/StCroix_Trap/run_structural_ensemble.R")   # 10 structural variants (≈ 54 min)
```

Outputs are written to `Projects/StCroix_Trap/output/tables` (SPF hand-off tables and diagnostics) and `output/figures`.

## 4. Results

The main run took 25.7 minutes. SIR on 48,000 prior draws gave ESS 86, 316 unique resampled draws and a largest single weight of 4.7%. Every retained draw supports all historical landings of the focus groups. Values are medians unless stated, in lb yr⁻¹; landings are expanded by 1.5.

### Group MMSY (Baseline, reference years 2022–2024)

| Group | MMSY p05 | **MMSY p50** | MMSY p95 | P* target (40%) | Current catch | B/BMSY | P(overfishing) | Benchmark |
|---|---|---|---|---|---|---|---|---|
| Spiny lobster | 178,509 | **273,858** | 446,066 | 256,940 | 112,540 | 1.63 | 0.000 | OFL 163,823 |
| Queen triggerfish | 15,593 | **25,252** | 39,465 | 24,373 | 14,787 | 1.73 | 0.019 | OFL 22,773 |
| Parrotfish 2 | 104,972 | **310,629** | 1,380,547 | 269,834 | 68,816 | 2.33 | 0.008 | ACL 72,365 |
| Surgeonfishes | 80,189 | **263,550** | 867,857 | 232,308 | 25,571 | 2.06 | 0 | — |
| Grunts | 197,825 | **575,461** | 1,915,195 | 488,713 | 20,159 | 2.32 | 0 | — |
| Snapper 3 (shallow) | 64,743 | **335,824** | 1,158,577 | 294,619 | 13,342 | 2.11 | 0 | ACL 14,156 |
| Groupers (shallow) | 30,699 | **104,264** | 319,170 | 98,200 | 14,244 | 2.08 | 0.002 | — |
| Squirrelfishes | 27,973 | **113,734** | 281,497 | 101,040 | 9,291 | 2.14 | 0 | — |
| Goatfishes | 11,320 | **37,798** | 115,062 | 29,759 | 5,434 | 2.02 | 0 | — |
| Boxfishes | 17,669 | **64,233** | 264,068 | 50,460 | 6,602 | 2.13 | 0 | — |
| Angelfishes | 41,899 | **196,951** | 731,758 | 170,756 | 3,811 | 2.09 | 0 | — |
| Wrasses (hogfish) | 6,985 | **11,018** | 37,777 | 10,130 | 6,559 | 1.79 | 0.034 | — |
| Porgies | 3,046 | **6,393** | 18,899 | 5,510 | 3,242 | 1.85 | 0.027 | — |
| *Outside focus (uncalibrated):* queen conch, yellowtail snapper, deepwater snappers, jacks, pelagics | | | | | | | | |
| Parrotfish 1 | (prohibited) | | | | 0 | | | ABC = 0 |

The full table, including the uncalibrated groups, is `spf_group_reference_points.csv`.

### Gear effort ceilings and choke groups (choke candidates restricted to calibrated groups)

| Gear | Effort multiplier p05 / **p50** / p95 | at P* target | Choke group (share of draws) |
|---|---|---|---|
| Fish traps | 0.97 / **1.33** / 1.85 | 1.27 | hogfish (37%), queen triggerfish (29%), porgies (27%) |
| Lobster traps | 1.08 / **1.65** / 3.11 | 1.55 | queen triggerfish (82%), spiny lobster (15%) |
| Diving | 0.98 / **1.42** / 2.21 | 1.35 | hogfish (48%), queen triggerfish (42%) |
| Hook and line | 1.01 / **1.42** / 3.72 | 1.37 | queen triggerfish (62%), porgies (37%) |

### Scenarios (final projection years 2043–2045)

| Scenario | Lobster MMSY | QT MMSY | QT P(overfishing) | Parrotfish 2 MMSY | Hogfish MMSY |
|---|---|---|---|---|---|
| Baseline | 311.8k | 28.7k | 0.032 | 397.9k | 12.5k |
| Climate SSP2-4.5 | 311.6k | 25.8k | 0.043 | 309.8k | 11.3k |
| Climate SSP5-8.5 | 309.6k | 23.8k | 0.062 | 192.1k | 10.1k |
| Major hurricane 2030 | 310.3k | 28.6k | 0.032 | 395.2k | 12.5k |
| Trap effort +30% | 311.8k | 28.7k | 0.062 | 397.9k | 12.5k |
| Trap reduction −25% | 311.8k | 28.7k | 0.023 | 397.9k | 12.5k |
| Coral disease (SCTLD) | 308.6k | 28.6k | 0.032 | 394.6k | 12.5k |
| Combined stress | 308.4k | 23.7k | 0.083 | 191.9k | 10.1k |

Table: `spf_scenario_reference_points.csv`; figure `fig03`.

### Robustness and validation

| Check | Result | File |
|---|---|---|
| SIR diagnostics | ESS 86 of 48,000; 316 unique draws; max weight 4.7%; top-10 draws hold 25% of weight | `sir_diagnostics_summary.csv` |
| Simulation self-test (100 replicates; data simulated from known truths, refitted) | 90% interval coverage 0.77–0.91; median bias −0.1% (QT), +10% (lobster), −31% (porgies), −37% (hogfish) | `self_test_summary.csv` |
| Leave-one-benchmark-out | QT OFL predicted 28.7k (90% 14.8–76.3k) vs 22.8k: **pass**. Lobster OFL predicted 1.06M vs 164k: **fail** (lobster scale is not identifiable without its OFL; the Opitz 1980s lobster biomass prior is too high) | `validation_loo_benchmarks.csv`, `fig13` |
| Held-out indices (placeholder series) | 90% coverage 1.0; MASE 1.2–1.8 (synthetic data, so this only demonstrates the machinery) | `validation_out_of_sample.csv`, `fig12` |
| Global sensitivity (rank regression on prior draws) | total focus MMSY driven by pavement TTE, invertebrate-competitor biomass, pavement NPP and food-web fraction; group MMSY by its own biomass partition, then pavement TTE and P/B | `global_sensitivity.csv`, `fig11` |
| Structural ensemble (10 variants: x_msy 0.3/0.4, TTE 0.10/0.20, no fishing feedback, no thermal effects, food-web fraction ×0.67/×1.5, satellite pelagic NPP) | between-variant CV of MMSY: lobster 0.10, QT 0.09, grunts 0.26, Parrotfish 2 0.45. Pooled lobster MMSY 178–419k (p50 280k); QT 15.2–42.3k (p50 25.7k) | `structural_ensemble_*.csv`, `fig15` |
| Ecosystem overfishing (Link & Watson 2019) | Ryther index 0.59–0.63 t km⁻² yr⁻¹ (below the 1 t threshold); Fogarty 0.54–0.59‰ (inside the 0.22–0.92‰ caution band), 2022–2024 | `ecosystem_overfishing_indices.csv`, `fig14` |

### Interpretation

1. **The two assessed trap stocks are anchored and structurally robust.** Lobster MMSY is 1.7× its OFL and queen triggerfish 1.1×. Their MMSY changes by only about 10% across the ten structural variants. Part of the lobster gap is expected: OFL is a catch at F_MSY given the current stock, while MMSY is an equilibrium cap. The leave-one-out test shows that without its OFL the lobster scale is not identified, so SEDAR 57 inputs matter more than any other single data item.
2. **Queen triggerfish, not lobster, is the binding constraint on lobster traps.** Lobster traps could expand about 65% (median) before a calibrated group reaches its gear-specific MMSY, and the choke in 82% of draws is queen triggerfish bycatch. Fish traps and diving are limited first by hogfish and porgies. These small, low-MMSY groups, which have the largest self-test bias (−31% to −37%, conservative), are where better survey data pays off most for trap management.
3. **Climate is the largest projected stressor.** By 2043–2045, SSP5-8.5 cuts Parrotfish 2 MMSY by about half and queen triggerfish, hogfish and porgies by 17–19%, through their AquaMaps thermal curves. Lobster has no thermal curve yet (SeaLifeBase pending), so its insensitivity is an assumption, not a result. Under status-quo projected effort, combined stress raises queen-triggerfish P(overfishing) from 3% to 8%.
4. **Unassessed complex scale remains prior-driven.** Grunts, Snapper 3 and Parrotfish 2 have MMSY well above their ACLs, with wide intervals and high structural sensitivity (CV 0.26–0.45). NCRMP biomass densities (data need C1) are the most direct way to narrow them.
5. **No calibrated group is overfishing at current placeholder catch**, but the ecosystem-level Fogarty index sits in the caution band throughout 1983–2024.

### Figures

All figures are in `Projects/StCroix_Trap/output/figures/`:

| Figure | Content |
|---|---|
| `fig01` | energy budget |
| `fig02` | group MMSY and benchmarks |
| `fig03` | scenario stoplight (2043–2045) |
| `fig04`, `fig05` | trajectories |
| `fig06` | coral quality |
| `fig07` | effort ceilings |
| `fig08` | SIR prior vs posterior |
| `fig09` | environmental drivers |
| `fig10` | hurricanes |
| `fig11` | global sensitivity (total focus MMSY, lobster, queen triggerfish) |
| `fig12` | held-out index validation |
| `fig13` | leave-one-benchmark-out |
| `fig14` | ecosystem-overfishing indices |
| `fig15` | structural ensemble |

The same results are presented in `docs/presentation/CaribTTM_StCroix_Trap.pptx`. The review-readiness summary is in [Review Readiness](review_readiness.md).

## 5. What would change the answers most

1. **SEDAR 57 / SEDAR 80 inputs (indices, B₀, M).** Leave-one-out shows the lobster scale depends on its OFL; standardized CPUE series used as *fit* indices would add a second, independent constraint.
2. **Real landings by gear and the expansion factor.** These set catch feasibility (the lower bound on productivity) and the gear choke analysis.
3. **NCRMP biomass densities** for hogfish, porgies, queen triggerfish and the unassessed complexes, and for the invertebrate competitors that GSA identifies as a top driver.
4. **Pavement production, TTE and food-web fraction.** Colonized pavement is 53% of the benthic domain and dominates the GSA.
5. **A thermal response for spiny lobster.**

## 6. Data-replacement checklist

See [Data Needs](data_needs.md) for the itemized list (A1–E4) with files, columns, priorities and suggested sources.

