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
| Uncertainty | 5 adaptive importance-sampling rounds × 12,000 = 60,000 prior draws (4 cores); every input CV and observation error sampled; SIR on OFLs and catch feasibility (CV 0.25); Pareto-smoothed or truncated weights | — |

### Prior-predictive check (catch feasibility)

Under wide priors (partition CV 0.5, or 1.0 for poorly known groups), many prior draws cannot supply the observed (expanded) landings: the simulated stock collapses under the historical catch. The catch-feasibility likelihood (stock-reduction-analysis logic) down-weights those draws, and is what bounds productivity from below for unassessed groups. Plain SIR on 5,000 draws collapsed to an effective sample size (ESS) of 1, because few draws satisfied all 15 focus groups simultaneously. Adaptive importance sampling fixes this: after each round, proposals are re-centred only for the groups in conflict, and the final weights are corrected exactly for the mixture of proposals (Owen & Zhou 2000).

## 3. Running the case study

```r
devtools::load_all(".")
acquire_project_data("Projects/StCroix_Trap")   # optional: refresh API data (results are committed)
source("Projects/StCroix_Trap/run_stcroix.R")   # main run + robustness + validation + MSE (≈ 45 min on 4 cores)
source("Projects/StCroix_Trap/run_structural_ensemble.R")   # 10 structural variants (≈ 54 min)
```

Outputs are written to `Projects/StCroix_Trap/output/tables` (SPF hand-off tables and diagnostics) and `output/figures`.

## 4. Results

Full tables, figures and discussion are in the [Technical Memorandum](technical_memo/CaribTTM_Technical_Memorandum.md), Sections 10–11. Values are medians in lb yr⁻¹, Baseline, 2022–2024, with landings expanded by 1.5. **Absolute values for unassessed groups depend on placeholder landings and priors.**

**Calibration.** 60,000 prior draws in five adaptive rounds took 42.5 minutes. The raw ESS was 20. The weight tail was too heavy for Pareto smoothing (k̂ = 2.2), so the weights were truncated: ESS 125, 307 unique draws, largest weight 1.3%. All retained draws support all historical landings.

| Group | MMSY p05 | **MMSY p50** | MMSY p95 | Catch | B/B_MSY | P(overfishing) | Benchmark |
|---|---|---|---|---|---|---|---|
| Spiny lobster | 120,321 | **249,798** | 441,273 | 121,353 | 2.44 | 0.07 | OFL 163,823 |
| Queen triggerfish | 12,541 | **23,497** | 37,329 | 15,466 | 1.83 | 0.16 | OFL 22,773 |
| Parrotfish 2 | 64,134 | **155,615** | 570,872 | 70,634 | 3.06 | 0.09 | ACL 72,365 |
| Surgeonfishes | 53,671 | **124,686** | 501,952 | 26,847 | 2.57 | 0.00 | — |
| Grunts | 83,145 | **315,880** | 881,866 | 21,707 | 3.04 | 0 | — |
| Snapper 3 (shallow) | 44,124 | **187,034** | 467,583 | 14,089 | 2.92 | 0 | ACL 14,156 |
| Groupers (shallow) | 19,718 | **55,711** | 143,712 | 15,247 | 2.86 | 0.01 | — |
| Squirrelfishes | 18,253 | **57,639** | 222,069 | 9,400 | 3.06 | 0.00 | — |
| Goatfishes | 6,805 | **23,597** | 68,086 | 5,757 | 2.55 | 0.02 | — |
| Boxfishes | 13,577 | **33,743** | 135,635 | 6,918 | 2.80 | 0.01 | — |
| Angelfishes | 29,389 | **134,861** | 487,461 | 4,113 | 2.76 | 0 | — |
| Wrasses (hogfish) | 4,157 | **8,201** | 26,263 | 7,003 | 2.03 | **0.35** | — |
| Porgies | 2,698 | **5,890** | 21,926 | 3,338 | 1.98 | **0.16** | — |

B/B_MSY uses each group's per-recruit B_MSY/K (≈ 0.29–0.38). Conch, yellowtail, deepwater snappers, jacks and pelagics are outside the trap focus and uncalibrated.

**Gear effort ceilings** (multipliers of 2022–2024 effort):

| Gear | Ceiling at median MMSY | Ceiling at P* target | Choke group |
|---|---|---|---|
| Fish traps | 0.95 | 0.90 | hogfish (54%) |
| Dive | 0.96 | 0.92 | hogfish (56%) |
| Lobster traps | 1.28 | 1.21 | queen triggerfish (73%) |
| Hook and line | 1.21 | 1.10 | queen triggerfish (61%) |

**Scenarios, 2043–2045.**

| Scenario | Lobster MMSY | QT MMSY | QT P(OF) | Hogfish P(OF) | Parrotfish 2 MMSY |
|---|---|---|---|---|---|
| Baseline | 317k | 27.6k | 0.08 | 0.12 | 194k |
| Climate SSP2-4.5 | 284k | 24.8k | 0.09 | 0.13 | 155k |
| Climate SSP5-8.5 | 239k | 22.5k | 0.10 | 0.16 | 112k |
| Major hurricane 2030 | 316k | 27.5k | 0.09 | 0.12 | 194k |
| Trap effort +30% | 317k | 27.6k | 0.11 | 0.14 | 194k |
| Trap effort −25% | 318k | 27.6k | 0.06 | 0.12 | 194k |
| Coral disease (SCTLD) | 313k | 27.5k | 0.08 | 0.12 | 193k |
| Combined stress | 238k | 22.3k | 0.16 | 0.18 | 111k |
| No-take expansion | 318k | 27.6k | 0.06 | 0.09 | 194k |
| Open-access effort | 318k | 27.6k | 0.20 | 0.24 | 194k |
| Dive effort +50% | 316k | 27.6k | 0.08 | 0.16 | 194k |
| Seagrass loss (*Halophila*) | 276k | 27.3k | 0.09 | 0.14 | 177k |

**Validation and robustness:**
* **Leave-one-benchmark-out.** Queen triggerfish passes: predicted 16,400 lb (90% 10,300–47,900) against an OFL of 22,773. Spiny lobster fails: predicted 774,000 lb.
* **Self-test.** 90% coverage 0.79–0.93; bias −19% to +12% (queen triggerfish 0%, lobster −6%).
* **Independent catch-only comparator.** Queen triggerfish 28,400 lb; lobster 205,000 lb.
* **Value of information** (share of total-focus MMSY variance): biomass surveys 24%, benthic NPP 18%, transfer efficiency 18%, food-web fraction 10%.
* **Closed-loop MSE.** P(any focus group < 0.5 B_MSY in 2036–2045):

  | Procedure | Probability |
  |---|---|
  | Status quo | 0.33 |
  | 25% trap reduction | 0.31 |
  | SPF effort-ceiling rule | 0.31 |
  | SPF rule, +50% biased assessment | 0.41 |
  | Catch-rate rule | 0.41 |

* **Ecosystem indices.** Ryther 0.61–0.66 t km⁻² yr⁻¹ (below 1); Fogarty 0.56–0.60‰ (caution band).
* **Ghost fishing** removed 3–11% of landings over 1983–2024.

### Interpretation

1. **Theory-based yield priors give small trap-bycatch groups much less headroom.** Per-recruit x and Pella–Tomlinson production make hogfish, porgies and queen triggerfish the binding constraints. Fish-trap and dive effort are at their multispecies ceilings.
2. **The assessed stocks are reproduced.** Queen triggerfish matches its OFL and passes out-of-sample validation. Lobster scale depends on its OFL.
3. **Climate is the largest projected stressor**, cutting MMSY by 16–43% under SSP5-8.5.
4. **No-take expansion and trap reductions lower choke-group risk by about a quarter.** Open-access effort and optimistic assessments raise it substantially.

### Figures

All figures are in `Projects/StCroix_Trap/output/figures/`:

| Figure | Content |
|---|---|
| fig01 | energy budget |
| fig02 | group MMSY |
| fig03 | scenario status |
| fig04–05 | trajectories |
| fig06 | coral quality |
| fig07 | effort ceilings |
| fig08 | SIR prior vs posterior |
| fig09 | environment |
| fig10 | hurricanes |
| fig11 | sensitivity |
| fig12 | held-out indices |
| fig13 | leave-one-out |
| fig14 | ecosystem indices |
| fig15 | structural ensemble |
| fig16 | catch-only comparator |
| fig17 | value of information |
| fig18 | MSE |

## 5. What would change the answers most

1. **SEDAR 57 / SEDAR 80 inputs (indices, B₀, M).** Leave-one-out shows the lobster scale depends on its OFL; standardized CPUE series used as *fit* indices would add a second, independent constraint.
2. **Real landings by gear and the expansion factor.** These set catch feasibility (the lower bound on productivity) and the gear choke analysis.
3. **NCRMP biomass densities** for hogfish, porgies, queen triggerfish and the unassessed complexes, and for the invertebrate competitors that GSA identifies as a top driver.
4. **Pavement production, TTE and food-web fraction.** Colonized pavement is 53% of the benthic domain and dominates the GSA.
5. **A thermal response for spiny lobster.**

## 6. Data-replacement checklist

See [Data Needs](data_needs.md) for the itemized list (A1–E4) with files, columns, priorities and suggested sources.

