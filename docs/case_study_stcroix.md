# Case Study: The St. Croix Trap Fishery

> **Read this first.** This case study demonstrates the full CaribTTM workflow with the best *public* data available for St. Croix, USVI. Several key inputs are flagged placeholders: confidential trip-ticket landings and effort, standardized assessment indices, and most biological priors. The structure, data flows and diagnostics are real. The **numbers are illustrative and are not management advice** until the placeholders listed in Section 7 are replaced.

## 1. Why St. Croix and why traps

* **An isolated platform.** St. Croix is a single insular shelf, separated from the Puerto Rico–Virgin Islands platform by deep trenches. Under the island-based Fishery Management Plans it is its own management unit (St. Croix FMP, effective 2022). The model domain is therefore the St. Croix shelf, including Lang Bank, Buck Island Reef National Monument and East End Marine Park.
* **A trap-dependent, multispecies fishery.** Fish and lobster traps catch dozens of species simultaneously, among them spiny lobster, queen triggerfish, parrotfishes, surgeonfishes, grunts, snappers, groupers, squirrelfishes, boxfishes and angelfishes. Single-species ACLs are hard to apply to such a gear. Gear-level effort controls informed by multispecies caps (SEDAR103-DW-20) are a natural fit.
* **Two assessed stocks provide anchors.** SEDAR 57 (spiny lobster) and SEDAR 80 (queen triggerfish) give assessment-based OFLs. These anchor the absolute scale of the trophic model, while the energy budget constrains the many unassessed complexes.

## 2. Domain and data

| Component | St. Croix configuration | Source / status |
|---|---|---|
| Period | history 1983–2024 (landings-driven; start of the commercial landings series used in SEDAR 80), projection 2025–2045 (effort-driven); reference years 2022–2024 | user choice |
| Benthic habitat areas | coral reef 41.7 km², colonized pavement 192.0, seagrass 59.4, macroalgae 13.1, sand/mud 12.7, mangrove 1.33, deeper unmapped shelf 388.3 | **API**: NOAA NCCOS USVI 2002 benthic habitat map (ArcGIS REST), class-mapping CVs 0.15–0.5 |
| Benthic NPP | habitat-specific Caribbean net production: reef 1500 t C km⁻² yr⁻¹, pavement 181 mmol O₂ m⁻² d⁻¹, seagrass 4.2 g DW m⁻² d⁻¹, macroalgae 582 mmol O₂, sand 7 mmol O₂, mangrove 400 mmol O₂ | literature synthesis (Input Dictionary), CVs 0.3–0.6 |
| Pelagic area and NPP | shelf + ~10 km buffer (1,500 km²); 85 g C m⁻² yr⁻¹; annual index 1998–2024 | SEDAR103-DW-20 App. A magnitude; **API** index computed with VGPM + Eppley-VGPM from OC-CCI chlorophyll, CRW SST and SeaWiFS/MODIS PAR |
| SST, DHW | annual mean SST 1983–2024 (CRW monthly + Pathfinder); annual max DHW 1985–2024 | **API**: NOAA Coral Reef Watch / Pathfinder (PIFSC OceanWatch ERDDAP), spatial SD as observation error |
| Climate projections | SSP2-4.5 and SSP5-8.5 SST and NPP index, 2025–2045 | **API**: CMIP6 CESM2 + ACCESS-ESM1-5 (PIFSC), delta method |
| Water clarity, chlorophyll | OC-CCI Kd490, chlorophyll-a (1998–) | **API** (optional covariates) |
| Coral cover | ≈22% (1983, placeholder) → 15% (2000) → 8% (2013) → 4–4.5% (2019–2021) → 3% (2024) | approximate 2000– (NCRMP/TCRMP; SCTLD from 2021), sd 0.01; placeholder before 2000, sd 0.03 |
| Hurricanes | 23 HURDAT2 storms within 200 km, 1983–2024 (Maria 2017 = 1.00, Lenny 1999 = 0.98, Hugo 1989 = 0.84, Georges 1998 and Marilyn 1995 = 0.54, Irma 2017 = 0.54, …), intensity CV 0.3 | **API**: NHC HURDAT2; loss magnitudes per event are placeholders |
| Functional groups | 19 FMP groups (all St. Croix FMP complexes relevant to the platform, with Parrotfish 1 prohibited) + 10 non-fishery competitor groups | P/B, partition priors mostly placeholders |
| Gears | fish traps, lobster traps, diving, hook and line | — |
| Landings / effort | by year × gear × group (lb, trips), landings CV 0.2, effort CV 0.1, expansion factor 1.5 ± 25% | **placeholders** (queen conch 2021 = 30,535 lb is published) |
| Indices (SIR) | SEDAR 57 lobster trap CPUE, SEDAR 80 queen triggerfish trap CPUE, NCRMP parrotfish density | **placeholders** (λ = 0.5) |
| Benchmarks | lobster OFL 163,823 lb, queen triggerfish OFL 22,773 lb (used in SIR, CV 0.4); ACLs for comparison | published |
| Uncertainty | 5,000 prior draws; observation error on covariates, landings (CV 0.2), effort (CV 0.1) and hurricane intensity; SIR on indices, OFLs and catch feasibility | — |

### Prior-predictive check (catch feasibility)

With the first set of placeholder partition priors, 56–68% of prior draws could not support the observed (expanded) landings of spiny lobster, queen triggerfish, queen conch and the deepwater snappers. In those draws the simulated stocks collapsed under the historical catch. Their priors were re-centred upward and flagged in `groups.csv`. The catch-feasibility likelihood in the SIR now down-weights any remaining draws in which a stock cannot supply a year's landings. This is the stock-reduction-analysis logic, and it is what bounds productivity from below for unassessed groups.

## 3. Running the case study

```r
devtools::load_all(".")
acquire_project_data("Projects/StCroix_Trap")   # optional: refresh API data (results are committed)
source("Projects/StCroix_Trap/run_stcroix.R")   # or: Rscript Projects/StCroix_Trap/run_stcroix.R
```

Outputs are written to `Projects/StCroix_Trap/output/tables` (SPF hand-off tables and diagnostics) and `output/figures`.

## 4. Results (Baseline, reference years 2022–2024)

The full run (5,000 prior draws, 8 scenarios) took 10.7 minutes. SIR retained 214 unique draws, an effective sample size of about 42. Every retained draw supports all historical landings, so the catch-feasibility shortfall is 0 for every group. Values are medians unless stated, in lb yr⁻¹; landings are expanded by 1.5.

### Group MMSY

| Group | MMSY p05 | **MMSY p50** | MMSY p95 | P* target (40%) | Current catch | B/BMSY | Benchmark |
|---|---|---|---|---|---|---|---|
| Spiny lobster | 126,478 | **219,812** | 337,909 | 203,611 | 113,149 | 1.83 | OFL 163,823 |
| Queen triggerfish | 23,589 | **35,798** | 54,778 | 33,923 | 15,216 | 1.71 | OFL 22,773 |
| Queen conch | 50,926 | **118,957** | 357,134 | 113,466 | 31,349 | 1.84 | ACL 50,000 |
| Parrotfish 2 | 199,884 | **699,847** | 1,811,875 | 619,817 | 73,955 | 2.02 | ACL 72,365 |
| Surgeonfishes | 292,761 | **904,026** | 2,359,703 | 797,187 | 26,426 | 2.00 | — |
| Grunts | 307,638 | **753,353** | 2,030,183 | 681,146 | 20,697 | 2.12 | — |
| Snapper 3 (shallow) | 60,748 | **147,088** | 392,711 | 131,575 | 13,964 | 2.07 | ACL 14,156 |
| Yellowtail snapper | 81,269 | **221,936** | 761,612 | 177,361 | 42,652 | 2.01 | — |
| Snapper 1 & 2 (deep) | 73,408 | **239,980** | 729,448 | 207,866 | 48,538 | 2.07 | ACL 69,366 |
| Groupers (shallow) | 53,647 | **109,422** | 543,860 | 100,526 | 14,615 | 2.08 | — |
| Squirrelfishes | 122,793 | **318,342** | 1,248,380 | 275,338 | 9,328 | 2.12 | — |
| Goatfishes | 28,712 | **75,968** | 178,111 | 68,347 | 5,483 | 1.89 | — |
| Boxfishes | 13,776 | **43,263** | 104,324 | 40,288 | 7,583 | 1.79 | — |
| Angelfishes | 35,121 | **97,072** | 241,734 | 85,825 | 4,060 | 2.00 | — |
| Wrasses (hogfish) | 47,403 | **140,942** | 334,314 | 111,396 | 7,152 | 1.92 | — |
| Jacks | 63,481 | **158,318** | 473,623 | 119,822 | 20,485 | 2.04 | — |
| Porgies | 9,005 | **35,073** | 105,525 | 31,272 | 3,314 | 1.89 | — |
| Pelagics (dolphin, wahoo) | 104,733 | **259,356** | 922,929 | 246,242 | 92,604 | 2.18 | ACL 86,633 |
| Parrotfish 1 | (prohibited) | 200,579 | | | 0 | 2.07 | ABC = 0 |

### Guild MMSY

| Guild | MMSY p50 | Current catch | P(overfishing) |
|---|---|---|---|
| Herbivores | 2.11 M | 130,319 | 0 |
| Invertivores | 1.90 M | 189,724 | 0 |
| Piscivores | 0.61 M | 83,864 | 0 |
| Mesopredators | 0.40 M | 57,646 | 0 |
| Pelagic piscivores | 0.26 M | 92,604 | 0.0085 |

### Gear effort ceilings and choke groups

| Gear | Effort multiplier at median MMSY | at P* target | Choke group (share of draws) |
|---|---|---|---|
| Fish traps | 1.65 | 1.63 | spiny lobster (58%) |
| Lobster traps | 1.69 | 1.63 | spiny lobster (62%) |
| Diving | 1.64 | 1.57 | spiny lobster (65%) |
| Hook and line | 1.84 | 1.70 | queen triggerfish (48%) |

### Interpretation

1. **The assessed stocks are anchored, and slightly above their OFLs.** SIR pulls lobster and queen triggerfish MMSY toward the SEDAR OFLs. The medians remain 1.3× (lobster) and 1.6× (queen triggerfish) the OFLs. Part of the gap is expected: OFL is a catch at F_MSY given the current stock, and the TTM's MMSY is an equilibrium cap. Part comes from the catch-feasibility constraint, because the placeholder landings, expanded by 1.5, require productive stocks.
2. **Spiny lobster is the choke group for all trap and dive gears.** Effort could rise by about 60% before lobster catch reached its gear-specific MMSY. This is the most robust management-relevant result, because lobster is the best-anchored group.
3. **The scale of unassessed complexes is placeholder-driven.** For grunts, surgeonfishes, Parrotfish 2 and Snapper 3, MMSY exceeds the ACLs by about 10×. That reflects the energy share these groups receive under the placeholder partition priors. Once NCRMP biomass densities replace those priors (data needs C1, C4), these numbers will drop substantially.
4. **Every stock is "Below target" in every scenario.** In 2043–2045 catch is well below MMSY even under SSP5-8.5, a 2030 hurricane or a 30% trap-effort surge. The scenarios do move B/BMSY and MMSY in the expected directions (see figures). The absence of status changes says the placeholder caps are generous, not that the fishery is risk-free.

### Figures

All figures are in `Projects/StCroix_Trap/output/figures/`:

| Figure | Content |
|---|---|
| `fig01` | energy budget |
| `fig02` | group MMSY and benchmarks |
| `fig03` | scenario stoplight |
| `fig04`, `fig05` | trajectories |
| `fig06` | coral quality |
| `fig07` | effort ceilings |
| `fig08` | SIR prior vs posterior |
| `fig09` | environmental drivers |
| `fig10` | hurricanes |

The same results are presented in `docs/presentation/CaribTTM_StCroix_Trap.pptx`.

## 5. What would change the answers most

1. **Real landings by gear and the expansion factor.** These set catch feasibility (the lower bound) and the gear allocation.
2. **NCRMP biomass densities for partitioning and the non-fishery competitors.** These set the share of energy, and so the absolute MMSY, of unassessed complexes.
3. **Deeper-shelf habitat composition and productivity.** This is 388 km² of the 708 km² benthic domain.
4. **Food-web fraction and TTE.** These scale all groups together.

## 6. Data-replacement checklist

See [Data Needs](data_needs.md) for the itemized list (A1–E4) with files, columns, priorities and suggested sources.

