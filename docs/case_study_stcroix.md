# Case Study: The St. Croix Trap Fishery

> **Read this first.** This case study demonstrates the full CaribTTM workflow with the best *public* data available for St. Croix, USVI. Several key inputs are flagged placeholders: confidential trip-ticket landings and effort, standardized assessment indices, and most biological priors. The structure, data flows and diagnostics are real. The **numbers are illustrative and are not management advice** until the placeholders listed in Section 7 are replaced.

## 1. Why St. Croix and why traps

* **An isolated platform.** St. Croix is a single insular shelf, separated from the Puerto Rico–Virgin Islands platform by deep trenches. Under the island-based Fishery Management Plans it is its own management unit (St. Croix FMP, effective 2022). The model domain is therefore the St. Croix shelf, including Lang Bank, Buck Island Reef National Monument and East End Marine Park.
* **A trap-dependent, multispecies fishery.** Fish and lobster traps catch dozens of species simultaneously, among them spiny lobster, queen triggerfish, parrotfishes, surgeonfishes, grunts, snappers, groupers, squirrelfishes, boxfishes and angelfishes. Single-species ACLs are hard to apply to such a gear. Gear-level effort controls informed by multispecies caps (SEDAR103-DW-20) are a natural fit.
* **Two assessed stocks provide anchors.** SEDAR 57 (spiny lobster) and SEDAR 80 (queen triggerfish) give assessment-based OFLs. These anchor the absolute scale of the trophic model, while the energy budget constrains the many unassessed complexes.

## 2. Domain and data

| Component | St. Croix configuration | Source / status |
|---|---|---|
| Period | history 2010–2024 (landings-driven; the last ~15 years), projection 2025–2045 (effort-driven); reference years 2022–2024 | user choice |
| Benthic habitat areas | coral reef 41.7 km², colonized pavement 192.0, seagrass 59.4, macroalgae 13.1, sand/mud 12.7, mangrove 1.33, deeper unmapped shelf 388.3 | **API**: NOAA NCCOS USVI 2002 benthic habitat map (ArcGIS REST), class-mapping CVs 0.15–0.5 |
| Benthic NPP | habitat-specific Caribbean net production: reef 1500 t C km⁻² yr⁻¹, pavement 181 mmol O₂ m⁻² d⁻¹, seagrass 4.2 g DW m⁻² d⁻¹, macroalgae 582 mmol O₂, sand 7 mmol O₂, mangrove 400 mmol O₂ | literature synthesis (Input Dictionary), CVs 0.3–0.6 |
| Pelagic area and NPP | shelf + ~10 km buffer (1,500 km²); 85 g C m⁻² yr⁻¹; annual index | SEDAR103-DW-20 App. A magnitude; **API** index (satellite NPP ensemble; VIIRS chlorophyll proxy when PFEG ERDDAP is unreachable) |
| SST, DHW | annual mean CoralTemp SST; annual max DHW | **API**: NOAA Coral Reef Watch (NESDIS ERDDAP), spatial SD as observation error |
| Water clarity, chlorophyll | VIIRS Kd490, chlorophyll-a (2012–) | **API** (optional covariates) |
| Coral cover | ≈15% (2000) → 8% (2013) → 4–4.5% (2019–2021) → 3% (2024), sd 0.01 | approximate (NCRMP/TCRMP; SCTLD from 2021) |
| Hurricanes | all HURDAT2 storms within 200 km, 2010–2024 (Maria 2017 = 1.0, Irma 2017 = 0.54, Dorian 2019 = 0.30, Irene 2011 = 0.25, …), intensity CV 0.3 | **API**: NHC HURDAT2; loss magnitudes per event are placeholders |
| Functional groups | 19 FMP groups (all St. Croix FMP complexes relevant to the platform, with Parrotfish 1 prohibited) + 10 non-fishery competitor groups | P/B, partition priors mostly placeholders |
| Gears | fish traps, lobster traps, diving, hook and line | — |
| Landings / effort | by year × gear × group (lb, trips), landings CV 0.2, effort CV 0.1, expansion factor 1.5 ± 25% | **placeholders** (queen conch 2021 = 30,535 lb is published) |
| Indices (SIR) | SEDAR 57 lobster trap CPUE, SEDAR 80 queen triggerfish trap CPUE, NCRMP parrotfish density | **placeholders** (λ = 0.5) |
| Benchmarks | lobster OFL 163,823 lb, queen triggerfish OFL 22,773 lb (used in SIR, CV 0.4); ACLs for comparison | published |

### Prior-predictive check (catch feasibility)

With the first set of placeholder partition priors, 56–68% of prior draws could not support the observed (expanded) landings of spiny lobster, queen triggerfish, queen conch and the deepwater snappers. In those draws the simulated stocks collapsed under the historical catch. Their priors were re-centred upward and flagged in `groups.csv`. The catch-feasibility likelihood in the SIR now down-weights any remaining draws in which a stock cannot supply a year's landings. This is the stock-reduction-analysis logic, and it is what bounds productivity from below for unassessed groups.

## 3. Running the case study

```r
devtools::load_all(".")
acquire_project_data("Projects/StCroix_Trap")   # optional: refresh API data (results are committed)
source("Projects/StCroix_Trap/run_stcroix.R")   # or: Rscript Projects/StCroix_Trap/run_stcroix.R
```

Outputs are written to `Projects/StCroix_Trap/output/tables` (SPF hand-off tables and diagnostics) and `output/figures`.

<!-- RESULTS -->
