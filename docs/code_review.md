# Code and Report Review

**Scope.** This review covered:
* the CaribTTM R package (`R/`, 16 files);
* the St. Croix project inputs (`Projects/StCroix_Trap/data`, `data-raw`);
* the documentation (`docs/`).

**Methods:**
* Line-by-line reading of the simulator, likelihoods, sampler and decision tools.
* Unit tests: 124 expectations, 0 failures. New tests cover each corrected mechanism.
* An automated audit matching every author–year citation in the docs, code and input CSVs against `references.md`.
* A provenance audit of every input row (`data_status` and `reference`).
* Spot verification of high-impact citations against the published record.

## 1. Findings and resolutions

Severity: **High** = could change a management-relevant number or mislead a reviewer; **Medium** = could bias a diagnostic or report; **Low** = clarity.

| # | Area | Finding | Severity | Resolution |
|---|---|---|---|---|
| 1 | Theory | MMSY and K used Ecopath P/B (= Z = M + F for an exploited system) where Gulland's form needs natural mortality. | High | Fixed: `m_nat` gives K = P/M and MMSY = x·M·K. P/B keeps its role as the production share. |
| 2 | Theory | x = 0.5 for every group, which is an upper bound under age-structured theory. | High | Fixed: per-recruit priors (x = 0.17–0.32) built from FishBase/SEDAR traits. |
| 3 | Theory | Schaefer dynamics force B_MSY = K/2, while the per-recruit analysis implies B_MSY/B₀ ≈ 0.29–0.38, so status calls were inconsistent with the yield theory. | High | Fixed: exact Pella–Tomlinson integrator with group `bmsy_k`. B/B_MSY uses it. Variant `schaefer_shape` kept. |
| 4 | Sampler | Adaptive proposals were fitted with the joint importance correction, which injects noise from other groups and stalled adaptation. | High | Fixed: per-group marginal correction. Raw ESS rose 2.6× in the pilot at the same budget. |
| 5 | Sampler | Pareto smoothing with a degenerate tail fit (k = 352) flattened the top weights and inflated ESS from 5 to 80. | High | Fixed: when k ≥ 0.7, truncated importance sampling (Ionides 2008) is used and flagged (`weight_method`). |
| 6 | Diagnostics | Self-test truths, global sensitivity, value of information, posterior shrinkage, the "prior" curve in fig08 and `prior_reference_points()` all treated proposal-mixture draws as prior draws. | Medium | Fixed: prior/proposal importance ratios (resampling or weights) are applied throughout. |
| 7 | MSE | The summary statistic was the mean share of years with any group overfished, but it was labelled as a probability. | Medium | Fixed: `P_any_focus_overfished` (any year) and `annual_P_any_focus_overfished` reported separately. |
| 8 | Reports | Hand-copied St. Croix value tables in the input dictionary were stale (lobster P/B 0.8 and x 0.5; deep shelf 388 km² with placeholder NPP). | High | Fixed: tables are generated from the project files by `docs/build_input_tables.R`. |
| 9 | Citations | "Renchen et al. 2014, Mar. Pollut. Bull." was cited but no such paper exists. | High | Fixed: replaced by Renchen et al. (2010, GCFI), Clark et al. (2012, NOAA TM NOS NCCOS 147) and Arthur et al. (2014, Mar. Pollut. Bull.). |
| 10 | Citations | The two Juman (2005) papers (mangrove; *Thalassia*) were merged into one reference. The seagrass NPP source was ambiguous. | Medium | Fixed: split into 2005a/2005b. Seagrass value (4.2 g DW m⁻² d⁻¹) confirmed in Juman 2005a and flagged as the high end of the CARICOMP range (an assumption for St. Croix). |
| 11 | Citations | Reef NPP range attributed to "Caribbean" values from Nair & Pillai (1972), which are Indian Ocean reefs. | Low | Fixed: labelled as Indian Ocean bracketing values. |
| 12 | Citations | Nine primary sources cited in code or data were missing from the reference list: Eppley 1972, Forsythe 1995, Gower 2013, Ionides 2008, Morel & Berthon 1989, Wang & Hu 2016, Zhang & Stephens 2009, Allen 1971, Pella & Tomlinson 1969. Also missing: Fletcher 1978, Christensen & Walters 2004, Link & Watson 2019, Duarte 1991, Thorson et al. 2012 and SeaLifeBase. | Medium | Fixed: added. The audit now finds no missing author–year pairs other than false positives. |
| 13 | Citations | Ryther thresholds were marked "verify". | Low | Verified: about 1 t km⁻² yr⁻¹ is the practical not-to-exceed level (range 0.3–1.1) and 3 is extreme (Link & Watson 2019). |
| 14 | Provenance | `habitat_preference.csv` and `diet.csv` had no status or source columns. Eleven settings and eight scenario rows had no note. One group was sourced only as "literature-typical". | Medium | Fixed: every row now states its basis. Expert-judgement rows are labelled `ASSUMPTION`. |
| 15 | Provenance | Placeholder process parameters (nursery, open recruitment) were hidden behind group-level "derived" flags. | Medium | Fixed: `data_status` lists every placeholder component of each row. |
| 16 | Reports | No single place listed the assumptions. | High | Added `docs/assumptions.md`: 49 assumptions with value, basis, type (theory / data / literature / expert / placeholder / convention) and how each is tested. |
| 17 | Calibration | OFLs are used as observations of MMSY, but an OFL is F_MSY × current B. | Medium (conceptual) | Documented as assumption C1 with CV 0.4. Leave-one-benchmark-out quantifies the consequence. |
| 18 | Code | Doc comment referred to non-existent settings `ryther_thresholds` and `fogarty_thresholds`. | Low | Fixed. |
| 19 | Performance | Stage 3 (all scenarios for retained draws) ran on one core with 13 scenarios. | Low | Parallelized (reproducible per-draw RNG streams; tests pass). |

## 2. Elements that existing data can support, now added

| Element | Data | Status |
|---|---|---|
| Spiny lobster and queen conch thermal performance | SeaLifeBase / AquaMaps temperature preference (*P. argus* 23.7 / 26.7 / 28.0 °C; *A. gigas* 25.3 / 27.0 / 28.0 °C) | Added. Removes the earlier assumption that lobster is climate-insensitive. |
| Chronic ghost fishing | ≈10% of traps lost per year (Clark et al. 2012) | Added, alongside storm losses |
| Water clarity → benthic production | OC-CCI Kd490 1998–2024 (API) | Added for seagrass and macroalgae |
| B_MSY/B₀ consistent with life history | FishBase growth, maturity and selectivity via the per-recruit model | Added (Pella–Tomlinson shape) |
| Sargassum index | USF AFAI on NOAA AOML ERDDAP (API) | Added (previous review round) |
| Natural mortality | FishBase (Then et al. 2015); SEDAR 57 for lobster | Added (previous review round) |

**Still blocked or needing data:**
* NCRMP fish surveys: NCEI returns 404/503. The absolute-biomass index model is ready.
* An MPA GIS overlay: polygon service not identified.
* TIP length compositions: confidential.
* SEDAR 57/80 CPUE series: to be transcribed from the reports.
* A larval-connectivity index for lobster.

## 3. Report quality

| Criterion | Assessment after review |
|---|---|
| Every parameter has a value, unit and source | Yes. The input dictionary has the units and definitions. `stcroix_input_values.md` and the generated dictionary tables carry `data_status` and `reference` for every row. |
| Every process has an equation and citation | Yes. Model description §§3–20, with citations for each process, plus the ecological review for the rationale. |
| Assumptions are labelled as such | Yes. `ASSUMPTION` and `PLACEHOLDER` appear in input rows and settings notes, and the assumptions register lists all of them with their type. |
| Citations resolve to the reference list | Yes, by automated audit (`scratchpad` script; re-run before release). |
| Citations verified against the published record | **Partly.** See §4. |
| Numbers in reports match outputs | The case study, review-readiness page and deck are regenerated after each full run. The deck reads numbers directly from output tables. |

## 4. Citation verification status

Verified against the published record during this review (abstract, record page or full text):
* Link & Watson (2019): thresholds.
* Clark et al. (2012): memorandum number and trap-loss figures.
* Arthur et al. (2014).
* Renchen et al. (2010): GCFI paper; page range still to confirm.
* Juman (2005a, b).
* Hu et al. (2015) and the AFAI product.
* SeaLifeBase and FishBase v24.07 fields.
* Then et al. (2015).
* Kough, Paris & Butler (2013).

Entered from standard bibliographic knowledge and **not yet checked line by line**:
* the remaining NPP sources (Adey & Steneck 1985; Naumann et al. 2013; Wanders 1976; Gattuso et al. 1998; Nair & Pillai 1972);
* the theory references (Beddington & Cooke 1983; Clark 1991; Zhou et al. 2012; Pella & Tomlinson 1969; Fletcher 1978; Allen 1971);
* the ecological effect references (Mumby et al. 2004, 2006, 2007; Nagelkerken et al. 2002; Albins & Hixon 2008; Green et al. 2012; van Tussenbroek et al. 2017).

Before submission to a CIE panel, each should be checked against the PDF for the specific value it supports. The Juman case shows why: the value was correct but the reference entry was not.

## 5. Residual risks a reviewer may still raise

1. Most fishery-dependent inputs (landings, effort, CPUE) are placeholders, so absolute results are illustrative (data needs A1–A6, B1–B3).
2. Several ecological process magnitudes are expert placeholders: nursery ν, herbivory γ, lionfish and Sargassum effects, ghost efficiency, MPA fractions. Each is isolated as a structural-ensemble variant, and each should go through structured expert elicitation (Ecological Review §5).
3. The model has no age or size structure. Growth overfishing enters only through the per-recruit priors.
4. Importance sampling can still be weight-limited for 13 constrained groups. Pareto k and the raw ESS are reported, and k ≥ 0.7 triggers truncation and a flag.
