# Review Readiness: Anticipated CIE Critiques and Responses

This page anticipates the questions a Center for Independent Experts (CIE) panel is likely to ask of CaribTTM and the St. Croix trap case study. For each, it gives the response, the evidence (file or figure in `Projects/StCroix_Trap/output/`), and what remains open. Numbers come from the main run (48,000 prior draws, ESS 86) and the 10-variant structural ensemble.

**Status legend:** ✅ addressed with evidence · ◐ addressed, but depends on placeholder data · ⬜ open (needs data)

## 1. Scope and purpose

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 1.1 | "What decision does this model inform?" | It provides group-, guild- and gear-level MMSY distributions, P* targets and effort ceilings that feed a stochastic production frontier (SPF) built by collaborators. It does not set ACLs directly. | `spf_*.csv`; [Model Description](model_description.md) | ✅ |
| 1.2 | "Effort is spread over groups the fishery doesn't catch." | Calibration effort targets the trap fishery: 13 groups with ≥ 25% of landings from traps, plus the two assessed stocks (`sir_feasibility_groups = trap`). Five non-trap groups stay in the food web but are labelled *Outside focus (uncalibrated)* and cannot be choke groups. | `spf_group_reference_points.csv` (`calibrated` column) | ✅ |

## 2. Model structure

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 2.1 | "MMSY = x·P/B·K is a strong assumption." | The Gulland/Schaefer form is standard for data-limited stocks, with x = 0.5 under logistic production. Structural variants with x = 0.3 and 0.4 change lobster MMSY by −22% and −11% and queen triggerfish by −18% and 0%. | `structural_ensemble_by_variant.csv`, `fig15` | ✅ |
| 2.2 | "Trophic transfer efficiency is unknown." | TTE is sampled (0.15 ± 20%) and tested structurally at 0.10 and 0.20. Assessed-stock MMSY changes by only 4–13% (lobster +8% and +4%, queen triggerfish −13% and +5%), because SIR re-scales the partition. Unassessed groups move more. | GSA (`fig11`); ensemble | ✅ |
| 2.3 | "Does the energy budget conserve mass?" | Demand is partitioned as w·D·pref, so production summed over consumers equals TTE × E. Unit tests check conservation. | `tests/testthat/test-core.R` | ✅ |
| 2.4 | "Results hinge on one structural choice." | Ten structural variants are pooled with equal weight. Between-variant CV of MMSY: lobster 0.10, queen triggerfish 0.09, grunts 0.26, Parrotfish 2 0.45. Assessed-stock conclusions are structurally robust; unassessed-complex scale is not, and is reported as such. | `structural_ensemble_pooled.csv` | ✅ |
| 2.5 | "Pelagic production is double-counted or mis-scaled." | Swapping the regional 85 g C m⁻² yr⁻¹ for the satellite VGPM ensemble leaves trap-group MMSY unchanged (ESS 19.95 vs 19.95). Trap stocks are benthic-energy limited. | ensemble variant `pelagic_npp_satellite` | ✅ |
| 2.6 | "The unmapped deeper shelf inflates production." | The NCCOS "Unknown" class (388 km²) was mostly off-shelf. It was replaced by 41.4 km² at 30–100 m from ETOPO 2022, with light-limited NPP (Morel 2007 Kd–PAR, tanh P–I) and CV 0.6. | `acquired/`; [Data Acquisition](data_acquisition.md) | ✅ |

## 3. Priors and inputs

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 3.1 | "Biomass and P/B priors are expert guesses." | They are now derived from the Opitz (1996) balanced Virgin Islands Ecopath model (× mass-balance factor 0.239), combined with FishBase M (Then et al. 2015), L∞ (from Lmax) and trophic level. Wide CVs (0.5; 1.0 for poorly known groups) let the data dominate. | `data-raw/build_priors_from_opitz.R`, `data-raw/fishbase/` | ◐ |
| 3.2 | "Input uncertainty is ignored." | Every input CV and observation error is sampled: habitat areas, NPP, food-web fraction, TTE, P/B, partition, diet preference, covariates, landings, effort, expansion factor, hurricane intensity and losses, and coral growth. | `R/parameters.R`; [Input Dictionary](input_dictionary.md) | ✅ |
| 3.3 | "Landings and effort are synthetic." | Yes. They are flagged `PLACEHOLDER`, scaled to ACL-era magnitudes and listed as priority **H** data needs (A1–A6). | [Data Needs](data_needs.md) | ⬜ |
| 3.4 | "Environmental data are ad hoc." | They are acquired reproducibly from public APIs (CRW, Pathfinder, OC-CCI, VGPM ensemble, CMIP6, NCCOS, HURDAT2, ETOPO), with spatial SD carried as observation error. Users can override any value. | `data_sources.csv`, `acquired/` | ✅ |

## 4. Calibration and identifiability

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 4.1 | "SIR weights are degenerate." | Plain SIR on 5,000 draws collapsed (ESS 1). The model now uses adaptive importance sampling with factorized per-group proposals, adapting only conflicted groups, and an exact deterministic-mixture correction (Owen & Zhou 2000). Final run: 48,000 draws, ESS 86, 316 unique, max weight 4.7%, top-10 weight share 25%. | `sir_diagnostics_summary.csv`, `fig08` | ✅ |
| 4.2 | "Are the posteriors just the priors?" | Posterior shrinkage is reported for every sampled parameter. GSA shows each group's own partition explains most MMSY variance (SRRC ≈ 0.6), which is the parameter the data update. | `posterior_shrinkage.csv`, `global_sensitivity.csv` | ✅ |
| 4.3 | "Can the method recover known truth?" | Simulation self-test (100 replicates; simulate data from a draw, refit): 90% coverage 0.77–0.91. Median bias is −0.1% for queen triggerfish and +10% for lobster; hogfish (−37%) and porgies (−31%) are biased low, which is conservative for choke-group limits. | `self_test_summary.csv` | ✅ |
| 4.4 | "Everything is calibrated, so nothing is validated." | Data are split by role. All indices are `role = validate` (held out). OFLs are calibrated but tested by leave-one-benchmark-out. A retrospective peel (`retrospective_peel()`) is available. | `R/validation.R` | ✅ |

## 5. Out-of-sample validation

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 5.1 | "Does the model predict data it didn't see?" | Leave-one-benchmark-out: queen triggerfish OFL predicted 28.7k lb (90% 14.8–76.3k) vs 22.8k observed: **pass**. Lobster OFL predicted 1.06M vs 164k: **fail**. | `validation_loo_benchmarks.csv`, `fig13` | ◐ |
| 5.2 | "Why does lobster fail?" | Without its OFL, lobster scale comes only from the Opitz 1980s biomass prior, which is high for the modern stock. The finding is that lobster scale is identified only by assessment information. The remedy is SEDAR 57 CPUE as a *fit* index and an M prior (B1, B5). | case study §4 | ⬜ |
| 5.3 | "Held-out index skill?" | The held-out placeholder indices give 90% coverage 1.0 and MASE 1.2–1.8 (worse than a naive forecast). Because the series are synthetic, this demonstrates the machinery only. It becomes a real test once SEDAR 57/80 and NCRMP series are entered. | `validation_out_of_sample.csv`, `fig12` | ⬜ |

## 6. Sensitivity and robustness

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 6.1 | "Which inputs matter?" | Global sensitivity analysis (SRRC/PRCC, rank R² 0.75–0.83). Total focus MMSY is driven by pavement TTE, invertebrate-competitor biomass, pavement NPP and food-web fraction. Group MMSY is driven by its own partition, then pavement TTE and P/B. | `global_sensitivity.csv`, `fig11` | ✅ |
| 6.2 | "Thermal response is speculative." | AquaMaps thermal curves come from FishBase. Removing thermal effects changes queen triggerfish MMSY by +11% and lobster by 0%. Lobster has no thermal curve yet (SeaLifeBase pending), so its climate insensitivity is an assumption. | ensemble `no_thermal` | ◐ |
| 6.3 | "Does fishing feedback on the food web matter?" | Switching it off changes trap-group MMSY by less than 0.1%. At current exploitation the top-down feedback is weak. | ensemble `no_fishing_feedback` | ✅ |

## 7. Ecosystem context and management use

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 7.1 | "Is the ecosystem overfished overall?" | Ryther index 0.59–0.63 t km⁻² yr⁻¹ (below the 1 t threshold) and Fogarty 0.54–0.59‰ (inside the 0.22–0.92‰ caution band) for 2022–2024 (Link & Watson 2019). Fogarty thresholds are confirmed; the Ryther thresholds should be checked against the primary source before citing. | `ecosystem_overfishing_indices.csv`, `fig14` | ◐ |
| 7.2 | "Which gear is limiting, and on what?" | Lobster traps could expand about 65% (median, 90% 1.08–3.11×) before a calibrated group reaches its gear-specific MMSY; the choke is queen triggerfish in 82% of draws. Fish traps and diving are limited by hogfish and porgies. | `spf_gear_effort_ceilings.csv`, `fig07` | ◐ |
| 7.3 | "Projections under change?" | Scenario evaluation uses the final projection years (2043–2045). By then SSP5-8.5 halves Parrotfish 2 MMSY and cuts queen triggerfish, hogfish and porgies by 17–19%. Hurricanes and SCTLD have transient effects. | `spf_scenario_reference_points.csv`, `fig03` | ◐ |
| 7.4 | "Status labels for uncalibrated groups?" | No status is assigned to them: they are reported as *Outside focus (uncalibrated)*. | `spf_group_reference_points.csv` | ✅ |

## 8. Reproducibility

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 8.1 | "Can results be reproduced?" | It is an R package with unit tests (95 expectations). Parallel runs use L'Ecuyer-CMRG per-draw streams, so they are reproducible regardless of the number of cores. Seeds are stored, and parameters are regenerated from seeds for kept draws. | `tests/`, `test-robustness.R` | ✅ |
| 8.2 | "Data provenance?" | Every input row carries `data_status` and `reference`. API pulls are committed in `acquired/`, with source metadata. | `stcroix_input_values.md` | ✅ |

## Open items before review

1. Enter SEDAR 57 lobster and SEDAR 80 queen triggerfish standardized CPUE. Use one as *fit* and keep the other *validate*, or fit both and validate on NCRMP.
2. Enter real CCL landings and effort by gear, and the expansion factor (A1–A6).
3. Ingest NCRMP St. Croix fish surveys. The pipeline (`ncrmp_fish` method, stratified estimator) is built and tested; NCEI downloads returned HTTP 503 throughout development.
4. Add a lobster thermal-performance curve.
5. Verify the Ryther thresholds against Link & Watson (2019).
6. Re-run the main run and the ensemble, and refresh this page.
