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
| 2.1 | "MMSY = x·P/B·K is a strong assumption." | Replaced by MMSY = x·M·K on natural mortality, with group-specific x (0.17–0.32) and Pella–Tomlinson shape (B_MSY/K 0.29–0.38) from a per-recruit analysis of FishBase/SEDAR traits (Beddington & Cooke 1983; Zhou et al. 2012). The original form is kept as structural variant `gulland_PB`. | `data-raw/life_history/xmsy_priors.csv`; Assumptions A5–A10 | ✅ |
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
| 4.1 | "SIR weights are degenerate." | Plain SIR on 5,000 draws collapsed (ESS 1). Adaptive factorized importance sampling uses per-group marginal corrections and the exact mixture correction (Owen & Zhou 2000). Final run: 60,000 draws, raw ESS 20. The Pareto tail was too heavy to smooth (k̂ = 2.2), so the weights were truncated (Ionides 2008): ESS 125, 307 unique draws, largest weight 1.3%. The truncation is flagged in the output. | `sir_diagnostics_summary.csv`, `fig08` | ◐ |
| 4.2 | "Are the posteriors just the priors?" | Posterior shrinkage is reported for every sampled parameter. GSA shows each group's own partition explains most MMSY variance (SRRC ≈ 0.6), which is the parameter the data update. | `posterior_shrinkage.csv`, `global_sensitivity.csv` | ✅ |
| 4.3 | "Can the method recover known truth?" | Simulation self-test (100 replicates; truths drawn from the prior; simulate, then refit): 90% coverage 0.79–0.93; median bias −19% to +12%; queen triggerfish 0%, lobster −6%. | `self_test_summary.csv` | ✅ |
| 4.4 | "Everything is calibrated, so nothing is validated." | Data are split by role. All indices are `role = validate` (held out). OFLs are calibrated but tested by leave-one-benchmark-out. A retrospective peel (`retrospective_peel()`) is available. | `R/validation.R` | ✅ |

## 5. Out-of-sample validation

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 5.1 | "Does the model predict data it didn't see?" | Leave-one-benchmark-out: queen triggerfish OFL predicted 16.4k lb (90% 10.3–47.9k) vs 22.8k observed: **pass**. Lobster OFL predicted 774k vs 164k: **fail**. An independent catch-only analysis gives queen triggerfish 28.4k and lobster 205k lb. | `validation_loo_benchmarks.csv`, `method_comparison_catch_only.csv`, `fig13`, `fig16` | ◐ |
| 5.2 | "Why does lobster fail?" | Without its OFL, lobster scale comes only from the Opitz 1980s biomass prior, which is high for the modern stock. The finding is that lobster scale is identified only by assessment information. The remedy is SEDAR 57 CPUE as a *fit* index and an M prior (B1, B5). | case study §4 | ⬜ |
| 5.3 | "Held-out index skill?" | Held-out placeholder indices: 90% coverage 1.0, MASE 1.04–1.16. The series are synthetic, so this demonstrates the machinery only. | `validation_out_of_sample.csv`, `fig12` | ⬜ |

## 6. Sensitivity and robustness

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 6.1 | "Which inputs matter?" | Global sensitivity analysis (rank R² 0.76 for total focus MMSY): invertebrate-competitor biomass (negative), pavement TTE, NPP and food-web fraction, reef NPP and lobster partition. Value of information by data stream: biomass surveys 24%, NPP 18%, TTE 18%, food-web fraction 10%. Landings act through calibration, not prior variance. | `global_sensitivity.csv`, `value_of_information_*.csv`, `fig11`, `fig17` | ✅ |
| 6.2 | "Thermal response is speculative." | AquaMaps thermal curves come from FishBase. Removing thermal effects changes queen triggerfish MMSY by +11% and lobster by 0%. Lobster has no thermal curve yet (SeaLifeBase pending), so its climate insensitivity is an assumption. | ensemble `no_thermal` | ◐ |
| 6.3 | "Does fishing feedback on the food web matter?" | Switching it off changes trap-group MMSY by less than 0.1%. At current exploitation the top-down feedback is weak. | ensemble `no_fishing_feedback` | ✅ |

## 7. Ecosystem context and management use

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 7.1 | "Is the ecosystem overfished overall?" | Ryther index 0.61–0.66 t km⁻² yr⁻¹ (below the practical limit of ~1; Link & Watson 2019). Fogarty 0.56–0.60‰ (inside the 0.22–0.92‰ caution band), 2022–2024. | `ecosystem_overfishing_indices.csv`, `fig14` | ◐ |
| 7.2 | "Which gear is limiting, and on what?" | Fish traps and diving are at their multispecies ceilings (0.95× and 0.96× current effort), choked by hogfish (54–56% of draws). Lobster traps could expand about 28% (choke: queen triggerfish, 73%). P(overfishing) at current catch: hogfish 0.35, porgies 0.16, queen triggerfish 0.16. | `spf_gear_effort_ceilings.csv`, `fig07` | ◐ |
| 7.3 | "Projections under change? Do management rules work?" | By 2043–2045, SSP5-8.5 cuts MMSY by 16–43% (lobster −25%, Parrotfish 2 −43%). No-take expansion or a 25% trap cut lowers choke-group risk by about a quarter. Closed-loop MSE, P(any focus group < 0.5 B_MSY): status quo 0.33, SPF ceiling rule 0.31, biased SPF rule 0.41, catch-rate rule 0.41. | `spf_scenario_reference_points.csv`, `mse_performance.csv`, `fig03`, `fig18` | ◐ |
| 7.4 | "Status labels for uncalibrated groups?" | No status is assigned to them: they are reported as *Outside focus (uncalibrated)*. | `spf_group_reference_points.csv` | ✅ |

## 8. Reproducibility

| # | Likely critique | Response | Evidence | Status |
|---|---|---|---|---|
| 8.1 | "Can results be reproduced?" | It is an R package with unit tests (124 expectations, 0 failures). Per-draw RNG streams make parallel runs reproducible. Report tables, including the technical memorandum, are generated from outputs. | `tests/`, `docs/technical_memo/build_memo.R` | ✅ |
| 8.2 | "Data provenance?" | Every input row carries `data_status` and `reference`. API pulls are committed in `acquired/`, with source metadata. | `stcroix_input_values.md` | ✅ |

## Open items before review

1. Enter SEDAR 57 lobster and SEDAR 80 queen triggerfish standardized CPUE. Use one as *fit* and keep the other *validate*, or fit both and validate on NCRMP.
2. Enter real CCL landings and effort by gear, and the expansion factor (A1–A6).
3. Ingest NCRMP St. Croix fish and invertebrate surveys as absolute-biomass indices. The pipeline is ready; NCEI downloads currently fail.
4. Replace the placeholder process magnitudes (G1–G9) through structured expert elicitation and fishers' knowledge.
5. Check the remaining literature citations against their sources (see the Code Review).
6. Raise the effective sample size: more draws, or a sequential Monte Carlo sampler.
