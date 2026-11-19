---
title: "CaribTTM: A Habitat-Resolved Trophic Transfer Model for Multispecies Sustainable Yield in U.S. Caribbean Fisheries"
subtitle: "Model description, data, uncertainty and validation, with application to the St. Croix trap fishery"
author: "Technical Memorandum: draft for internal and Center for Independent Experts review"
date: "Draft of November 2026"
lang: en-US
---

**Suggested citation.** [Authors]. 2026. CaribTTM: a habitat-resolved trophic transfer model for multispecies sustainable yield in U.S. Caribbean fisheries. Model description, data, uncertainty and validation, with application to the St. Croix trap fishery. Draft technical memorandum, SEDAR 103 supporting document.

**Status of this document.** This is a working draft. The model structure, data pipeline, uncertainty framework and diagnostics are complete and tested. Many St. Croix inputs remain **placeholders**:
* confidential landings and effort by gear;
* standardized assessment indices;
* several ecological process magnitudes.

Every placeholder is flagged in the input files and listed in Section 13 and Appendix B. Results in Section 10 illustrate the method and the direction of effects. **They are not management advice** until those inputs are replaced.

**Reproducibility.** The tables in this memorandum are generated from the project input files and the model outputs by `docs/technical_memo/build_memo.R`. Text and numbers therefore come from the same model run. The run reported here took {{RUN_TIME}}.

# Abstract {-}

Island-based fishery management in the U.S. Caribbean must set catch limits for dozens of species complexes that are rarely assessed, caught together by non-selective gears, and supported by coral reef, seagrass, mangrove and pelagic habitats that are changing quickly. CaribTTM provides one component of the SEDAR 103 integrated trophic frontier framework. It estimates the energetically supported, multispecies maximum sustainable yield (MMSY) of every functional group, guild and gear. The estimates hold under prevailing oceanographic, habitat, disturbance and fishery conditions, and the model produces the inputs a stochastic production frontier (SPF) needs to set effort ceilings.

The model works as follows:
* Habitat-specific net primary production (mapped area × production rate × water-clarity index) is routed through trophic-level energy pools with habitat-specific transfer efficiencies.
* Pool production is partitioned among consumers in proportion to biomass-weighted demand, diet and habitat preference.
* Production is converted to unfished biomass at natural mortality. Sustainable yield is capped at x·M·K, with group-specific x and production shape derived from per-recruit life-history theory.
* Annual biomass dynamics include:
  - historical landings with reporting expansion;
  - effort-driven projections;
  - open larval recruitment (spiny lobster);
  - nursery-habitat dependence;
  - herbivory feedback on coral recovery;
  - no-take areas;
  - ghost fishing by lost traps;
  - thermal performance;
  - lionfish and pelagic *Sargassum*;
  - hurricanes, bleaching and coral disease.
* Environmental and habitat data are acquired automatically from public application programming interfaces (APIs), with user override.
* Every input carries an uncertainty that is propagated by Monte Carlo.
* Draws are conditioned on assessment benchmarks and on the requirement that each stock could have supported its historical landings. Conditioning uses adaptive importance sampling with Pareto-smoothed or truncated weights.
* Robustness tools:
  - out-of-sample validation (held-out indices, leave-one-benchmark-out, retrospective peels);
  - a simulation self-test;
  - global sensitivity and value-of-information analyses;
  - a 14-variant structural ensemble;
  - an independent catch-only comparator;
  - closed-loop evaluation of SPF-style harvest rules.

The St. Croix trap fishery case study covers 1983–2045. It applies the model to 19 Fishery Management Plan complexes and 10 competitor groups across seven benthic habitats and the shelf pelagic zone. The 13 trap-relevant complexes and the two assessed stocks (spiny lobster, SEDAR 57; queen triggerfish, SEDAR 80) form the calibration focus.

# Executive summary {-}

{{INCLUDE:technical_memo/exec_summary.md,0}}
