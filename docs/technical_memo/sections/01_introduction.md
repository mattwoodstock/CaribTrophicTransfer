# Introduction

## Management context

Federal fisheries in the U.S. Caribbean are managed by the Caribbean Fishery Management Council (CFMC) under island-based Fishery Management Plans (FMPs) for Puerto Rico, St. Thomas/St. John and St. Croix. The St. Croix FMP took effect in 2022. Under these plans, annual catch limits (ACLs) are set for species complexes defined mostly by taxonomy and catch composition. Only a handful of stocks have full or data-limited assessments:
* Caribbean spiny lobster (SEDAR 57 and its update);
* queen triggerfish (SEDAR 80);
* yellowtail snapper and stoplight parrotfish in some islands.

Every other complex relies on landings-based catch limits. These limits do not account for the productive capacity of the ecosystem, for interactions among species caught together, or for changing environmental conditions.

SEDAR 103 develops an **integrated trophic frontier framework** to fill this gap (SEDAR103-DW-20). It has three linked steps:
1. A **trophic transfer model** estimates the multispecies maximum sustainable yield (MMSY) that the ecosystem's primary production can support for each functional group and guild.
2. A **stochastic production frontier** (SPF) relates landings to effort, gear and environmental inputs, and identifies the frontier of efficient production.
3. A **management framework** aligns harvest caps with ecological capacity through effort ceilings, choke-group constraints and harvest control rules.

This memorandum documents the first component, the CaribTTM package. It describes how the model was built for, and applied to, the St. Croix trap fishery. The SPF is being developed separately by collaborators, and CaribTTM provides it with tables of group, guild and gear MMSY and effort ceilings. The SPF itself is outside the scope of this memorandum.

## Why a trophic-transfer approach

Single-species assessment needs long, contrast-rich time series of catch and abundance. Most Caribbean complexes lack these. Ecosystem models such as Ecopath with Ecosim (Christensen & Walters 2004) represent food-web interactions explicitly, but they need detailed diet, consumption and biomass data for every group, and their yield estimates are sensitive to poorly known vulnerability parameters.

The trophic transfer approach sits between these extremes. It accepts that the absolute production of a reef system is bounded by:
* the primary production of its habitats;
* the efficiency with which that production is transferred up the food web.

It then asks how that bounded production is shared among exploited and unexploited consumers, and what fraction of each group's production can be harvested sustainably. Ecosystem-level fishing limits were developed on the same logic (Link & Watson 2019), and "energy-to-fish" approaches of this kind have been used to set ecosystem-level fishing caps elsewhere.

The approach is attractive for St. Croix for four reasons:
1. **Habitat data are good.** NOAA's National Centers for Coastal Ocean Science (NCCOS) benthic habitat map covers the shelf, and satellite products give the pelagic and deeper-shelf production.
2. **Two anchors exist.** The assessed lobster and queen triggerfish stocks fix the absolute scale.
3. **The gear is non-selective.** Fish and lobster traps catch dozens of species at once, so effort-based controls informed by multispecies caps fit the fishery better than species-by-species ACLs.
4. **Conditions are changing.** Coral loss, warming, hurricanes, invasive lionfish and *Sargassum* inundations are documented and can be represented as forcing on production, habitat quality and survival.

## Objectives of this work

The work had six objectives:

1. **Upgrade the model.** Extend the proof-of-concept TTM in SEDAR103-DW-20 so that it estimates group- and guild-level MMSY under prevailing perturbations and oceanographic, habitat and fisheries conditions, with defensible theory for each process.
2. **Automate data acquisition.** Acquire every input that has a public API automatically (sea-surface temperature, degree heating weeks, chlorophyll, water clarity, primary production, climate projections, habitat areas, bathymetry, hurricanes, species traits). The user must always be able to override.
3. **Propagate uncertainty.** Carry input and observation uncertainty through to outputs, and condition outputs on assessment benchmarks and on historical landings.
4. **Validate and stress-test the model** to the standard expected of a Center for Independent Experts (CIE) review: out-of-sample validation, simulation self-tests, sensitivity and structural uncertainty, independent comparators, and closed-loop management strategy evaluation.
5. **Apply the model to the St. Croix trap fishery** as a case study, from the start of the commercial landings series used in assessments (1983) to 2045, under climate, hurricane, disease, effort and management scenarios.
6. **Document everything** so that each parameter, process and assumption is traceable to data, literature or explicit expert judgement.

## Structure of this memorandum

* **Section 2** describes the study area and the fishery.
* **Sections 3–7** describe the model: overview, primary production, trophic transfer, carrying capacity and ecological modifiers, and population dynamics and reference points.
* **Section 8** describes the data and how each parameter was obtained.
* **Section 9** describes the uncertainty, calibration, validation and robustness methods.
* **Section 10** reports the St. Croix results.
* **Section 11** discusses them.
* **Sections 12 and 13** summarize the assumptions and the data needs.
* **Section 14** concludes.
* **Appendices:**
  - A: notation;
  - B: the assumptions register;
  - C: complete input tables;
  - D: the code and report review;
  - E: software and reproducibility.
