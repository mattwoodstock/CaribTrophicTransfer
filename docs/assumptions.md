# Assumptions Register

This register lists every assumption in CaribTTM and the St. Croix case study. Each row gives:
* the value or form used;
* its **basis** (a citation where one exists, or "expert judgement");
* its **type**;
* how its influence is **tested** and what would **replace** it.

Each input row also carries its own `data_status` and `reference` columns in `Projects/StCroix_Trap/data/` (see [St. Croix Input Values](stcroix_input_values.md)).

**Type legend:**
* **T**: standard theory or a model form from the literature.
* **D**: derived from data (API or published dataset).
* **L**: literature value transferred from another place or system.
* **E**: expert judgement (an assumption).
* **P**: placeholder awaiting data.
* **S**: statistical convention.

**Test legend:**
* **ens:x**: structural-ensemble variant x.
* **GSA**: global sensitivity analysis on sampled CVs.
* **VOI**: value of information.
* **CV**: sampled with the stated CV.

## A. Model structure

| ID | Assumption | Value / form | Basis | Type | Tested / replaced by |
|---|---|---|---|---|---|
| A1 | Energy moves through four trophic-level pools per pathway (benthic, pelagic) in annual steps with monthly biomass sub-steps | Lindeman pools; 12 sub-steps | Lindeman (1942); SEDAR103-DW-20 | T | Standard trophic-transfer abstraction; not varied |
| A2 | A pool is shared among consumers in proportion to biomass-weighted demand × diet × habitat preference (no predator functional response) | $w_i D_{ik} \pi_{ih}$ | Ecopath-style mass balance (Christensen & Walters 2004) | T/E | GSA on partitions; partition CV 0.5–1.0 |
| A3 | Trophic transfer efficiency is constant within a pathway | 0.15 ± 20% | SEDAR103-DW-20 default; global mean ≈ 0.10 (Pauly & Christensen 1995) | L | CV; ens: tte_0.10, tte_0.20 |
| A4 | Only a fraction of net primary production enters the grazing/detrital food web used by fished groups | 0.20–0.50 by habitat | Expert judgement, informed by Ecopath detritus flows | E | CV 0.3; ens: fwf_low, fwf_high; VOI |
| A5 | Unfished biomass turns over at natural mortality, so K = P/M | — | Equilibrium P/B = Z, and Z = M when unfished (Allen 1971) | T | ens: gulland_PB (original P/B form) |
| A6 | Sustainable yield cap MMSY = x·M·K | x from per-recruit theory (0.17–0.32) | Gulland (1971); Beddington & Cooke (1983); Zhou et al. (2012) | T/D | CV on x; ens: gulland_PB, x_msy_low |
| A7 | Surplus production is Pella–Tomlinson with B_MSY/K equal to the per-recruit B_MSY/B₀ | 0.29–0.38 | Pella & Tomlinson (1969); Fletcher (1978); per-recruit analysis | T/D | ens: schaefer_shape |
| A8 | Stock–recruitment steepness for the x prior | h ~ N(0.75, 0.1) | Below the meta-analytic centre for demersal fishes (≈ 0.8; Shertzer & Conn 2012). Chosen as a precaution. | E/L | Carried into x_msy CV |
| A9 | Trap retention length (knife-edge-like logistic) | L50 = 15 cm TL (≤ 0.6 L∞), CV 0.3; lobster 8.9 cm CL (legal minimum) | Expert judgement for 1.5–2 in. mesh; USVI lobster size limit | E/P | Data need G8 |
| A10 | Maturity length from L∞ | Lm = 10^(0.8979 log L∞ − 0.0782) | Froese & Binohlan (2000) | L | — |
| A11 | Spiny lobster recruitment is mostly imported | φ = 0.8 ± 25% | Kough, Paris & Butler (2013) | L/P | ens: closed_population; data need G1 |
| A12 | The open-recruitment component relaxes to K at rate 2xM, so its yield at F = 2xM equals x·M·K | — | Model construction: keeps one reference-point definition. It understates an open population's MSY, which is conservative. | E | ens: closed_population |
| A13 | Capacity scales with nursery-habitat area × quality | elasticity ν = 0.2–0.5 | Nagelkerken et al. (2002); Mumby et al. (2004); Adams et al. (2006). Magnitudes are expert judgement. | E/P | ens: no_nursery; data need G3 |
| A14 | Capacity scales with structural refuge (coral cover) | ρ = 0–0.7 | Expert judgement | E | GSA; data need C8 |
| A15 | Relative habitat preferences | 0–1 matrix, CV 0.2 | Expert judgement from habitat descriptions (Randall 1967; FishBase/SeaLifeBase) | E | `pref_cv`; data need C7 |
| A16 | Coral recovery scales with herbivore grazing (projections only, capped at 2×) | γ = 1 ± 50% | Mumby et al. (2006, 2007). Magnitude is expert judgement. | E/P | ens: no_herbivory_feedback; data need G4 |
| A17 | Coral cover dynamics | growth 0.10 ± 30% yr⁻¹, cap 0.30, bleaching 3% per DHW above 4 °C-weeks | Literature-typical Caribbean values. Coral cover is observed in all historical years, so this matters only in projections. | L/E | CV; scenarios |
| A18 | Thermal responses follow AquaMaps temperature-preference curves (realized niche, not physiology) | Gaussian; σ = (max − min)/2 | FishBase / SeaLifeBase AquaMaps fields | D/E | ens: no_thermal |
| A19 | Lionfish reduce K of small-prey groups (−10%) and mesopredators (−5%) at full invasion | timeline 2009–2012 | Albins & Hixon (2008); Green et al. (2012); Albins (2013); Schofield (2010). Magnitudes are expert judgement. | E/P | ens: no_lionfish_sargassum; data need G6 |
| A20 | Sargassum degrades nursery groups (−3% K per index unit) and benefits pelagics (+3%) | AFAI index 2016–2024; 2011–2015 relative magnitudes | Index: data (Hu et al. 2015; AOML ERDDAP). Effects: van Tussenbroek et al. (2017); Casazza & Ross (2008). Magnitudes and pre-2016 values are placeholders. | D/E/P | ens: no_lionfish_sargassum; data need G7 |
| A21 | Seagrass and macroalgae production scale with bottom light from Kd490 | depths 4 m / 8 m; elasticity 1; held at 1 before 1998 | Kd(PAR) from Morel et al. (2007); light limitation (Duarte 1991). Depths and elasticity are assumptions. | D/E | Effect ±2%; not varied |
| A22 | Stressor effects combine multiplicatively | — | Multiplicative null model | E | Recommended interaction variant |
| A23 | Last year's catch is removed from the trophic-level pool it fed on (bottom-up fishing feedback only) | — | SEDAR103-DW-20 | T/E | Earlier ensemble: < 0.1% effect on trap groups |
| A24 | Process error is lognormal, independent across years and groups | σ = 0.05–0.15 | Statistical convention | S | — |
| A25 | Deeper-shelf (30–100 m) production is light-limited | NPP from Kd(PAR), PAR and a tanh P–I curve, CV 0.6 | Morel et al. (2007); ETOPO 2022 | D/E | CV |
| A26 | Benthic NPP rates transferred from Caribbean (and, for the reef range, Indian Ocean) studies | see habitats.csv | Adey & Steneck (1985, St. Croix); Naumann et al. (2013); Juman (2005); Wanders (1976); Gattuso et al. (1998); Nair & Pillai (1972) | L | CV 0.3–0.6; VOI; data need D2 |
| A27 | Carbon-to-wet-weight conversion | 9 ± 10% | Pauly & Christensen (1995) | L | CV |

## B. Fishery and observation processes

| ID | Assumption | Value / form | Basis | Type | Tested / replaced by |
|---|---|---|---|---|---|
| B1 | Reported landings understate removals by a constant factor | 1.5 ± 25% | Puerto Rico ≈ 2.0; USVI unquantified (SEDAR103-DW-20) | L/E | CV; VOI; data need A3 |
| B2 | Landings and effort records have lognormal observation error | CV 0.2 / 0.1 | Expert judgement for self-reported trip tickets | E | CV |
| B3 | Catchability is constant and estimated from the last 3 historical years; projected F = qE | — | Standard effort-driven projection | S/E | MSE robustness |
| B4 | No-take areas remove a fixed share of each habitat from fishing. Historical catch is capped at 90% of fishable biomass per month, implicitly allowing spillover. | reef 0.25, pavement 0.15, … | Placeholder approximating Buck Island Reef NM and East End Marine Park no-take zones | P | Scenario MPA_expansion; data need G2 |
| B5 | Derelict traps keep fishing | efficiency 0.2 ± 50%; half-life 1.5 yr; chronic loss 10% yr⁻¹; storm losses from disturbance templates | Chronic loss: Clark et al. (2012) data. Ghost fishing observed: Renchen et al. (2010); Arthur et al. (2014). Efficiency and half-life are placeholders. | D/P | ens: no_ghost_fishing; data need G5 |
| B6 | Fishery-dependent CPUE has catchability creep, and CPUE may be hyperstable | creep 2% yr⁻¹; β = 1 | Palomares & Pauly (2019); Harley et al. (2001) | L/E | Data need G9 |
| B7 | Initial (1983) depletion | B/K = 0.4–0.8 by group, CV 0.2–0.3 | Expert judgement | E | GSA; data need C10 |
| B8 | Hurricane losses (NPP, coral, traps, lobster production) and recovery half-lives | 25% / 15% / 60% / …, half-lives 1.5–2 yr; intensity CV 0.3; common loss multiplier CV 0.5 | Placeholders; intensity from HURDAT2 data | P/D | CV; data need E2 |
| B9 | Effort responds to catch rate only in the Open_access_effort scenario | elasticity 0.5 | Smith (1969) | T/E | Scenario |

## C. Calibration and statistics

| ID | Assumption | Value / form | Basis | Type | Tested / replaced by |
|---|---|---|---|---|---|
| C1 | **An assessment OFL is treated as an observation of MMSY** (2022–2024 mean) with lognormal CV 0.4 | lobster 163,823 lb; queen triggerfish 22,773 lb | An OFL is F_MSY × current biomass, which equals MSY only at B = B_MSY. CV 0.4 absorbs the mismatch. | S/E | Leave-one-benchmark-out validation |
| C2 | Historical landings must be supportable (stock-reduction logic) | penalty on the largest annual shortfall, CV 0.25 | Kimura & Tagart (1982); Walters et al. (2006); Martell & Froese (2013) | T | Self-test; feasibility diagnostics |
| C3 | Only trap-fishery groups and assessed stocks constrain calibration; others are carried but not calibrated | 13 + 2 groups | Project scope | E | Reported as "Outside focus (uncalibrated)" |
| C4 | 1980s Opitz (1996) Ecopath biomasses, scaled by the mass-balance factor 0.239, centre the 1983 partition priors | lognormal CV 0.5 (1.0 where weak) | Opitz (1996) | L/E | Wide CVs; VOI; data need C1 |
| C5 | Group P/B is the geometric mean of Opitz (1996) P/B and FishBase M | CV = max(0.25, \|log ratio\| / 2) | Opitz (1996); Then et al. (2015) | L | CV |
| C6 | The posterior is represented by importance-weighted prior draws | adaptive factorized proposals; Pareto smoothing, or truncation when k ≥ 0.7 | Owen & Zhou (2000); Vehtari et al. (2024); Ionides (2008) | S | ESS, Pareto k and raw ESS reported |
| C7 | Reference period and management percentile | 2022–2024; P* = 0.40 | SEDAR103-DW-20 example | E | Settings |
| C8 | Status thresholds | overfished if B < 0.5 B_MSY; Ryther ≈ 1 (3 extreme) t km⁻² yr⁻¹; Fogarty 0.22–0.92‰ | MSST proxy; Link & Watson (2019) | L | — |
| C9 | Placeholder indices are held out (role = validate) rather than fitted | — | Avoids fitting synthetic data | S | Replace with SEDAR 57/80 CPUE |

## D. Decision tools

| ID | Assumption | Value / form | Basis | Type |
|---|---|---|---|---|
| D1 | Catch-only comparator: Schaefer dynamics; resilience range from M; start depletion 0.5–0.9; end depletion 0.2–0.8 | — | Martell & Froese (2013); Froese et al. (2017) defaults | L/E |
| D2 | Value of information uses first-order (main-effect) variance shares; interactions are ignored | 20 quantile bins, bias-corrected | Saltelli et al. (2008); Oakley & O'Hagan (2004) | S |
| D3 | MSE assessment error and decision rules | MMSY CV 0.3 (and a +50% bias case); every 3 years; ±25% change cap; P* = 0.4 | Expert judgement; typical of data-limited MSE | E |
| D4 | MSE operating models are the SIR-retained draws under the Baseline scenario | 150 draws | — | S |

## Where assumptions are stated

* **Input files:** every row has `data_status` (derived / published / API / PLACEHOLDER / ASSUMPTION) and `reference`. `settings.csv` notes label ASSUMPTION and PLACEHOLDER values.
* **Placeholders to replace:** [Data Needs](data_needs.md) (A1–G9).
* **Process-level rationale and gaps:** [Ecological Review](ecological_review.md).
* **Equations:** [Model Description](model_description.md).
