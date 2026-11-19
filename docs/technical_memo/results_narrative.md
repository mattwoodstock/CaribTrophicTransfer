<!-- results narrative -->
## Summary of results

**Calibration.** The run used 60,000 prior draws in five adaptive rounds of 12,000 and took 42.5 minutes on four cores.
* The raw importance weights had an effective sample size (ESS) of 20.
* The weight tail was too heavy for reliable Pareto smoothing (k̂ = 2.2), so the weights were truncated (Ionides 2008). This gave ESS 125, with 307 unique draws among 2,000 resamples and a largest weight of 1.3%.
* Reported quantiles are therefore stable, but the posterior is still represented by a few hundred distinct parameter sets. Larger runs, or a sequential Monte Carlo sampler, would sharpen the tails.
* Every retained draw supports all historical landings of the focus groups (shortfall 0).

**Assessed stocks.**
* **Queen triggerfish:** median MMSY 23,497 lb (90% interval 12,541–37,329) against the SEDAR 80 OFL of 22,773 lb (ratio 1.03). Current expanded catch is 15,466 lb, B/B_MSY 1.83 and P(overfishing) 0.16.
* **Spiny lobster:** median MMSY 249,798 lb (120,321–441,273), 1.52× the SEDAR 57 OFL. Catch is 121,353 lb, B/B_MSY 2.44 and P(overfishing) 0.07.

The lobster gap is consistent with an OFL being a catch at F_MSY under current conditions while MMSY is an equilibrium cap. It is within the CV of 0.4 used for the OFL.

**Trap groups.** The groups with the least headroom are:
* **hogfish:** MMSY 8,201 lb against catch 7,003 lb; P(overfishing) 0.35;
* **porgies:** 5,890 against 3,338; P(overfishing) 0.16.

These are low-production groups caught mainly as trap bycatch. The theory-based priors (x ≈ 0.20, B_MSY/B₀ ≈ 0.36) give them much less headroom than the original x = 0.5 form did.

The large herbivore and invertivore complexes have wide intervals and MMSY well above their catches:
* Parrotfish 2: 155,615 lb, 2.2× its ACL;
* grunts: 315,880 lb;
* shallow snappers (Snapper 3): 187,034 lb, 13× its ACL.

These scales are the least constrained by data (Section 10.11). Uncalibrated groups (conch, yellowtail, deepwater snappers, jacks, pelagics) are reported for completeness without a status.

**Gears.** Effort ceilings, as multipliers of 2022–2024 effort, are:

| Gear | Ceiling at median MMSY | Ceiling at P* target | Choke group |
|---|---|---|---|
| Fish traps | 0.95 | 0.90 | hogfish (54% of draws) |
| Dive | 0.96 | 0.92 | hogfish (56%) |
| Lobster traps | 1.28 | 1.21 | queen triggerfish (73%) |
| Hook and line | 1.21 | 1.10 | queen triggerfish (61%) |

Fish-trap and dive effort are therefore at or just above the multispecies sustainable level, set by their bycatch of small reef fishes.

**Processes.**
* The no-take areas leave 83–88% of each focus group's biomass fishable.
* Ghost fishing (chronic plus storm losses) removed the equivalent of 3–11% of landings over 1983–2024: about 5% for lobster, 9% for queen triggerfish and 11% for boxfishes. This mortality is not counted in the landings record.
* Nursery indices changed by less than 3% over the history, because mapped habitat areas are static. They respond in the seagrass-loss scenario.

**Scenarios** (2043–2045, against Baseline):
* **SSP5-8.5** lowers MMSY by about 25% for lobster (now with its SeaLifeBase thermal curve), 43% for Parrotfish 2, 32% for grunts, 18% for queen triggerfish, 16% for hogfish and 22% for porgies.
* **Combined stress** raises P(overfishing) for queen triggerfish from 0.08 to 0.16 and for hogfish from 0.12 to 0.18.
* **Open-access effort dynamics** raise it to 0.20, 0.24 and 0.29 for queen triggerfish, hogfish and lobster.
* **Management measures:**
  - the 25% trap reduction lowers queen triggerfish risk to 0.06;
  - the no-take expansion lowers it to 0.06, and hogfish risk to 0.09.
* **+50% dive effort** (the herbivory test) raises hogfish and lobster risk (0.16 each), because divers also take those groups.
* **Native seagrass loss** of 2% per year lowers lobster MMSY by 13% and grunts by 21% through the nursery pathway.

**Validation.**
* **Leave-one-benchmark-out:**
  - The queen triggerfish OFL lies inside the predictive interval of a fit that excludes it (PIT 0.66).
  - The lobster OFL does not (prediction 774,000 lb). Lobster scale is identified only by assessment information, because the 1980s Opitz biomass prior is high relative to the modern stock.
* **Held-out indices:** the three placeholder series are covered at 90%, with MASE about 1.0–1.2. Because they are synthetic, this only demonstrates the procedure.
* **Simulation self-test:** 90% interval coverage is 0.79–0.93 and median bias is −19% to +12% across the 13 groups. Bias is 0.0% for queen triggerfish and −6% for lobster.

**Sensitivity and value of information.** Total focus-group MMSY is driven by:
* the biomass of the competing benthic predatory invertebrates (negative);
* the transfer efficiency, NPP and food-web fraction of colonized pavement, which is 53% of the benthic domain;
* reef NPP;
* the lobster partition.

The rank model explains 76% of the variance. Grouped by data stream, the first-order variance shares are:

| Data stream | Share of variance |
|---|---|
| Biomass surveys | 24% |
| Primary-production measurements | 18% |
| Transfer efficiency | 18% |
| Food-web flows | 10% |

For individual stocks, biomass surveys dominate: 46% for lobster, 37% for queen triggerfish and 57% for hogfish. Landings and the expansion factor show no prior variance share, because they act through calibration rather than through prior MMSY. Their value is shown instead by how strongly catch feasibility shapes the posterior.

**Structural uncertainty.** The 14 structural variants each took 7,500 draws (five rounds). The assessed stocks are structurally robust:
* lobster: pooled MMSY 279,000 lb (90% 146,000–554,000), between-variant CV 0.07;
* queen triggerfish: 23,300 lb (13,600–40,300), CV 0.10.

The unassessed herbivore and invertivore complexes are not: Parrotfish 2 CV 0.44, grunts 0.38.

The variants quantify each structural choice (variant medians against base):
* **Original form** (`gulland_PB`: x = 0.5 on P/B, Schaefer shape). MMSY rises for hogfish (+67%), porgies (+52%), queen triggerfish (+39%) and Parrotfish 2 (+91%). This is the effect of the theory correction.
* **Closed lobster population.** Lobster MMSY falls by 14%.
* **No thermal effects.** Lobster +13%, Parrotfish 2 +39%.
* **Food-web fraction ×1.5.** Parrotfish 2 MMSY triples.
* **Other variants.** Schaefer shape, nursery, ghost fishing, and lionfish/*Sargassum* change trap-group MMSY by less than 25%.

The herbivory feedback acts only in projections, so it does not change reference-year MMSY. Its effect appears in the `Parrotfish_harvest_plus50` scenario. The low-TTE variant has a small effective sample (ESS 8), so its medians are less precise.

**Independent comparator.** The catch-only (CMSY-style) analysis, which uses only landings and resilience, gives results close to the TTM and the OFLs for the assessed stocks:
* queen triggerfish: 28,400 lb (TTM 23,500; OFL 22,800);
* spiny lobster: 205,000 lb (TTM 250,000; OFL 164,000).

It also agrees closely for porgies, hogfish and Parrotfish 2 (TTM/CMSY 0.75–1.24). For grunts, shallow snappers and angelfishes the TTM is 8–19× higher. Those catch-only estimates are bounded by the placeholder landings, while the energy budget sees large unexploited production. This is where real landings and survey biomass would discriminate between the two views.

**Management strategy evaluation** (150 operating models, 2036–2045). The probability that any focus group falls below 0.5 B_MSY in some year is:

| Procedure | Probability |
|---|---|
| Status quo | 0.33 |
| 25% trap reduction | 0.31 |
| SPF effort-ceiling rule | 0.31 |
| SPF rule, +50% biased assessment | 0.41 |
| Catch-rate rule | 0.41 |

Queen triggerfish (0.17), hogfish (0.13) and porgies (0.06) account for the risk. The unbiased ceiling rule raises focus-group catch by about 11% over status quo at similar risk. The biased version raises catch by 68% at much higher risk, which quantifies the cost of optimistic assessments. The catch-rate rule lets effort drift upward (1.48×), because CPUE stays high while the choke groups decline.

**Ecosystem overfishing.** The Ryther index (0.61–0.66 t km⁻² yr⁻¹ in 2022–2024) is below the practical limit of 1. The Fogarty ratio (0.56–0.60‰) is in the 0.22–0.92‰ caution band.
