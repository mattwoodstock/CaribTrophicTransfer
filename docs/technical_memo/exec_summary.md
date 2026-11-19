<!-- executive summary -->
**Purpose.** CaribTTM estimates how much yield each functional group, guild and gear in the St. Croix trap fishery can sustain, given:
* the primary production of the island's habitats;
* how efficiently that production moves up the food web;
* how it is shared among fished and unfished consumers;
* how warming, coral loss, hurricanes, invasive lionfish, *Sargassum* and fishing modify it.

Its outputs (group, guild and gear MMSY, effort ceilings and choke groups) are the biological inputs to the stochastic production frontier being developed for the SEDAR 103 trophic frontier framework.

**What was built.**
* **An R package** with:
  - a habitat-resolved energy budget;
  - theory-consistent population dynamics (natural-mortality basis, per-recruit priors for x and B_MSY/B₀, Pella–Tomlinson production);
  - ecological processes that theory and expert review require: open recruitment, nursery habitats, herbivory–coral feedback, no-take areas and ghost fishing;
  - automated acquisition of eleven public data products, with user override.
* **Full uncertainty propagation**, with adaptive importance sampling conditioned on assessment OFLs and on the feasibility of historical landings.
* **A review toolkit:**
  - out-of-sample validation;
  - a simulation self-test;
  - sensitivity and value-of-information analyses;
  - a 14-variant structural ensemble;
  - an independent catch-only comparator;
  - closed-loop management strategy evaluation.

**Main St. Croix findings.** These are conditional on placeholder landings and effort.

1. **The two assessed stocks are reproduced.**
   * Queen triggerfish MMSY is 23,500 lb (90% interval 12,500–37,300), against an OFL of 22,773. It passes leave-one-out validation: predicted without its OFL, the 90% interval of 10,300–47,900 lb contains the OFL.
   * Spiny lobster MMSY is 250,000 lb (120,000–441,000), 1.5× its OFL. An independent catch-only analysis gives 205,000 lb. Without its OFL, lobster scale is poorly identified (the leave-one-out prediction misses the OFL), so the SEDAR 57 inputs are critical.
2. **Small trap-bycatch groups limit the fishery, not lobster.**
   * Fish traps and diving are already at their multispecies effort ceilings (median 0.95× and 0.96× current effort). Hogfish is the choke group in more than half of draws.
   * Lobster traps could expand by about 28% before queen triggerfish bycatch limits them (73% of draws).
   * At current catch, the probability of overfishing is 35% for hogfish, 16% for porgies and 16% for queen triggerfish. It is 7% for lobster and 9% for Parrotfish 2.
3. **Climate is the largest projected stressor.** By 2043–2045 under SSP5-8.5, MMSY falls by about 43% for Parrotfish 2, 32% for grunts, 25% for spiny lobster and 16–22% for queen triggerfish, hogfish and porgies. Combined stresses double the overfishing probability of queen triggerfish (0.08 to 0.16) and raise that of hogfish by half (0.12 to 0.18).
4. **Management levers work in the expected direction.**
   * Expanding no-take areas or cutting trap effort by 25% lowers the overfishing risk of the choke groups by about a quarter.
   * Open-access effort dynamics roughly double or triple it.
   * In closed-loop tests, the SPF effort-ceiling rule performs slightly better than status quo. A 50% optimistic assessment makes it worse than status quo, which shows the value of accurate assessments for these choke groups.
5. **The ecosystem as a whole is not overfished by Link & Watson's (2019) Ryther criterion** (0.61–0.66 t km⁻² yr⁻¹ in 2022–2024). The Fogarty ratio (0.56–0.60‰) sits in the caution band.
6. **What would most improve the estimates.** The value-of-information analysis ranks four data streams that together account for about 70% of the first-order variance in total trap-group MMSY:
   * fishery-independent biomass surveys (NCRMP), especially of competing benthic invertebrates;
   * benthic primary-production measurements;
   * transfer efficiency;
   * the food-web fraction of colonized pavement.

   Real landings and effort, and the SEDAR 57/80 CPUE series, determine the calibration and are the highest-priority data overall.

**Status.** The model, data pipeline and diagnostics are complete and tested (124 unit tests). Every assumption is listed in a register with its basis and how it is tested. The results illustrate the method. Management use requires replacing the placeholders listed in Section 13.
