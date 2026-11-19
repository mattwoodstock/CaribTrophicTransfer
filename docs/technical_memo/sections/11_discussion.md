# Discussion

{{INCLUDE:technical_memo/discussion_results.md,0}}

## What the model can and cannot say

CaribTTM answers one question: **how much yield can each part of the St. Croix food web sustain, given the energy its habitats produce and how that energy is shared?** Its strengths:

* **Absolute scale from energy, not catch history alone.** Primary production and transfer efficiency bound total yield independently of the landings record. Assessment OFLs and catch feasibility then set how it is shared. This is most valuable for the many unassessed complexes, where landings-only approaches cannot separate low productivity from low exploitation.
* **Explicit mechanisms for conditions that are changing.** Coral loss, warming, hurricanes, lionfish, *Sargassum*, nursery degradation and fishing on herbivores all enter through documented pathways. Their projected effects can be compared by scenario.
* **Consistency between the yield cap and the dynamics.** The x priors and production shapes come from the same per-recruit analysis. Status calls (B/B_MSY) and yield caps therefore rest on one theory.
* **Gear-level outputs.** Effort ceilings and choke groups translate group caps into the units a trap-fishery manager controls.

Its limitations are equally important:

* **Groups, not species.** Biomass pools cannot represent size structure, growth overfishing (except through the per-recruit priors), sex change in groupers and parrotfishes, or spawning-aggregation dynamics.
* **No functional responses.** Pools are shared by demand weight, not by predator–prey functional responses. Strong top-down effects such as predator release are represented only through bottom-up fishing feedback.
* **Absolute production rates are transferred.** Benthic NPP values come from other Caribbean (and, for the reef range, Indian Ocean) sites. Food-web fraction and TTE are not measured for St. Croix. Together these set the overall scale, which the two OFL anchors pin only for their own groups.
* **Placeholder fishery data.** Until real landings, effort and assessment indices are used, the unassessed complexes' MMSY, the gear choke analysis and the MSE are demonstrations of the method.
* **Spatial aggregation.** The shelf is one spatial unit, with habitats as compartments. Lang Bank, the north and south shelves and the no-take areas are not spatially explicit.

## Relationship to assessments

CaribTTM does not replace single-species assessments.

* **For the two assessed stocks**, it uses their OFLs as anchors and adds ecosystem context: how their productive capacity depends on habitat, temperature and competitors, and how it may change. An OFL is a catch at F_MSY given current biomass, not an equilibrium MSY, so the model's MMSY and the OFL should agree only within the uncertainty that the CV of 0.4 allows.
* **For unassessed complexes**, it gives an energetically bounded estimate that can be compared with ACLs and, once data allow, with data-limited assessments.

The independent catch-only comparator shows where the energy budget and the catch history agree or disagree. Disagreement identifies which assumptions drive a result.

## Issues a CIE panel is likely to raise

The project's review-readiness and code-review documents (Appendix D) map anticipated critiques to responses. The most substantive are:

1. **Is the absolute scale credible?** The answer rests on:
   - the OFL anchors, tested by leave-one-out;
   - the structural ensemble (TTE, food-web fraction, x, production shape);
   - the catch-only comparator.
2. **Is the posterior dominated by a few draws?** The effective sample size, Pareto-k diagnostic and weight-truncation flag are reported. Wider adaptive sampling, or a sequential Monte Carlo sampler, would increase precision.
3. **Are ecological effects double-counted?** Nursery, refuge, *Sargassum* and lionfish effects act multiplicatively on capacity. Their combined effect is bounded by the structural variants that switch each off.
4. **How were placeholders chosen, and do they matter?** Every placeholder is listed with its value and basis (Appendix B). The value-of-information analysis ranks which ones matter most for MMSY.
