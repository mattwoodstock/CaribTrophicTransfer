<!-- discussion of results -->
## Interpretation of the St. Croix results

**Theory changes the answer.** Two changes give the trap bycatch groups much less headroom than the original SEDAR103-DW-20 formulation did:
* replacing x = 0.5 on Ecopath P/B with per-recruit x on natural mortality;
* replacing Schaefer with Pella–Tomlinson production.

Under the earlier form, every trap group was "Below target" with P(overfishing) of at most 3.4%. Under the theory-consistent form, hogfish, porgies and queen triggerfish carry overfishing probabilities of 16–35% at current (placeholder) catch. Fish-trap and dive effort are at their multispecies ceilings.

This direction of change is what fisheries theory predicts. Gulland's x = 0.5 is an upper bound for species whose gear selectivity, maturity and steepness keep F_MSY below M (Beddington & Cooke 1983; Zhou et al. 2012). The `gulland_PB` structural variant quantifies the change.

**The binding constraint is bycatch, not lobster.** Lobster and Parrotfish 2 dominate the value of the trap fishery and have substantial headroom. The groups that limit fish-trap and dive effort are small, slow-turnover bycatch species. This matches the logic of the SEDAR 103 framework: in a multispecies, non-selective fishery, effort ceilings are set by the weakest stock the gear catches. Management options that protect choke groups without cutting all effort would raise sustainable value. Examples include escape gaps or mesh changes for small fishes, and spatial closures where those species concentrate. The model can evaluate such options as scenarios once their effect on group-specific catchability is specified.

**The lobster anchor matters most.** Lobster MMSY is 1.5× the OFL. The catch-only comparator lies between the two. Without its OFL, the energy budget overestimates lobster scale several-fold. Lobster is a large-bodied invertivore that shares the invertivore pools with abundant benthic predatory invertebrates, and the biomass of those competitors is the single largest driver of total MMSY. Fishery-independent estimates of lobster and invertebrate-competitor biomass, and the SEDAR 57 CPUE as a fitted index, are therefore the most valuable additions for the trap fishery.

**Climate effects are large but uncertain.** Projected MMSY declines of 16–43% under SSP5-8.5 come mainly from the thermal-performance curves. These are realized-niche curves, so the magnitudes are assumptions (Appendix B, A18). They are tested by the `no_thermal` ensemble variant. The direction is robust: every focus group has an optimum near or below current St. Croix temperatures. The magnitude would benefit from physiological thermal-performance data for the key species.

**Management procedures.**
* The closed-loop results caution against relying on catch-rate rules for this fishery. CPUE in a multispecies trap fishery is dominated by the abundant groups, so it can stay high while choke groups decline.
* The SPF effort-ceiling rule tracks the choke groups and performs better, but only with unbiased assessments.
* In none of the procedures tested is the risk to queen triggerfish and hogfish below about 10–15%. Achieving that would need lower effort on the gears that catch them, or gear and area measures that reduce their catchability.
