# Trophic transfer and partitioning

## Trophic-level energy pools

Within each benthic habitat and in the pelagic domain, production moves up a Lindeman (1942) chain of four trophic-level pools:

$$
E_{h,1}(t) = P1_h(t), \qquad E_{h,k+1}(t) = \tau_h\,\big[E_{h,k}(t) - R_{h,k}(t)\big], \quad k = 1,2,3
$$

where:
* $\tau_h$ is the trophic transfer efficiency (TTE) of habitat $h$;
* $R_{h,k}(t)$ is the previous year's catch of groups feeding at trophic level $k$, allocated to habitats by each group's habitat use (**fishing feedback**).

Removing catch from the pool where it was produced means heavy fishing of lower-trophic-level groups reduces the energy reaching higher levels, and the reverse.

**TTE.** TTE is 0.15 with CV 0.2 in every pathway. This is the SEDAR103-DW-20 default, slightly above the global mean of about 0.10 (Pauly & Christensen 1995) and consistent with efficient tropical reef systems. TTE is a major lever on absolute scale and is tested at 0.10 and 0.20 in the structural ensemble.

**Two pool variants** are computed every year: $E$, with fishing feedback, and $E_0$, without it. They give prevailing MMSY and unfished-ecosystem MMSY₀ respectively.

## Diet and trophic level

Each group's diet is a vector of fractions over the eight pools (B_TL1–4, P_TL1–4). Its trophic level follows as TL$_i$ = 1 + Σ$_k$ D$_{ik}$ TL$_k$. For St. Croix:
* Diets come from food-habits descriptions (Randall 1967) and the Opitz (1996) diet matrix.
* Each FMP group's pools are adjusted so that its implied trophic level matches the FishBase estimate (Froese & Pauly 2024).

Diets are sampled from a Dirichlet distribution centred on the table, with concentration 40.

## Energy partitioning

The production of pool $k$ in habitat $h$ is shared among consumers in proportion to their **demand**:

$$
s_{ihk} = \frac{w_i\,D_{ik}\,\pi_{ih}}{\sum_j w_j\,D_{jk}\,\pi_{jh}}, \qquad
P_i(t) = \sum_h \sum_k \tau_h\, s_{ihk}\, E_{h,k}(t)
$$

where:
* $w_i$ is the group's demand weight;
* $D_{ik}$ is its diet fraction from pool $k$;
* $\pi_{ih}$ is its relative preference for habitat $h$.

Pelagic pools are shared the same way, without habitat preferences. Summed over consumers, production equals TTE × pool production, so **energy is conserved by construction**. The package's unit tests check this.

**Demand weights.** These follow the partition method recorded in each group's input row. For St. Croix every group uses the `biomass` method, with demand weight $w_i$ = biomass density × P/B. This is the group's production density, as in an Ecopath mass balance:
* **Biomass densities** for FMP fish come from the Opitz (1996) Virgin Islands species biomasses, multiplied by the factor 0.239 that the balanced Table 3.22 model applied to the raw survey biomass.
* **Lobster and competitor groups** use the balanced model directly.
* **Groups with no direct data** use wide priors (CV 1.0): queen conch, pelagics, deepwater snappers, wrasses and porgies.

Because absolute scale is pinned by energy, the partition determines **how** production is shared. It is the parameter the data (benchmarks and catch feasibility) most directly update. In the global sensitivity analysis, each group's own partition is the leading driver of its MMSY.

## Habitat use

Habitat preferences $\pi_{ih}$ (0–1) are expert judgements based on species habitat descriptions (Randall 1967; FishBase and SeaLifeBase habitat fields). Each element is sampled with CV 0.2. They do three things:
1. route energy, since a group can only take energy from habitats it uses;
2. allocate catch to habitats for the fishing feedback;
3. determine the coral quality, nursery and no-take shares the group experiences.

NCRMP occupancy by habitat stratum would replace these judgements once the survey data can be ingested.

## Non-fishery competitors

Energy not claimed by modelled consumers would be credited to the fished groups. Ten non-fishery competitor groups therefore claim their shares of the pools. Their biomasses and P/B ratios come from the Opitz (1996) balanced model, aggregated by trophic role. The benthic predatory invertebrates (crabs, octopus and others) compete strongly with spiny lobster, queen triggerfish and other invertivores. Their biomass is a top driver of total focus-group MMSY (Section 10), which identifies an important data need.
