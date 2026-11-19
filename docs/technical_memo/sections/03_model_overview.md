# Model overview

## Conceptual structure

CaribTTM is an annual, habitat-resolved, energy-budget model coupled to surplus-production dynamics for each functional group. Each year it follows a fixed sequence:

1. **Habitat state.** Mapped habitat areas and qualities are updated for the year. Coral cover is either observed or modelled (growth, bleaching, disease, hurricane loss, herbivory). Other habitats are modified by disturbance multipliers and scenario area trends.
2. **Primary production.** Each benthic habitat supplies net primary production (area × rate × annual index × quality effect), and the pelagic habitat supplies phytoplankton production. A habitat-specific food-web fraction enters the consumer food web, converted from carbon to wet weight.
3. **Trophic-level energy pools.** Production cascades through four trophic-level (TL) pools per pathway, benthic (B_TL1–4) and pelagic (P_TL1–4). Each step is multiplied by the transfer efficiency. The previous year's catch is removed from the pool at the trophic level where it was produced (fishing feedback).
4. **Partitioning.** Each pool's production is shared among the groups that feed on it. Shares are proportional to the group's demand weight, its diet fraction from that pool and its preference for that habitat.
5. **Carrying capacity.** Group production, multiplied by environmental and disturbance effects, is converted into unfished biomass (K = P/M). K is then modified by structural refuge (coral quality) and nursery habitat.
6. **Reference points.** MMSY = x·M·K, with x and the production shape from per-recruit life-history theory.
7. **Biomass dynamics.** Biomass is updated in monthly sub-steps under surplus production. Landings (historical) or effort (projections) set the catch. Other terms add open recruitment, ghost-fishing mortality and immigration, and annual process error is applied.
8. **Feedbacks.** Herbivore biomass sets coral recovery in the following year. Catch feeds back on the energy pools. Projected effort can respond to catch rate or to a management procedure.

The model is implemented as an R package (`CaribTTM`). It runs over Monte Carlo draws of every uncertain input and over a set of scenarios, and is conditioned on data by importance sampling (Section 9).

## Functional groups

The model distinguishes:

* **FMP groups:** 19 St. Croix FMP complexes or sub-complexes that are fished.
  - Parrotfish 1 is included as a harvest-prohibited group.
  - The 13 groups with at least 25% of landings from traps, plus the two assessed stocks, form the **calibration focus**.
  - The others (queen conch, yellowtail snapper, deepwater snappers, jacks and pelagics) are carried in the food web as energy competitors whose landings are removed, but they do not constrain calibration. They are reported as "Outside focus (uncalibrated)".
* **Non-fishery competitor groups:** 10 groups representing the rest of the consumer community, so that FMP groups do not receive energy that in reality supports other taxa:
  - benthic grazers;
  - benthic detritivores and filter feeders;
  - benthic predatory invertebrates;
  - small reef fishes;
  - other invertivore fishes;
  - other reef predators;
  - sharks;
  - small pelagics;
  - squids and pelagic predators;
  - zooplankton.

  Their biomasses and P/B ratios come mainly from the balanced Opitz (1996) Virgin Islands Ecopath model.

## Time structure and scenarios

The historical period (1983–2024) is driven by reported landings and the observed or acquired environmental series. The projection period (2025–2045) is driven by effort. Catchability is estimated in each draw from the last three historical years.

Scenarios modify drivers over the projection period:
* **Climate:** CMIP6 SSP2-4.5 and SSP5-8.5 SST and NPP projections with bleaching years.
* **Disturbance:** a major hurricane in 2030.
* **Effort:** trap effort +30%; trap effort −25%.
* **Disease:** continued SCTLD coral loss.
* **Combined stress.**
* **Management:** no-take area expansion; open-access effort dynamics; +50% dive effort, which tests the herbivory feedback.
* **Habitat:** loss of native seagrass to the invasive *Halophila stipulacea*.

## Outputs

For every draw, scenario, year and group, the model reports:
* biomass;
* carrying capacity (prevailing and unfished-ecosystem);
* production;
* MMSY (prevailing, and without fishing feedback);
* catch by gear;
* fishing mortality;
* the fishable fraction;
* the nursery index;
* ghost-fishing removals.

Summary tables for the SPF hand-off:
* group reference points: MMSY quantiles, OFL proxy = median, P* target = 40th percentile, B/B_MSY, P(overfishing), status;
* guild and complex aggregates;
* the gear targeting matrix;
* gear-specific MMSY;
* effort ceilings with choke groups;
* draw-level MMSY distributions;
* MMSY time series.

Ecosystem-level overfishing indices (Ryther and Fogarty; Link & Watson 2019) are also reported.
