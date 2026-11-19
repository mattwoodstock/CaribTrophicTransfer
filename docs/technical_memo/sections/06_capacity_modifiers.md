# Carrying capacity and ecological modifiers

## From production to unfished biomass

Group production is converted to the biomass it can sustain, its carrying capacity:

$$
K_i(t) = \frac{P_i(t)\;\phi^P_i(t)\;m_i(t)}{M_i}\;\;\bar Q_i(t)^{\rho_i}\;\;\phi^K_i(t)\;\;N_i(t)^{\nu_i}
$$

where:
* $\phi^P_i$ and $\phi^K_i$ are environmental multipliers on production and capacity;
* $m_i$ is a disturbance multiplier on production;
* $\bar Q_i$ is the habitat quality the group experiences, with refuge exponent $\rho_i$;
* $N_i$ is the nursery-habitat index, with elasticity $\nu_i$.

**Why natural mortality.** In an unfished equilibrium, the production-to-biomass ratio equals total mortality, which then equals natural mortality (Allen 1971). Biomass that production $P$ can sustain without fishing is therefore $P/M$.

The original SEDAR103-DW-20 form divided by the Ecopath P/B instead. For a system already fished in the 1980s, that P/B estimates Z = M + F, so it understates unfished biomass and, combined with x = 0.5, overstates sustainable yield. CaribTTM keeps P/B for what it measures well (each group's production share in the partition) and uses natural mortality for K and MMSY.

Natural mortality for each group is the geometric mean, over the group's species, of the Then et al. (2015) growth-based estimator applied to FishBase growth parameters. Spiny lobster uses M = 0.34 from SEDAR 57. M is sampled with CV 0.3 (0.2 for lobster).

## Environmental responses

Covariates act on production or capacity through response functions:

| Type | Multiplier |
|---|---|
| linear | $\max(0,\,1+s\,(x-x_0))$ |
| log-linear | $\exp(s\,(x-x_0))$ |
| threshold | $\max(0,\,1 - s\,\max(0, x - x_{thr}))$ |
| thermal performance | $\exp[-\tfrac12((x-T_{opt})/\sigma_T)^2] / \exp[-\tfrac12((x_0-T_{opt})/\sigma_T)^2]$ |

$x_0$ defaults to the historical mean, so the multipliers are 1 under average historical conditions. Sensitivities carry CVs.

### Temperature

Thermal performance curves are parameterized from AquaMaps temperature preferences:
* FishBase v24.07 for the reef fishes;
* SeaLifeBase v24.07 for spiny lobster (23.7 / 26.7 / 28.0 °C, minimum / mean / maximum preference) and queen conch (25.3 / 27.0 / 28.0 °C).

The optimum is the mean preference, and the width σ is half the min–max preference range.

These curves describe the **realized niche** (where species occur), not physiological performance, so their use as performance curves is an assumption. It is tested by the `no_thermal` ensemble variant. Deepwater snappers have no curve, because the shelf-surface temperature is a poor proxy for their environment.

### Lionfish

A lionfish invasion covariate is 0 before 2009, rises through 2009–2011, and is 1 from 2012 (first USVI records 2008–2009; Schofield 2010). It reduces carrying capacity log-linearly:
* by up to 10% for small-bodied prey groups and recruits: squirrelfishes, goatfishes, wrasses, grunts, Parrotfish 2 and angelfishes (Albins & Hixon 2008; Green et al. 2012);
* by up to 5% for native mesopredators through competition: shallow groupers and snappers (Albins 2013).

The magnitudes are placeholders with CV 1.0. NCRMP lionfish densities and fishers' timelines would calibrate them.

### Pelagic *Sargassum*

The annual *Sargassum* index is the mean monthly fraction of cloud-free pixels with positive floating-algae signal. It uses the University of South Florida Alternative Floating Algae Index (AFAI; Hu et al. 2015), in 7-day composites distributed on the NOAA AOML ERDDAP server, within an offshore box around the shelf (2016–2024). The index is expressed relative to its 2016–2024 mean:
* **2016–2024:** from AFAI. The record years 2018 and 2021 stand out.
* **2011–2015:** relative magnitudes from Wang et al. (2019), as placeholders.
* **Before 2011:** 0 (Gower et al. 2013).

Mass strandings degrade nearshore seagrass and mangrove nurseries (van Tussenbroek et al. 2017). The index therefore reduces K of nursery-dependent groups by 3% per index unit. Floating *Sargassum* is habitat for dolphinfish and other pelagics (Casazza & Ross 2008), so it increases K of the pelagics group by 3% per unit. Both magnitudes are placeholders with CV 1.0.

## Habitat refuge and coral dynamics

Groups that depend on reef structure for shelter lose capacity as live coral and rugosity decline. This is the mechanism by which coral loss reduces fish carrying capacity. It is represented as $\bar Q_i^{\rho_i}$, where:
* $\bar Q_i$ is the habitat-use-weighted habitat quality;
* $\rho_i$, the refuge dependence, ranges from 0 (pelagics, jacks) to 0.7 (groupers). The values are expert judgement.

**Coral reef quality** is coral cover relative to a reference cover (0.08, about the 2013 NCRMP cover). In historical years with observed cover, the state is set to the observations: approximate TCRMP/NCRMP magnitudes from 2000, and a placeholder decline before that. In projections it follows:

$$
c(t+1) = \Big[c + g\,\Gamma(t)\,c\,(1 - c/c_{max})\Big]\,(1-\beta(t))\,(1-\mu_{dis}(t))\,(1-\eta(t))
$$

where:
* $g$ = 0.10 yr⁻¹ (CV 0.3) is intrinsic growth and $c_{max}$ = 0.30;
* $\beta(t)$ = 0.03 × max(0, DHW − 4) is bleaching mortality (CV 0.3);
* $\mu_{dis}$ is disease mortality, e.g., SCTLD in a scenario;
* $\eta$ is hurricane loss.

These coral parameters are literature-typical Caribbean values. They matter only in projections, because cover is observed throughout the history.

### Herbivory feedback

Parrotfish and surgeonfish grazing keeps macroalgae in check and allows coral recruitment and recovery (Mumby et al. 2006, 2007). In projections, coral growth is multiplied by

$$
\Gamma(t) = \min\!\Big(2,\; \big[G(t-1)/G_{ref}\big]^{\gamma}\Big), \qquad G = \frac{\sum_{herb} B}{\sum_{herb} K_0}
$$

where:
* $G$ is grazing intensity (herbivore biomass relative to unfished biomass) for Parrotfish 1, Parrotfish 2 and surgeonfishes;
* $G_{ref}$ is its 2022–2024 mean;
* $\gamma$ = 1 (CV 0.5) is a placeholder elasticity.

Fishing herbivores therefore slows coral recovery. That reduces the refuge capacity of reef-dependent groups, which is the main ecosystem trade-off behind the parrotfish management measures. The formulation approximates, but does not reproduce, the hysteresis of Mumby et al. (2007). Explicit alternative stable states are a recommended extension.

## Nursery habitats

Many reef fishes and invertebrates settle in, and depend on, seagrass, mangrove or macroalgal nurseries. Adult biomass on reefs scales with nursery availability (Nagelkerken et al. 2002; Mumby et al. 2004; Adams et al. 2006). The nursery index is

$$
N_i(t) = \frac{\sum_{h \in \text{nursery}(i)} A_h(t)\,Q_h(t)}{\sum_{h \in \text{nursery}(i)} A_h(1983)\,Q_h(1983)}
$$

and capacity scales as $N_i^{\nu_i}$. St. Croix uses these elasticities, all placeholders:

| Group | Nursery habitats | ν |
|---|---|---|
| Grunts | seagrass, mangrove | 0.5 |
| Queen conch | seagrass | 0.5 |
| Shallow snappers | mangrove, seagrass | 0.4 |
| Parrotfish 1 | mangrove, seagrass | 0.4 |
| Yellowtail snapper | seagrass | 0.3 |
| Spiny lobster | macroalgae, seagrass | 0.3 |
| Parrotfish 2 | seagrass | 0.2 |
| Hogfish | seagrass | 0.2 |

Nursery loss can come from area trends (e.g., the `Seagrass_loss_Halophila` scenario), hurricanes or quality declines.

## Disturbances

Hurricanes are acquired from the NOAA National Hurricane Center HURDAT2 best-track archive. Every storm passing within 200 km of the island in 1983–2024 is included. Each storm's intensity is

intensity = (max wind − 34 kt) / (137 − 34 kt) × distance decay

where distance decay = 1 within 50 km and declines with a 60 km e-folding scale beyond that. Intensity carries CV 0.3.

Each event applies the `major_hurricane` template losses, scaled by intensity and recovering exponentially:
* primary production of reef, pavement, seagrass, macroalgae and mangrove −25%, half-life 2 yr;
* coral cover −15%;
* fish- and lobster-trap effort −60%, half-life 1.5 yr;
* spiny lobster production −20%.

The companion template for other gears reduces dive and hook-and-line effort by 30% (half-life 1 yr).

A common multiplier with CV 0.5 is applied to all loss magnitudes, because the template magnitudes are placeholders.
