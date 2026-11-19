# Model Description and Capabilities

This chapter describes the algorithm in `CaribTTM`. CaribTTM is the trophic transfer model (TTM) component of the integrated trophic frontier framework proposed in SEDAR103-DW-20 (Karnauskas et al. 2026). It covers the equations, the reasoning behind each design choice, and what the model can and cannot do.

## 1. Purpose and position in the management framework

SEDAR103-DW-20 proposes a four-phase approach:

1. **Biomass caps.** A trophic transfer model (TTM) estimates multispecies maximum sustainable yield (MMSY) for each guild and FMP stock complex.
2. **Production frontiers.** A stochastic production frontier (SPF) uses trip-ticket data to estimate fleet capacity by gear.
3. **Management framework.** Effort limits are derived by aligning the TTM caps with SPF capacity.
4. **Simulation testing.** Management strategy evaluation (MSE) tests the framework in closed loop.

`CaribTTM` implements **phase 1**. It also produces every input that phases 2–3 need from the biological side: group and guild MMSY distributions, gear-specific MMSY, gear targeting matrices, effort ceilings and choke groups (Section 9). The SPF itself is developed separately by collaborators and is **not** part of this package.

Compared with the original proof-of-concept script (preserved in `inst/legacy/`), the upgraded algorithm adds:

| Capability | Legacy script | CaribTTM 0.2 |
|---|---|---|
| Benthic primary production | single random number | habitat-resolved (area × habitat-specific net PP), with literature unit conversion |
| Pelagic primary production | single random number | area × NPP rate × annual index (satellite ensemble, MOM6, GLORYS) |
| Habitat–species linkage | none | habitat-preference matrix; habitat-resolved energy pools; habitat-quality refuge effects on capacity |
| Biomass partitioning | "diet matrix" rows treated as pool shares (not conserved) | energy-conserving, demand-weighted partitioning with six partition methods incl. assessment-fixed biomass |
| Time dynamics | single snapshot | annual time steps; logistic biomass dynamics with time-varying K; history driven by landings, projections by effort |
| Trophic feedback of fishing | none | last-year catch removed from the trophic pool it was produced in (energy cascade) |
| Perturbations | one temperature term | covariate responses (linear, log-linear, threshold, thermal performance), hurricanes, bleaching (DHW), coral disease, habitat area change, NPP trends, effort shocks |
| Uncertainty | uniform catch noise | Monte Carlo over every uncertain input + SIR conditioning on indices and assessment benchmarks |
| MMSY definition | not computed | MMSY = x · (P/B) · K (Gulland/Schaefer), prevailing and no-feedback variants, quantiles, OFL proxy and P* target |
| Gear outputs | none | gear MMSY, targeting matrix, effort ceilings, choke-group probabilities, SPF-ready tables |
| Indicators | energy utilization only | energy utilization, catch/MMSY, B/BMSY, mean TL of catch, pelagic:demersal ratio, biomass and PP indicators |

## 2. Model structure

The model domain is one island platform (an FMP management unit), represented by:

* **Habitats** *h*: one or more benthic habitats (coral reef, colonized pavement, seagrass, macroalgae, sand, mangrove, mesophotic hardbottom, …) and at most one pelagic habitat.
* **Energy pools**: four trophic-level pools (TL1–TL4) in each benthic habitat and four in the pelagic habitat. TL1 holds primary producers plus detritus.
* **Functional groups** *i*: FMP stock complexes (fishery groups) and non-fishery groups (zooplankton, benthic invertebrates, small reef fish, apex predators, …). The non-fishery groups compete for the same energy, so FMP groups are not handed all of it.
* **Gears** *g*: fleets defined in landings and effort data.

Time runs in annual steps. Biomass dynamics are integrated within each year using `substeps` sub-steps (default 12, i.e. monthly).

## 3. Primary production (energy input)

Annual primary production entering the food web of habitat *h* in year *t*, in t wet weight (WW) per year:

$$
P1_h(t) = A_h\,a_h(t)\;\cdot\;\text{NPP}_h\;\cdot\;I_h(t)\;\cdot\;d_h(t)\;\cdot\;Q_h(t)^{\epsilon_h}\;\cdot\;f_h\;\cdot\;c_{WW}
$$

| Symbol | Meaning | Input |
|---|---|---|
| $A_h$ | habitat area (km²) | `habitats.csv: area_km2` |
| $a_h(t)$ | scenario area multiplier (habitat loss/gain) | `scenarios.csv: area_trend, area_mult` |
| $\text{NPP}_h$ | net primary production (t C km⁻² yr⁻¹), converted from source units | `npp_value`, `npp_unit` |
| $I_h(t)$ | NPP index (annual anomaly ratio from satellite/ocean model) | `npp_covariate` + `env_covariates.csv`; `npp_mult`, `npp_trend` |
| $d_h(t)$ | disturbance multiplier (hurricanes etc.), with exponential recovery | `disturbances.csv: p1_loss, half_life_yr` |
| $Q_h(t)^{\epsilon_h}$ | habitat-quality effect on production | `p1_quality_elasticity` |
| $f_h$ | food-web fraction: share of NPP that enters metazoan grazing and detrital food webs, after export, burial and microbial respiration | `food_web_fraction` |
| $c_{WW}$ | wet weight per unit carbon (default 9; Pauly & Christensen 1995) | `settings: c_to_ww` |

**Net, not gross.** Trophic transfer efficiency applies to *net* production. Gross values (e.g., $P_g$ in the benthic productivity synthesis) should not be entered as `npp_value`.

**Unit conversion** (`convert_productivity()`), all to t C km⁻² yr⁻¹:

| Unit code | Conversion |
|---|---|
| `g_C_m2_d` | × 365 |
| `g_C_m2_yr`, `t_C_km2_yr` | × 1 |
| `kg_C_ha_yr` | × 0.1 |
| `mg_C_m2_d` | × 0.365 |
| `mmol_O2_m2_d` | × 12×10⁻³ × 365 / PQ = 4.38 / PQ |
| `g_O2_m2_d` | × (12/32) × 365 / PQ |
| `g_DW_m2_d`, `g_DW_m2_yr`, `t_DW_km2_yr` | × C:DW (0.34) (× 365 for daily) |
| `g_OM_m2_d` | × C:OM (0.45) × 365 |

The identity used throughout: 1 g m⁻² = 1 t km⁻². PQ is the photosynthetic quotient (default 1).

## 4. Trophic-level energy pools

Energy moves up a Lindeman spine within each habitat. A pathway-specific trophic transfer efficiency $\text{TTE}_h$ (SEDAR103-DW-20 default 15%, sensitivity 10–20%) applies at each step:

$$
E_{h,1}(t) = P1_h(t), \qquad E_{h,k+1}(t) = \text{TTE}_h(t)\,\big[E_{h,k}(t) - R_{h,k}(t-1)\big]
$$

$R_{h,k}(t-1)$ is the **fishing feedback**: the previous year's catch, removed from the pool at the trophic level where it was produced.

* Group *i* has a fractional trophic level $TL_i$ (Section 5). Its catch is split between pools $\lfloor TL_i\rfloor$ and $\lceil TL_i\rceil$ by linear weights.
* Benthic catch is split across habitats in proportion to the group's habitat use: $u_{ih} = \pi_{ih} A_h / \sum_h \pi_{ih} A_h$, where $\pi$ is the habitat preference.
* Pelagic-pathway catch is removed from the pelagic pools.

Catch of a prey group therefore lowers the energy available to its predators in the following year. This is a simple, energy-based multispecies interaction. Switch it off with `fishing_feedback = FALSE`.

## 5. Diet, trophic level and energy partitioning

### Diet

Each group has a diet vector $D_{ik}$ over the eight pools (`B_TL1..4`, `P_TL1..4`), with rows summing to 1. As SEDAR 103 slide 10 notes, the TTM only needs **trophic-level-specific feeding**, not species-to-species predation rates. This lowers the data bar compared with Ecopath.

$$
TL_i = 1 + \sum_k D_{ik}\,TL_k,\qquad TL_k\in\{1,2,3,4\}
$$

### Partitioning a pool among its consumers (energy conserving)

Each consumer *i* of pool *k* in habitat *h* has a demand

$$
\delta_{ihk} = w_i(t)\,D_{ik}\,\pi_{ih}
$$

and receives the share $s_{ihk} = \delta_{ihk}/\sum_j \delta_{jhk}$. Group production is:

$$
P_i(t) = \sum_{h}\sum_{k}\text{TTE}_h\,s_{ihk}\,E_{h,k}(t) \;+\; \sum_k \text{TTE}_{pel}\,\frac{w_i D_{ik}^{pel}}{\sum_j w_j D_{jk}^{pel}}\,E_{pel,k}(t)
$$

Summed over all consumers of a pool, production equals $\text{TTE}\times E$, so energy is conserved. Tests verify this.

### Partition weights $w_i$

These implement the options on SEDAR 103 slide 9. Methods can be mixed among groups.

| `partition_method` | $w_i$ | Use when |
|---|---|---|
| `fixed_biomass` | calibrated so that first-year unfished biomass $K_{0,i} = $ `B0_fixed_t` | a benchmark assessment or survey estimate of B₀ exists (e.g., spiny lobster, queen conch). The remaining energy is divided among the other groups. |
| `biomass` | `partition_value` (t km⁻² survey or literature density) × P/B | fishery-independent surveys (NCRMP/DCRMP) or Ecopath priors |
| `catch_proportion` | proportion in non-selective gear catch × P/B | species composition of trap catch among same-TL species |
| `landings_proportion` | proportion of reported landings × P/B | backup to catch proportions |
| `uniform_pb` | P/B | no information: split uniformly, weighted by turnover |
| `weight` | `partition_value` directly | expert-specified weights |

Each weight carries a CV (`partition_cv`). Groups with weaker partitioning information therefore carry more MMSY uncertainty. Use `data_tier` (1 = assessed, 2 = survey/indicator, 3 = assumed) to report this.

## 6. Carrying capacity, habitat refuge and environment

Energetically supported biomass (carrying capacity) in year *t*:

$$
K_i(t) = \frac{P_i(t)\,\phi_i^{P}(t)\,m_i(t)}{M_i}\;\bar Q_i(t)^{\rho_i}\;\phi_i^{K}(t)\;N_i(t)^{\nu_i}
$$

* $M_i$ is natural mortality (`m_nat`). Unfished biomass turns over at M, so $P/M$ is the biomass that production $P$ can sustain. If `m_nat` is blank, $M_i = (P/B)_i$, which is the original formulation. The Ecopath P/B of an exploited system equals total mortality Z = M + F. It still sets the group's **share** of production (partition weights), but no longer converts production into unfished biomass.
* $N_i(t)$ is the **nursery-habitat index**: area × quality of the group's nursery habitats (`nursery_habitat`) relative to the first year. $\nu_i$ (`nursery_dependence`) is the elasticity of capacity to nursery availability. Juvenile survival in seagrass, mangrove or macroalgal nurseries limits the adults a reef can hold (Nagelkerken et al. 2002; Mumby et al. 2004).
* $\phi_i^{P}(t)$ and $\phi_i^{K}(t)$ are environmental multipliers on production and on capacity (`env_response.csv`, Section 7).
* $m_i(t)$ is a group-specific disturbance production multiplier, for example juvenile lobster habitat loss after a hurricane.
* $\bar Q_i(t) = \sum_h u_{ih} Q_h(t)$ is the habitat quality experienced by the group. $\rho_i$ (`refuge_dependence`) sets how strongly capacity depends on structural refuge. For example, lobster and groupers depend on reef rugosity: as live coral and structure decline, predation increases and fewer individuals can be supported.
* $K_{0,i}(t)$ is the same quantity computed with no fishing feedback. It is the **unfished ecosystem potential** under prevailing environmental and habitat conditions.

### Habitat linkage, summarized

Habitat availability (km²) reaches the species through three routes:

1. **Primary production.** Area × habitat-specific NPP sets the energy entering each habitat's pools.
2. **Access to energy.** Habitat-specific pools are shared only among groups that use that habitat, weighted by preference. Losing seagrass area therefore mainly reduces the groups that feed in seagrass (grunts, conch, goatfishes).
3. **Survival/refuge.** Habitat *quality* (coral cover or any covariate) scales carrying capacity through the refuge exponent.

### Habitat-quality dynamics

* `quality_model = "coral"`: coral cover $c(t)$ is a state variable. Where observations exist (`quality_covariate`, e.g., NCRMP coral cover) it is set to them during the historical period. Otherwise it follows:

  $$
  c(t+1) = \Big[c + g\,c\,(1 - c/c_{max})\Big](1-\beta(t))(1-\mu_{dis}(t))(1-\eta(t)),\qquad
  \beta(t) = b\cdot\max(0, \text{DHW}(t) - \text{DHW}_{thr})
  $$

  Here $\beta$ is bleaching mortality (degree heating weeks), $\mu_{dis}$ is disease mortality (e.g., SCTLD), and $\eta$ is hurricane cover loss. Quality is $Q = c / c_{ref}$.

  **Herbivory feedback.** In projection years the coral growth rate $g$ is multiplied by $(G(t-1)/G_{ref})^{\gamma}$, capped at 2. $G = \sum B_{herb} / \sum K_{0,herb}$ is the grazing intensity of the `herbivore_groups` (parrotfishes, surgeonfishes), and $G_{ref}$ is its mean over the reference years. Fishing herbivores therefore slows coral recovery, which lowers habitat quality and the refuge capacity of reef-dependent groups (Mumby et al. 2006, 2007). $\gamma$ is `herbivory_elasticity` (with a CV).
* `quality_model = "covariate"`: $Q$ = covariate / `quality_ref`. Use this for rugosity, seagrass shoot density, water clarity, etc.
* `quality_model = "none"`: $Q$ = disturbance multiplier only.

## 7. Oceanographic and environmental forcing

Any annual covariate can drive the model: SST, bottom temperature, DHW, salinity, O₂, NPP index, *Sargassum* index, coral cover, and so on. Covariates are entered in long format (`env_covariates.csv`). Group responses (`env_response.csv`):

| `type` | Multiplier |
|---|---|
| `linear` | $\max(0,\,1+s\,(x-x_0))$ |
| `loglinear` | $\exp(s\,(x-x_0))$ |
| `threshold` | $\max(0,\,1 - s\,\max(0, x - x_{thr}))$ |
| `thermal_performance` | $\exp[-\tfrac12((x-T_{opt})/\sigma_T)^2] \,/\, \exp[-\tfrac12((x_0-T_{opt})/\sigma_T)^2]$ |

$x_0$ (`baseline`) defaults to the historical mean, so multipliers equal 1 under average historical conditions. Sensitivities carry CVs and are sampled in Monte Carlo.

**Projections and ocean models.**

* Without future values, a covariate is held at the mean of its last five historical years. Scenario drivers then apply offsets, trends or event years.
* If `env_covariates.csv` contains **future years**, for example a MOM6-NWA12 downscaled SSP projection or a GLORYS-based climatology, they are used directly.
* `read_gridded_netcdf()` and `aggregate_gridded_covariate()` turn GLORYS / MOM6 / satellite NetCDF fields into area-weighted regional annual series for any bounding box or polygon. This makes it straightforward to apply the model to a different island or region.

## 8. Biomass dynamics, catch and MMSY

### Reference point: why MMSY = x · M · K

The SEDAR 103 presentation wrote MMSY = available biomass × P/B. That expression is the group's **total annual production**, which also supplies natural mortality: the prey for predators and the energy that sustains the stock. Harvesting all of it would collapse the group and starve its predators, so it is an **upper bound**, not an MSY.

The package uses the defensible and widely applied data-limited form:

$$
\text{MMSY}_i(t) = x_i\,M_i\,K_i(t)
$$

* With $x = 0.5$ this is **Gulland's (1971) estimator**, MSY = 0.5 · M · B₀. Because $K = P/M$, MMSY = $x \cdot P$: a fixed fraction of the production the energy budget supplies to the group.
* **Production shape.** The dynamics use **Pella–Tomlinson** surplus production (Pella & Tomlinson 1969; Fletcher 1978) with MSY = $x\,M\,K$ at $B_{MSY} = \beta K$. The shape $n$ solves $n^{1/(1-n)} = \beta$ (`bmsy_k`). St. Croix uses each group's per-recruit $B_{MSY}/B_0$ ≈ 0.32–0.38, so the dynamics and the reference point share one theory. With $\beta = 0.5$ ($n = 2$) this is exactly Schaefer, $r = 4xM$, MSY = $rK/4$. The equation is integrated exactly: $y = (B/K)^{1-n}$ obeys $dy/dt = -\rho\,(y-1)$ with $\rho = n^{n/(n-1)}\,x\,M$. Status is reported as $B/(\beta K)$.
* **Life-history priors for x.** Age-structured theory shows MSY/(M·B₀) depends on growth, maturity, gear selectivity and stock–recruit steepness. Gulland's 0.5 is an upper bound (Beddington & Cooke 1983; Clark 1991; Zhou et al. 2012). `life_history_xmsy()` derives a prior for each group with a Beverton–Holt per-recruit model:
  - von Bertalanffy growth and length–weight from FishBase;
  - maturity at $L_m$ (Froese & Binohlan 2000);
  - trap retention $L_c$;
  - steepness $h \sim N(0.75, 0.1)$ (Myers et al. 1999; Shertzer & Conn 2012).

  It returns x, F_MSY/M and B_MSY/B₀ (`data-raw/build_life_history_priors.R` for St. Croix: x = 0.17–0.31, B_MSY/B₀ ≈ 0.33–0.36). Queen conch keeps 0.4 to reflect depensation.
* The original SEDAR103-DW-20 form (x = 0.5 on P/B) is kept as the structural-ensemble variant `gulland_PB`.
* `production_p50_t` in the outputs reports total production (the slide formula, x = 1) for comparison.

Two MMSY variants are reported:

* **MMSY (prevailing):** uses $K_i(t)$, including the trophic feedback of current fishing on prey pools.
* **MMSY0 (ecosystem potential):** uses $K_{0,i}(t)$, with no fishing feedback.

### Dynamics

$$
\frac{dB_i}{dt} = (1-\phi_i)\,g_i(B_i) + \phi_i\,2x_iM_i\big(K_i(t)\,s_i(t) - B_i\big) - C_i(t) - G_i(t) + I_i(t)
$$

where $g_i$ is the Pella–Tomlinson production function above (logistic when `bmsy_k` = 0.5).

* **Open recruitment.** $\phi_i$ (`open_recruitment`) is the fraction of production that comes from externally supplied recruits, for example *Panulirus argus* larvae from upstream sources (Kough, Paris & Butler 2013). That part does not depend on local spawning biomass. It relaxes toward $K\,s(t)$ at rate $2xM$, so its yield at $F = 2xM$ equals the same $xMK$ cap and the reference point is unchanged. Local depletion cannot collapse it. $s_i(t)$ is a supply index (`recruit_covariate`, relative to its historical mean) multiplied by the nursery index.
* **Ghost fishing.** $G_i(t)$ is unaccounted mortality from derelict traps (`ghost_gears`). Lost traps keep fishing at `ghost_efficiency` of an active trap and decay with `ghost_half_life` (Renchen et al. 2010; Clark et al. 2012; Arthur et al. 2014). Two sources of lost traps are included:
  * **Chronic loss** (`ghost_background_loss`, about 10% of traps per year; Clark et al. 2012) gives a steady-state derelict-to-active ratio $d = \lambda\,t_{1/2}/\ln 2$.
  * **Storm losses** from the disturbance templates add to it.

  Ghost removals are reported separately (`ghost`) and are not counted as landings.

* The equation is solved with the **exact logistic solution** in each sub-step. This keeps it stable even for P/B = 30 (zooplankton). Catch and immigration are removed or added after each sub-step.
* Annual lognormal process error $\varepsilon_{i,t}\sim N(-\sigma^2/2,\sigma^2)$ is applied (`process_sd`).
* $I_i(t)$ is immigration (`immigration_t`, t yr⁻¹). It is the hook for the **Greater Caribbean connectivity model** (slide 12), where larval or adult supply from outside the platform can be specified, assumed, or tested as a sensitivity.
* Initial biomass is `init_depletion` × $K_{0,i}$ in the first year.

### Catch

* **Historical period:** catch = reported landings (lb → t) × `catch_expansion`. Catch is capped at 90% of the **fishable** biomass within any sub-step. The fishable fraction is $f_i = 1 - \sum_h u_{ih}\,\text{closed}_h$, where `closed_fraction` is the no-take (MPA) share of habitat h. The expansion factor addresses under-reporting (≈2.0 in Puerto Rico; unquantified in the USVI) and carries a CV.
* **Projection period:** effort-driven. $F_{gi}(t) = q_{gi}E_g(t)$, where catchability $q_{gi}$ is estimated draw by draw from the last `q_years` of history. Each sub-step catch is $B(1-e^{-F/S})$. Groups with `harvest_allowed = FALSE` (e.g., Parrotfish 1 in St. Croix) have $F = 0$.
* Effort $E_g(t)$ is the reference effort (mean of the last `q_years`) times scenario and disturbance multipliers. For example, trap losses after a major hurricane recover with a half-life.
* **No-take areas.** Projected F is scaled by $f_i(t)/f_i^{ref}$, so expanding a no-take area (`closed_fraction` scenario driver) reduces exploitation of the groups that use it.
* **Effort dynamics.** With `effort_response` = ε > 0 (setting or scenario driver), effort follows profitability. Each year it is multiplied by $(\text{CPUE}_{t-1}/\text{CPUE}_{ref})^{ε}$, bounded at 0.2–3× (open-access dynamics; Smith 1969).
* **Management procedures.** `simulate_ttm(..., mp = )` accepts a closed-loop management procedure that resets effort each year from noisy assessment outputs or catch rates (Section 20).

## 9. Outputs for management and for the SPF

| Output | Definition |
|---|---|
| Group MMSY quantiles | p05–p95 of draw-level MMSY averaged over reference years (t and lb) |
| OFL proxy | median MMSY: 50% probability of overfishing, as in SEDAR103-DW-20 |
| Target | MMSY at percentile P* (`pstar`; e.g., 0.40 gives a 40% chance of overfishing) |
| P(overfishing) | share of draws where catch > MMSY |
| Energy utilization (EU) | catch / production (= x × catch/MMSY) |
| U | catch / MMSY |
| B/BMSY | 2B/K |
| Status | Below target / Caution (catch > target) / Overfishing (catch > OFL proxy) / Prohibited |
| Guild MMSY | draw-level sums over member groups, so covariance among groups is preserved |
| Gear MMSY | catch-share rule $MMSY_{gi} = MMSY_i\,C_{gi}/\sum_g C_{gi}$ (default), or the slide-11 rule $MMSY_i\,W_{gi}\,E_g/\sum E$ with $W_{gi}=C_{gi}/\sum_i C_{gi}$ |
| Effort ceiling and choke group | $m_g = \min_i MMSY_{gi}/C_{gi}$; the minimizing group is the choke group, reported with its probability across draws |
| Ecosystem indicators | total catch, mean TL of catch, pelagic:demersal ratio, total FMP MMSY, catch/MMSY, FMP biomass, benthic and pelagic P1 |

Note on the catch-share rule: $MMSY_{gi}/C_{gi} = MMSY_i/C_i$ for every gear that catches group *i*. The choke group of a gear is therefore the most-utilized group *that gear catches*. Gears that share a choke group must be managed jointly, which is useful information for the SPF and the harvest control rule. The slide rule distributes MMSY by gear composition and effort instead, so its choke groups can differ by gear.

**SPF hand-off.** `write_spf_inputs()` writes these tables (see the User Manual, Section 6). They contain everything an SPF analyst needs: the frontier (capacity) of each gear can be compared directly with `MMSY_gear_*`, and capacity-based effort limits with `effort_ceiling_*`. `spf_mmsy_draws.csv` gives the full MMSY distribution, so the SPF/MSE can propagate the TTM uncertainty.

## 10. Uncertainty: Monte Carlo and SIR

Every uncertain input is sampled independently in each draw:

* habitat areas, NPP, food-web fraction and TTE
* C:WW
* P/B, x_msy, partition values, B0_fixed and initial depletion
* diet (Dirichlet with concentration `diet_concentration`)
* catch expansion, bleaching mortality, environmental sensitivities, coral growth, habitat preferences, immigration
* process error
* **observation error in data inputs**: covariate values (`sd`, e.g., the spatial or ensemble SD returned by the APIs), landings and effort records (`cv`), and disturbance intensities (`intensity_cv`). When these are present, the annual drivers are rebuilt for each draw. The complete list is in [Data Acquisition & Uncertainty](data_acquisition.md), Section 4.

The sampling distributions are lognormal for positive quantities and beta for fractions, each defined by its mean and CV. All scenarios use the same parameter draw, so scenario contrasts are paired.

**Sampling-Importance-Resampling (SIR)** optionally conditions the draws on data:

* **Relative indices** (`indices.csv`): e.g., SEDAR 57 lobster trap CPUE, SEDAR 80 queen triggerfish trap CPUE, NCRMP densities. These use a lognormal likelihood with an analytical catchability, so they inform **trends** (depletion, productivity), not absolute scale.
* **Catch feasibility** (stock reduction analysis; Kimura & Tagart 1982, Walters et al. 2006): draws in which the simulated stock could not support the observed (expanded) landings in some year are down-weighted. This places a lower bound on the productivity of every harvested group, assessed or not.
* **Assessment benchmarks** (`benchmarks.csv`, `use_in_sir = TRUE`): e.g., the SEDAR 57 / SEDAR 80 OFLs. These anchor the **absolute scale** of MMSY for assessed groups. Because all groups share the energy pools, the anchoring also constrains the scale of the unassessed groups through the shared primary production, TTE and food-web fraction draws. ACLs include management buffers and should not be used in SIR; they are compared in `benchmark_comparison.csv`.

Draws are weighted by $\exp(-\text{NLL})$ and resampled. The effective sample size (ESS) is reported. A low ESS means the priors conflict with the data or more draws are needed. `prior_results()` returns the unconditioned results for comparison.

## 11. Scenario and perturbation library

| Driver (`scenarios.csv`) | Effect |
|---|---|
| `effort_mult` | multiply effort of gear(s) (`target`, `;`-separated) |
| `cov_offset` / `cov_trend` / `cov_set` | add a constant, add a linear trend, or set event values of any covariate (e.g., SST warming, DHW bleaching years) |
| `cov_from` | replace a covariate by another series, e.g. `sst=sst_ssp585` from a CMIP6 / MOM6 projection |
| `npp_mult` / `npp_trend` | scale habitat NPP once, or with a compound annual trend |
| `area_trend` / `area_mult` | habitat area change (loss of seagrass, reef, mangrove) |
| `tte_mult` | change transfer efficiency |
| `coral_disease` | add annual coral mortality (e.g., SCTLD) |
| `immigration_mult` | scale external supply |
| `disturbance` | apply a template event from `disturbances.csv` (hurricane, oil spill, …) in `start_year` with intensity `value` |

Disturbance events can simultaneously:

* reduce primary production in listed habitats;
* reduce habitat quality or coral cover;
* reduce effort of listed gears (trap and vessel loss);
* reduce production of listed groups.

Each effect recovers exponentially with its own half-life.

## 12. Key assumptions and caveats

The full list, with values, sources, assumption type and how each is tested, is the [Assumptions Register](assumptions.md). The most consequential are:

* **Static diet.** Diet composition is static apart from Monte Carlo uncertainty; there is no prey switching. Fishing feedback acts through pools, not explicit predator–prey functional responses.
* **Lindeman spine.** Production at TL *k*+1 equals TTE × production at TL *k*, so all pool production is attributed to modeled consumers. Unmodeled consumers and detritus losses are absorbed by TTE and the food-web fraction. Include non-fishery competitor groups so that FMP groups do not receive energy that, in reality, supports other taxa.
* **Production shape.** Surplus production is Pella–Tomlinson, with $B_{MSY}/K$ set to each group's per-recruit $B_{MSY}/B_0$ (Schaefer, $K/2$, when `bmsy_k` = 0.5). The open-recruitment component is linear and keeps the same MMSY definition, which is conservative for open populations.
* **OFL as a calibration target.** Assessment OFLs are treated as observations of MMSY with CV 0.4. An OFL is $F_{MSY}$ × current biomass, so it equals MSY only near $B_{MSY}$ (Assumptions register C1). Leave-one-benchmark-out validation tests the consequence.
* **Catch-driven history.** Historical dynamics are driven by landings, so landings quality matters. Use `catch_expansion` and its CV, or subsets of validated trip tickets (e.g., MER Consultants Sampling and Catch Validation Project).
* **MMSY is conditional.** MMSY reflects prevailing conditions and must be re-estimated when indicators trigger (Section 9; SEDAR103-DW-20 Section C).
* **Uncertain inputs.** Many inputs are literature-based. Run the sensitivity of key results to TTE, food-web fraction and partition weights, which are the main levers on absolute scale. The SIR benchmarks and the `data_status` flag on every input help identify weak points.

## 13. Evidence-based priors

| Input | Source | How it is derived |
|---|---|---|
| FMP fish partition priors (biomass density) | Opitz (1996) Table 3.6: species biomass in Virgin Islands reef environments | Summed to St. Croix FMP complexes, then multiplied by Opitz's own mass-balance adjustment for reef fish (balanced / raw biomass of fish groups 1–27 = 123.9 / 519.3 = 0.239). Large Scaridae as a check: 18.0 g m⁻² derived vs 18.98 balanced |
| Lobster and non-fishery biomass, P/B | Opitz (1996) Table 3.22 (balanced 50-box Caribbean reef model) | Matched groups; biomass-weighted P/B |
| P/B of FMP fish | Opitz (1996) balanced P/B and FishBase empirical M (Then et al. 2015 growth estimator; L∞ from Lmax via Froese & Binohlan 2000) | Geometric mean; CV = max(0.25, \|log ratio\| / 2), so disagreement between sources widens the prior |
| Diet / trophic level | FishBase trophic levels | Diet vectors shifted up the trophic-level pools until the implied TL equals the FishBase TL |
| Thermal responses | AquaMaps temperature preferences (via FishBase) | Gaussian performance curve: t_opt = preferred mean, width = (max − min) / 2; no SST response for deepwater snappers |
| Transfer efficiency | SEDAR103-DW-20 default 15%; Opitz (1996) Table 3.27 TL II–IV efficiencies of 10–17% | Prior 0.15 ± 20% |
| Deeper-shelf habitat | ETOPO 2022 bathymetry (30–100 m) | Area 41.4 km² (not the 388 km² NCCOS "Unknown" class) |
| Deeper-shelf NPP | Light at depth from OC-CCI Kd490 (Morel et al. 2007 KdPAR) and satellite PAR, saturating P–I curve | Colonized-pavement NPP scaled by tanh(I(z)/Ik) / tanh(I(10 m)/Ik), Monte Carlo over Kd490, PAR and Ik |

Where a prior conflicts with the data, the SIR exposes it (`landings_shortfall`, `prior_reference_points()$P_infeasible_landings`, `posterior_shrinkage.csv`).

## 14. Sampling the posterior: adaptive importance sampling

With many harvested groups, each draw must support every group's landings simultaneously, so the feasible region of plain prior sampling is small and SIR weights degenerate. `run_ttm()` therefore supports `sir_adaptive_rounds`:

1. Round 0 draws from the prior.
2. After each round, a proposal for every group's log partition value is fitted to that group's **own** likelihood components (its catch feasibility and its own benchmarks). This is a factorized proposal, with the spread inflated 1.5×.
3. Later rounds draw partition values from the proposal. All other inputs are still drawn from their priors.
4. Final importance weights use the joint likelihood times prior / mixture-of-proposals, the deterministic-mixture "balance heuristic" (Owen & Zhou 2000). The posterior is therefore unchanged in expectation; only the sampling efficiency improves.

Only groups whose prior conflicts with their own data are adapted: a group keeps its prior when its own-likelihood weights already have an effective sample size above half the draws. This avoids needless importance-correction variance.

**Fishery focus.** `sir_feasibility_groups` limits which groups constrain the calibration. In the St. Croix case it is `trap`: the 13 trap-relevant FMP groups (at least 25% of landings from fish or lobster traps) plus the assessed stocks. Queen conch, deepwater snappers, pelagics, yellowtail snapper and jacks remain in the food web, consuming energy, and their landings are still removed. They do not constrain the posterior, so sampling effort concentrates on the trap fishery. Dive and hook-and-line removals of trap species (e.g., about 55% of lobster) are retained, because they determine total mortality.

**Parallel sampling.** Stage-1 draws run in parallel (`cores`) using independent L'Ecuyer-CMRG random-number streams per draw. Results are identical for any number of cores (unit-tested).

`sir_diagnostics()` reports:

* effective sample size;
* number of unique resampled draws;
* the largest weight and the share of the top 10 weights;
* a Pareto-k tail estimate (values > 0.7 mean the importance sampling is unreliable);
* posterior shrinkage, 1 − var_post / var_prior, for every sampled input. Shrinkage shows which inputs the data inform and which remain prior-dominated, which addresses identifiability directly.

## 15. Out-of-sample validation

Calibration and validation data are separated explicitly:

* `indices.csv: role` is `fit` (used in SIR) or `validate` (held out);
* `settings: holdout_from_year` holds out all index observations from that year onward (forecast skill);
* benchmarks with `use_in_sir = FALSE` are held out.

| Function | Validation design | Metrics |
|---|---|---|
| `validate_out_of_sample()` | Held-out index observations. If the series has fitting years, catchability is estimated from them and future values are forecast; whole held-out series are compared as scale-free anomalies | 50% and 90% predictive coverage, RMSE (log), MASE vs a naive forecast (< 1 beats naive), anomaly correlation |
| `loo_benchmarks()` | Leave-one-assessment-benchmark-out (e.g., predict the SEDAR 80 OFL from a calibration that excludes it) | PIT, inside-90% flag, predicted/benchmark ratio |
| `retrospective_peel()` | Truncate fitting indices 3, 6, 9 years | relative change in posterior median MMSY (Mohn's-rho analogue) |

Because the Stage-1 draws are retained, every design re-weights the same simulations, so the validation adds no new simulation cost.

## 16. Robustness analyses

| Function | Question answered |
|---|---|
| `sensitivity_analysis()` | Which inputs drive MMSY? Standardized rank regression coefficients (SRRC) with the rank-model R², plus partial rank correlation coefficients (PRCC), on all prior draws |
| `self_test()` | Does the estimator recover the truth? Pseudo-data are simulated from a known draw and re-estimated, giving bias (median relative error), MARE and 50/90% interval coverage |
| `run_structural_ensemble()` | How much do structural choices matter? Variants: the original Gulland-on-P/B form; x −25%; closed population (no external recruitment); no nursery dependence; no herbivory feedback; no ghost fishing; no lionfish / Sargassum effects; TTE 10/20%; no thermal effects; food-web fraction ×0.67/×1.5. Reported per variant and pooled with equal weight |
| `ecosystem_overfishing_indices()` | Whole-system fishing intensity independent of species partitioning: Ryther index (catch / km²) and Fogarty ratio (catch / primary production), with Link & Watson (2019) thresholds |

## 17. Observation models for indices

Each index row can declare how it relates to biomass:

$$
I_t = q\,(1+c)^{\,t-t_0}\,B_t^{\beta}\,e^{\varepsilon_t}
$$

| Column | Meaning | Default |
|---|---|---|
| `hyperstability` | β < 1: CPUE declines more slowly than abundance (aggregation, trap saturation; Harley et al. 2001) | 1 |
| `creep` | annual catchability increase c for fishery-dependent CPUE (technology, skill; Palomares & Pauly 2019) | `settings$effort_creep` (St. Croix 0.02) for indices with `fishery_dependent = TRUE` (default: names containing "CPUE"), else 0 |
| `type` | `relative` (q profiled out analytically) or `absolute` (index in tonnes, e.g., NCRMP density × habitat area) | relative |
| `q_prior`, `q_cv` | for absolute indices: lognormal prior on detectability or availability; the conditional MAP of log q is used together with its prior penalty | 1, 0.5 |

Absolute survey indices are the most direct constraint on the scale of unassessed groups, because density × area pins biomass up to detectability.

## 18. Pareto-smoothed importance sampling

After the adaptive importance-sampling rounds, the largest raw weights are replaced by expected order statistics of a generalized Pareto distribution fitted to the weight tail (`psis_smooth()`; Vehtari et al. 2024). This bounds the influence of any single draw. The shape estimate k is reported as a reliability diagnostic: below 0.5 is good, 0.5–0.7 acceptable, above 0.7 means more draws are needed. Raw and smoothed ESS are both reported (`sir_diagnostics()`).

## 19. Independent comparator and value of information

* **Catch-only comparator.** `catch_msy()` and `method_comparison()` run a CMSY-style stock-reduction analysis (Martell & Froese 2013; Froese et al. 2017): Schaefer dynamics with only landings and a resilience prior from M. It shares no energy-budget assumption with the TTM, so agreement between the two is independent corroboration and disagreement points to which assumptions drive a result.
* **Value of information.** `value_of_information()` estimates each input's first-order variance share of log MMSY (Sobol' main effect, by quantile binning, with a bias correction for binning noise). That share is the expected variance reduction from learning the input exactly. Inputs are mapped to the data stream that would inform them (biomass surveys, catch monitoring, production studies, life history, connectivity, and so on), so data collection can be ranked by how much uncertainty it would remove.

## 20. Closed-loop management strategy evaluation

`run_mse()` treats SIR-conditioned draws as operating models and steps each management procedure through the projection years. The procedure sees only noisy information; the operating model keeps every process (open recruitment, nursery, herbivory, ghost fishing, environment).

| Procedure | Rule |
|---|---|
| `mp_status_quo()` | effort fixed at the reference level |
| `mp_fixed(0.75, traps)` | 25% trap reduction |
| `mp_spf_ceiling()` | every 3 years, an assessment estimates each focus group's MMSY with CV 0.3. Each gear's effort is set so no focus group's catch by that gear exceeds its share of the P* target, with changes capped at ±25% per decision. This is the SPF effort-ceiling rule. |
| `mp_spf_ceiling(bias = 1.5)` | the same rule with a 50% optimistic assessment (robustness test) |
| `mp_cpue_hcr()` | empirical rule: effort follows each gear's observed catch rate relative to the reference years |

Performance over the final 10 projection years:
* probability that any focus group falls below 0.5 B_MSY;
* median minimum B/B_MSY;
* median focus-group catch;
* catch variability (average annual variation);
* effort relative to the reference years.

User-defined procedures are functions of `(year, state)` that return effort multipliers by gear.
