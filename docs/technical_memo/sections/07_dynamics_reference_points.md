# Population dynamics and reference points

## The sustainable-yield cap

The total annual production of a group also supplies natural mortality: it is the prey consumed by predators and the energy that sustains the stock. The total ("x = 1") is therefore an upper bound, not a sustainable yield. CaribTTM uses the data-limited form

$$
\text{MMSY}_i(t) = x_i\,M_i\,K_i(t)
$$

which is Gulland's (1971) estimator when $x = 0.5$. Because $K = P/M$, it equals $x \cdot P$: a fixed fraction of the production the energy budget supplies.

### Life-history priors for x

Age-structured theory shows that MSY/(M·B₀) depends on growth, maturity, gear selectivity and stock–recruitment steepness. For most teleosts it lies well below 0.5 (Beddington & Cooke 1983; Clark 1991; Zhou et al. 2012).

The function `per_recruit_xmsy()` computes x for a set of traits with a Beverton–Holt per-recruit equilibrium:
* von Bertalanffy length at mid-age;
* the length–weight relationship;
* logistic maturity at $L_m$ (Froese & Binohlan 2000);
* logistic gear retention at $L_c$;
* recruitment from steepness $h$ (Mace & Doonan 1988);
* MSY found by grid search followed by local optimization.

The function `life_history_xmsy()` draws, for each group:
* species in proportion to their landings weight;
* M and K with CV 0.2;
* steepness from N(0.75, 0.1).

That value is slightly below the meta-analytic centre for demersal fishes (Shertzer & Conn 2012), a precautionary choice. Trap retention is about 15 cm TL (capped at 0.6 L∞, CV 0.3), an assumption for 1.5–2 inch mesh. Hook-and-line groups are retained at maturity.

For spiny lobster, local steepness is effectively 1 because most recruits are imported. Its x is the open-recruitment-weighted blend of the open (h → 1) and local (h = 0.75) values.

The resulting priors are x = 0.17–0.31 for fishes and 0.32 for lobster, with F_MSY/M = 0.5–1.4 and B_MSY/B₀ = 0.29–0.38. The table below gives them by group.

{{AUTO:in_lh}}

### Production shape

Schaefer (logistic) dynamics imply B_MSY = K/2, which is inconsistent with the per-recruit B_MSY/B₀ of 0.29–0.38 that sets x. CaribTTM therefore uses **Pella–Tomlinson** production (Pella & Tomlinson 1969; Fletcher 1978). The shape $n$ solves $n^{1/(1-n)} = \beta_i$, where $\beta_i$ is the group's per-recruit B_MSY/B₀ (`bmsy_k`), and the production is scaled so that MSY = $x_i M_i K_i$.

The equation is integrated exactly. The transformed state $y = (B/K)^{1-n}$ obeys the linear equation $dy/dt = -\rho\,(y-1)$, with $\rho = n^{n/(n-1)}\,x\,M$. With $\beta$ = 0.5 ($n$ = 2) this reduces to the logistic, with $\rho = r = 4xM$. Stock status is reported as $B/(\beta K)$.

## Biomass dynamics

$$
\frac{dB_i}{dt} = (1-\phi_i)\,g_i(B_i) \;+\; \phi_i\,2x_iM_i\,\big(K_i\,s_i(t) - B_i\big) \;-\; C_i(t) \;-\; G_i(t) \;+\; I_i(t)
$$

where:
* $g_i$ is the Pella–Tomlinson production;
* $\phi_i$ is the **open-recruitment fraction**;
* $s_i(t)$ is a recruitment-supply index (a connectivity covariate if available) multiplied by the nursery index;
* $C_i$ is catch, $G_i$ ghost-fishing mortality and $I_i$ immigration.

Integration uses 12 monthly sub-steps: exact production, exact linear relaxation of the open component, then catch removal. Annual lognormal process error (σ = 0.05–0.15) is applied at the end of each year.

**Open recruitment.** *Panulirus argus* has a larval phase of 6–9 months, and most settlers on small islands originate upstream (Kough, Paris & Butler 2013). The spiny lobster open-recruitment fraction is 0.8 (CV 0.25).

The open component relaxes towards $K s$ at rate $2xM$. Its yield at F = 2xM therefore equals the same $xMK$ cap, which preserves a single reference-point definition. This understates the true MSY of a fully open population, so it is conservative. Local depletion cannot collapse the imported supply. The `closed_population` ensemble variant sets φ = 0.

**Initial conditions.** Biomass in 1983 is initial depletion × K₀. Depletion is assumed to be 0.4–0.8 by group (CV 0.2–0.3), which reflects a fishery already well developed by the early 1980s.

## Catch, fishable biomass and effort

**Historical period.** Catch equals reported landings by year, gear and group (lb → t), multiplied by the expansion factor (1.5, CV 0.25) and by a lognormal observation error for each record (CV 0.2).

Within each sub-step, catch is capped at 90% of the **fishable** biomass. The fishable fraction is $f_i = 1 - \sum_h u_{ih}\,\text{closed}_h$, where:
* $u_{ih}$ is habitat use;
* closed$_h$ is the no-take share of habitat $h$.

St. Croix uses placeholder shares approximating BIRNM and the EEMP no-take zones (Appendix C). When a stock cannot supply its landings, the shortfall is recorded. The largest annual shortfall enters the calibration as a catch-feasibility likelihood (Section 9).

**Projection period.** Catch is effort-driven: $F_{gi}(t) = q_{gi}\,E_g(t)\,f_i(t)/f_i^{ref}$.
* Catchability $q_{gi}$ is estimated draw by draw from the last three historical years.
* Effort is the reference effort times scenario and disturbance multipliers.
* Changes in no-take area scale fishing mortality through $f_i(t)/f_i^{ref}$.
* With `effort_response` ε > 0, effort follows profitability (open-access dynamics; Smith 1969): it is multiplied each year by (CPUE$_{t-1}$/CPUE$_{ref}$)^ε, bounded to 0.2–3× the reference.
* A management procedure can reset effort each year (Section 9).

**Ghost fishing.** Derelict traps come from two sources:
* chronic loss of 10% of traps per year (Clark et al. 2012), which gives a steady-state derelict-to-active ratio d = λ·t½/ln 2;
* storm losses from the hurricane templates.

They fish at 20% of an active trap's efficiency (CV 0.5) and decay with a 1.5-year half-life. The resulting mortality, $G_i = e \cdot (\text{derelict}/\text{active}) \cdot F^{trap}_{i}(t-1)$, is unaccounted mortality. It is not part of landings and is reported separately.

## Reference points and management outputs

| Output | Definition |
|---|---|
| OFL proxy | median MMSY over 2022–2024 |
| P* target | 40th percentile of MMSY (SEDAR103-DW-20 example) |
| P(overfishing) | probability over draws that catch exceeds MMSY |
| B/B_MSY | $B/(\beta K)$; "Overfished" if below 0.5 (MSST proxy) |
| Status | Prohibited / Outside focus (uncalibrated) / Overfished / Overfishing / Caution (catch above the P* target) / Below target |
| Guild MMSY | sums of draw-level group MMSY, which preserves covariance |
| Gear MMSY | MMSY × the gear's share of the group's catch |
| Effort ceiling | $m_g = \min_i\, \text{MMSY}_{gi}/C_{gi}$ over SIR-calibrated groups caught by gear $g$; the minimizing group is the **choke group** |
| Ecosystem indices | Ryther index (catch per km²) and Fogarty ratio (catch / primary production, per mil), against Link & Watson (2019) thresholds |
