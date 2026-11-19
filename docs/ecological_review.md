# Ecological and Fisheries-Science Review: Gaps, Additions and Remaining Needs

This page reviews CaribTTM as an ecologist or stock-assessment scientist would. It asks what fisheries theory, expert opinion and stakeholder knowledge require of a model that sets multispecies yield caps for the St. Croix trap fishery, and records:

* what was **added** in response;
* what is **partly** handled;
* what remains **recommended**, and the data or process it needs.

Every added process can be switched off, and the switch-off is a variant in the structural ensemble ([Review Readiness](review_readiness.md)). Reviewers can therefore see each one's effect on MMSY directly.

## 1. Population-dynamics theory

| Gap | Why it matters | What was done | Status |
|---|---|---|---|
| **MMSY used P/B (≈ total mortality Z) in place of natural mortality M** | Gulland's MSY ≈ x·M·B₀ is defined on M. Ecopath P/B for an exploited 1980s system equals M + F, which overstates sustainable yield and understates unfished biomass. | Groups gain `m_nat`. K = P/M (unfished biomass that supports production P), MMSY = x·M·K and r = 4·x·M. P/B still sets each group's share of production. The default (`m_nat` blank) reproduces the old form. | **Added** |
| **Fixed x = 0.5** | Age-structured theory gives MSY/(M·B₀) ≈ 0.2–0.4 for most teleosts, depending on growth, maturity, gear selectivity and steepness (Beddington & Cooke 1983; Clark 1991; Zhou et al. 2012). x = 0.5 is an upper bound. | `per_recruit_xmsy()` and `life_history_xmsy()` derive a prior for x for each group. They use a Beverton–Holt per-recruit model with FishBase growth, maturity (Froese & Binohlan 2000), trap retention (≈ 15 cm; lobster 8.9 cm CL legal minimum) and steepness h ~ N(0.75, 0.1). St. Croix priors: x = 0.17–0.31, B_MSY/B₀ ≈ 0.33–0.36. The original form is kept as structural variant `gulland_PB`. | **Added** |
| **Spiny lobster treated as a closed population** | *Panulirus argus* has a 6–9-month larval phase. Most settlers on a small island come from upstream sources (Kough, Paris & Butler 2013), so local spawning stock does not set recruitment. A closed model overstates collapse risk and misjudges scale. | `open_recruitment` (φ) splits production into a local logistic part and an external-supply part, dB/dt = (1−φ)·rB(1−B/K) + φ·2xM·(K·s_t − B). The supply index s_t can come from a connectivity covariate. Lobster: φ = 0.8 ± 25%, and x blends open (h → 1) and local (h = 0.75) per-recruit values (0.32). | **Added**. The connectivity time series is still a data need. |
| Depensation and Allee effects (queen conch) | Conch reproduction fails below ~50–100 adults per ha (Stoner & Ray-Culp 2000). Recovery is therefore not logistic at low density. | Conch is outside the trap focus. Its lower x (0.4) is a crude precaution. | **Recommended**: an explicit Allee term if conch enters the focus |
| Size structure and growth overfishing | Biomass pools cannot represent trap selectivity, size limits or truncation of size structure. | Handled partly through the per-recruit x prior (selectivity and maturity). | **Partial** |

## 2. Habitat and ecosystem processes

| Gap | Why it matters | What was done | Status |
|---|---|---|---|
| **Nursery-habitat dependence** | Grunts, schoolmaster and gray snapper, rainbow parrotfish, lobster and conch rely on seagrass, mangrove or macroalgal nurseries. Adult biomass on reefs scales with nursery availability (Nagelkerken et al. 2002; Mumby et al. 2004; Adams et al. 2006). | `nursery_habitat` and `nursery_dependence` (ν) scale K by (nursery area × quality / baseline)^ν. ν is 0.2–0.5 for eight groups (expert-informed, flagged as placeholders). New scenario: `Seagrass_loss_Halophila` (invasive seagrass displacing native beds). | **Added**; ν magnitudes need elicitation or survey support |
| **Herbivory–coral feedback** | Parrotfish and surgeonfish grazing controls macroalgae and so coral recovery (Mumby et al. 2006, 2007). This is why Parrotfish 1 is prohibited and Parrotfish 2 has an ACL. A model that harvests herbivores without the feedback misses the main ecosystem trade-off. | Projected coral growth is multiplied by (grazing / reference)^γ, where grazing = herbivore B / unfished B. γ = 1 ± 50% (placeholder). New scenario: `Parrotfish_harvest_plus50`. | **Added**. It approximates alternative stable states but does not model bistability explicitly. |
| Habitat refuge and coral decline | Structural complexity sets carrying capacity for reef-associated species. | Already present (`refuge_dependence`, coral cover dynamics with bleaching, SCTLD and hurricanes). | Existing |
| **Lionfish invasion (from 2008)** | Lionfish prey on small reef fish and recruits and compete with native mesopredators (Albins & Hixon 2008; Green et al. 2012). | A lionfish covariate (0 before 2009, established by 2012) reduces K by up to 10% for six small-prey groups and 5% for groupers and shallow snappers (CV 1.0). | **Added**; magnitude is a placeholder (NCRMP lionfish densities would calibrate it) |
| **Pelagic Sargassum inundations (from 2011)** | Mass strandings degrade nearshore seagrass and mangrove nurseries (van Tussenbroek et al. 2017). Sargassum is also habitat for dolphinfish. Fishers report fouled and lost traps. | Sargassum index from the USF Alternative Floating Algae Index (NOAA AOML ERDDAP, monthly 7-day composites, 2016–2024; 2018 and 2021 record years). It is applied as a −3% K effect per index unit on nursery-dependent groups and +3% for pelagics. 2011–2015 come from relative belt magnitudes (Wang et al. 2019), flagged as placeholders. | **Added**. Trap-fouling effects on effort are recommended once fishers' reports are compiled. |
| Water clarity and land-based pollution | Turbidity (Kd490) limits seagrass and coral production. Sedimentation follows coastal development. | Kd490 is acquired (1998–2024) but not yet linked to habitat production. | **Recommended**: Kd-driven light index on seagrass and coral NPP (data already in hand) |
| Ocean acidification | Affects calcifiers (coral accretion, conch, lobster). | Not modelled. | **Recommended** (CMIP6 aragonite saturation) |
| Multi-stressor interaction | Warming, disease and fishing can interact synergistically. | Effects multiply (the multiplicative null model). | **Recommended**: sensitivity variant with an interaction term |
| Dynamic predation mortality (M2) | Predator release and depletion change prey M. | Fishing feedback acts bottom-up through energy pools only. | **Partial** |

## 3. Fishery and management processes

| Gap | Why it matters | What was done | Status |
|---|---|---|---|
| **No-take marine protected areas** | Buck Island Reef National Monument (≈ 76 km², no-take since 2001–03) and the no-take zones of East End Marine Park shelter part of each stock. Landings come only from the fishable fraction. | Habitats gain `closed_fraction`, the share of each group's habitat-weighted biomass that cannot be fished. Historical catch is capped by fishable biomass, and projected F scales with changes in closed area. New scenario: `MPA_expansion`. | **Added**; fractions are approximate and should be replaced by a GIS overlay of MPA polygons on the NCCOS map |
| **Ghost fishing by lost traps** | Hurricanes (Hugo, Marilyn, Maria) destroyed large numbers of traps, and derelict traps keep catching for months (Renchen et al. 2014). This is unaccounted mortality. | Traps lost in disturbances keep fishing at 20% of active-trap efficiency (CV 50%), decaying with a 1.5-year half-life. The removals are reported separately from landings. | **Added**; efficiency and half-life are placeholders |
| **CPUE is not proportional to abundance** | Trap CPUE is often hyperstable (aggregation, trap saturation). Fishery-dependent indices rise with catchability creep of about 2% per year (Harley et al. 2001; Palomares & Pauly 2019). | The index observation model is I = q·(1+creep)^t·B^β. β comes from the `hyperstability` column (default 1). Creep defaults to 2% per year for CPUE indices. | **Added** |
| **Surveys as absolute biomass** | Visual-census density × habitat area gives absolute biomass with detectability below 1. This is the strongest constraint on unassessed groups. | Indices with `type = absolute` use a lognormal detectability prior (`q_prior`, `q_cv`) in place of a free scaling. | **Added**; awaits NCRMP data (NCEI still unavailable) |
| **Effort dynamics** | Open-access effort tracks profitability (Smith 1969). Holding effort fixed understates risk. | Projected effort can follow catch rate with elasticity `effort_response`. New scenario: `Open_access_effort`. | **Added**; prices and costs are a recommended extension |
| Spawning-aggregation fisheries and seasonal closures | Red hind (Lang Bank) and mutton snapper aggregations are vulnerable through hyperstability. Seasonal closures exist. | Hyperstability is available in the index model. Closures can be represented as scenario effort or closed-area changes. Seasonal dynamics are not explicit. | **Partial** |
| Discards, undersized-lobster release mortality, recreational and subsistence take | These are unreported removals. | The catch expansion factor (1.5 ± 25%) covers them in aggregate. | **Partial** (data needs A3, A7) |
| **Management strategy evaluation** | Reference points matter only through the rules that use them. Reviewers expect a closed-loop test of the proposed harvest control rule. | `run_mse()` uses SIR-conditioned operating models to compare status quo, a 25% trap cut, the SPF effort-ceiling rule (unbiased, and with a +50% biased assessment) and a CPUE-based empirical rule. Assessments include observation error. | **Added** |

## 4. Statistical practice

| Gap | What was done |
|---|---|
| An independent model to triangulate against | `catch_msy()`: a CMSY-style catch-only surplus-production comparator (Martell & Froese 2013; Froese et al. 2017). It shares no energy-budget assumptions with the TTM. |
| Prioritizing data collection | `value_of_information()` estimates first-order variance shares (Sobol' main effects) of MMSY for every input, mapped to the data stream that would inform it. |
| Importance-weight stability | Pareto-smoothed importance sampling with the Pareto-k diagnostic (Vehtari et al. 2024). The raw ESS is reported alongside. |

## 5. Stakeholder and local knowledge

Calibration cannot fix these items. They need structured engagement:

1. **Fishers' local ecological knowledge (LEK).** St. Croix trap fishers can date and describe:
   - lionfish arrival and abundance;
   - Sargassum disruption of trap fishing;
   - trap losses and theft after storms;
   - gear switching from traps to diving;
   - the decline of particular species.
   
   Interview-based timelines would turn several placeholders into data (lionfish covariate, ghost-trap rates, Sargassum effort effects).
2. **Structured expert elicitation.** Many magnitudes are expert-informed placeholders: ν, γ, lionfish and Sargassum sensitivities, ghost efficiency, hurricane losses. They should be elicited with a documented protocol, such as the Sheffield Elicitation Framework (SHELF), from the Caribbean SEDAR panels, DPNR and NCRMP scientists. Each elicited distribution goes straight into the existing CV columns.
3. **Market and cultural factors.** Ciguatera risk, species preferences and prices shape what is landed (and so what the landings say about abundance). They should be documented when interpreting landings by gear and species.
4. **Management feedback.** The MSE procedures should be co-designed with the Caribbean Fishery Management Council's District Advisory Panel. That ensures the rules tested are ones that could be adopted.

## 6. How the additions change the St. Croix results

See [Case Study §4](case_study_stcroix.md) for the numbers. In brief:
- Theory-based x and M lower the energy-implied MMSY of unassessed groups, and SIR raises their energy shares only as far as landings and benchmarks require.
- Open recruitment makes lobster dynamics robust to local depletion.
- The structural ensemble reports how far each added process moves MMSY.
