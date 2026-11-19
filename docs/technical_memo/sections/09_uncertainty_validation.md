# Uncertainty, calibration, validation and robustness

## Monte Carlo propagation of input and observation uncertainty

Each Monte Carlo draw samples one complete parameter set. The sampled quantities are:

* **Habitats.** Area (lognormal, by CV), NPP (lognormal), food-web fraction and TTE (beta), and the carbon-to-wet-weight conversion.
* **Groups.** P/B, natural mortality, x, partition values, initial depletion and immigration (lognormal or beta), and open-recruitment fraction (beta).
* **Diets.** A Dirichlet draw (concentration 40).
* **Habitat preferences.** Each element is perturbed with a lognormal error (CV 0.2).
* **Environmental responses.** Sensitivities are sampled by their CVs: lionfish, *Sargassum* and the other coefficients.
* **Coral.** Bleaching mortality per DHW, coral growth and herbivory elasticity.
* **Ghost fishing.** Ghost-trap efficiency.
* **Observation errors.** Each covariate value is perturbed by its acquired standard deviation (spatial SD, between-model SD or between-algorithm spread). Each landings record and each effort record gets its own lognormal error (CV 0.2 and 0.1). The catch expansion factor is sampled (CV 0.25), and so is each hurricane's intensity (CV 0.3), plus a common multiplier on all disturbance-loss magnitudes (CV 0.5).
* **Process error.** A seed for annual process deviations.

Drivers are rebuilt for every draw, so the observation errors propagate. The draws use reproducible per-draw random-number streams (L'Ecuyer-CMRG), so results do not depend on the number of processor cores.

## Calibration by importance sampling

### Likelihood components

Draws are weighted by the likelihood of the data that inform the calibration.

1. **Assessment benchmarks.** The model's 2022–2024 MMSY for spiny lobster and for queen triggerfish is compared with each assessment OFL under lognormal error with CV 0.4. An OFL is the catch at F_MSY given current biomass. It equals MSY only near B_MSY, so treating it as an observation of MMSY is an explicit assumption (Appendix B, C1). The CV absorbs the mismatch, and the leave-one-benchmark-out test quantifies its consequences.
2. **Catch feasibility.** This is stock-reduction-analysis logic (Kimura & Tagart 1982; Walters et al. 2006; Martell & Froese 2013). For each focus group, the largest annual shortfall between expanded landings and the catch the simulated stock could supply is penalized as 0.5·(log(1 − shortfall)/0.25)². This bounds productivity from below: a draw whose stock collapses under the observed landings is implausible.
3. **Indices** (when real series are available). These are fitted with the observation model $I_t = q\,(1+c)^{t-t_0}\,B_t^{\beta}$:
   * catchability creep $c$ is 2% per year for fishery-dependent CPUE (Palomares & Pauly 2019);
   * $\beta$ allows hyperstability (Harley et al. 2001);
   * $q$ is profiled analytically for relative indices;
   * absolute survey indices (density × area) use a lognormal detectability prior on $q$.

In the St. Croix configuration, all indices are placeholders and are therefore **held out**. Only the two OFLs and catch feasibility condition the posterior. Calibration is restricted to the trap fishery: the 13 trap groups and the two assessed stocks.

### Adaptive importance sampling

Plain sampling-importance-resampling (SIR) from wide priors fails when many groups must be feasible at once. A draw must satisfy all 15 focus groups simultaneously, so the joint acceptance probability is the product of the per-group probabilities. CaribTTM therefore uses **adaptive factorized importance sampling**:
1. The first round samples the prior.
2. After each round, every group whose own likelihood components conflict with its prior gets a new lognormal proposal for its partition value. The new proposal is fitted to that group's importance-weighted posterior, with the variance inflated by 1.5.
3. Each group's adaptation uses its own **marginal** prior-to-proposal ratio, so noise from other groups' corrections does not enter.
4. The final weights use the joint likelihood and the exact deterministic-mixture correction for all proposals used (Owen & Zhou 2000). The posterior is therefore the same as plain SIR; only the efficiency differs.

St. Croix uses four rounds of 12,000 draws in the main run.

### Weight stabilization and diagnostics

The final log weights go through **Pareto-smoothed importance sampling** (Vehtari et al. 2024). A generalized Pareto distribution is fitted to the upper tail of the weights (Zhang & Stephens 2009), and the largest weights are replaced by its expected order statistics.

The fitted shape $\hat k$ is the reliability diagnostic:
* below 0.5: good;
* 0.5–0.7: acceptable;
* 0.7 or above: unreliable. Smoothing is then not trusted, and the weights are instead **truncated** at mean(w)·√S (Ionides 2008). The method used is reported.

The report also gives:
* effective sample size, before and after stabilization;
* the number of unique resampled draws;
* the largest weight;
* the top-10 weight share;
* posterior shrinkage of every sampled input, against prior moments corrected for the adaptive proposal.

### Three-stage execution

1. **Stage 1.** Simulates the Baseline history for every draw, in parallel, with a memory-light store: biomass, reference-year means, parameter summaries and importance corrections.
2. **Stage 2.** Computes the weights and resamples.
3. **Stage 3.** Simulates every scenario for the retained draws only. Parameters are regenerated from their stored random-number streams, in parallel.

## Validation

* **Held-out indices.** Indices with role = validate are predicted from the posterior fitted without them. Coverage of the 50% and 90% predictive intervals, log RMSE, MASE against a naive forecast and anomaly correlation are reported.
* **Leave-one-benchmark-out.** Each OFL is dropped in turn. The posterior is refitted without it, and the held-out OFL is located in its predictive distribution (PIT value, inside the 90% interval or not).
* **Retrospective peel.** Refits with the final 1–n years of data removed measure how far estimates move as data accumulate (`retrospective_peel()`).
* **Simulation self-test.**
  1. A "true" state is drawn from the prior, using the importance ratio to undo the adaptive proposal.
  2. Pseudo-data are simulated with the same observation errors: indices, and benchmarks with CV 0.4.
  3. The SIR is refitted with the true draw excluded.
  4. Over 100 replicates, the median relative error (bias) and the coverage of the 50% and 90% intervals are reported for each focus group.

## Robustness and decision support

* **Global sensitivity analysis.** A rank regression of MMSY on every sampled input over the prior draws (resampled to the prior), giving standardized rank regression coefficients, partial rank correlations and the rank-model R².
* **Structural ensemble.** Fourteen variants of the model structure, each calibrated separately, with group MMSY pooled at equal weight:
  - `base`;
  - `gulland_PB`: the original x = 0.5 on P/B with Schaefer dynamics;
  - `schaefer_shape`;
  - `x_msy_low` (−25%);
  - `closed_population`;
  - `no_nursery`;
  - `no_herbivory_feedback`;
  - `no_ghost_fishing`;
  - `no_lionfish_sargassum`;
  - `tte_0.10` and `tte_0.20`;
  - `no_thermal`;
  - `fwf_low` and `fwf_high`.
* **Independent comparator.** A CMSY-style catch-only stock-reduction analysis (Martell & Froese 2013; Froese et al. 2017). It uses Schaefer dynamics, the same expanded landings, log-uniform priors on r (resilience class from M) and K, start depletion 0.5–0.9 and end depletion 0.2–0.8. It shares no energy-budget assumption with the TTM.
* **Value of information.** The first-order variance share (Sobol' main effect) of log MMSY for every input, estimated by quantile binning over the prior draws and corrected for binning noise (Saltelli et al. 2008; Oakley & O'Hagan 2004). Inputs are mapped to the data stream that would inform them.
* **Closed-loop management strategy evaluation.** 150 operating models are drawn from the SIR posterior and projected under five management procedures:
  1. status-quo effort;
  2. a fixed 25% trap reduction;
  3. the SPF effort-ceiling rule. Every three years, an assessment estimates each focus group's MMSY with CV 0.3, and each gear's effort is reset so that no focus group's catch by that gear exceeds its share of the P* target, with changes capped at ±25%;
  4. the same rule with a 50% optimistic assessment bias;
  5. an empirical catch-rate rule.

  Performance over 2036–2045 is summarized as:
  - the probability that any focus group falls below 0.5 B_MSY in any year;
  - the annual rate of such years;
  - median minimum B/B_MSY;
  - focus-group catch;
  - catch variability;
  - effort.
