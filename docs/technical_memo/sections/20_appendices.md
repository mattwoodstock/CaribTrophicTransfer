# References {-}

{{INCLUDE:references.md,1}}

# Appendix A. Notation {-}

| Symbol | Meaning | Units |
|---|---|---|
| $A_h$ | area of habitat h | km² |
| NPP$_h$ | net primary production of habitat h | t C km⁻² yr⁻¹ |
| $f_h$ | food-web fraction of NPP | – |
| $c$ | carbon to wet weight | g WW g C⁻¹ |
| $E_{h,k}$ | production of trophic-level pool k in habitat h | t WW yr⁻¹ |
| $\tau_h$ | trophic transfer efficiency | – |
| $D_{ik}$ | diet fraction of group i from pool k | – |
| $\pi_{ih}$ | habitat preference | – |
| $w_i$ | demand weight (biomass density × P/B) | t km⁻² yr⁻¹ |
| $P_i$ | production of group i | t WW yr⁻¹ |
| $M_i$ | natural mortality | yr⁻¹ |
| $K_i$, $K_{0,i}$ | carrying capacity (prevailing; no fishing feedback) | t |
| $\rho_i$, $\bar Q_i$ | refuge exponent; habitat quality experienced | – |
| $\nu_i$, $N_i$ | nursery elasticity; nursery index | – |
| $x_i$ | MSY/(M·B₀) | – |
| $\beta_i$, $n_i$ | B_MSY/K; Pella–Tomlinson shape | – |
| $\phi_i$, $s_i$ | open-recruitment fraction; supply index | – |
| $C_i$, $G_i$, $I_i$ | catch; ghost-fishing removals; immigration | t yr⁻¹ |
| $q_{gi}$, $E_g$ | catchability; effort of gear g | yr⁻¹ per unit effort; trips |
| $f_i$ | fishable (non-no-take) fraction | – |
| $\Gamma$, $G$, $\gamma$ | herbivory multiplier on coral growth; grazing index; elasticity | – |
| MMSY | multispecies maximum sustainable yield, $x M K$ | t (lb) yr⁻¹ |

# Appendix B. Assumptions register {-}

{{INCLUDE:assumptions.md,1}}

# Appendix C. Complete St. Croix input tables {-}

The tables below are generated from `Projects/StCroix_Trap/data/` and are the inputs used for the results in Section 10.

{{INCLUDE:stcroix_input_values.md,1}}

# Appendix D. Code and report review {-}

{{INCLUDE:code_review.md,1}}

# Appendix E. Software and reproducibility {-}

CaribTTM is an R package. Its source files and exported functions are listed below.

{{AUTO:code_inventory}}

To reproduce the St. Croix results, run from the repository root:

```
Rscript -e 'devtools::load_all("."); acquire_project_data("Projects/StCroix_Trap")'   # optional: refresh API data
Rscript Projects/StCroix_Trap/run_stcroix.R            # main run, robustness, validation, MSE
Rscript Projects/StCroix_Trap/run_structural_ensemble.R 1500 4
Rscript docs/build_input_tables.R
Rscript docs/technical_memo/build_memo.R
```

Unit tests: `devtools::test()`. Acquired data are stored in `Projects/StCroix_Trap/data/acquired/` with an acquisition log. Data-raw scripts (`Projects/StCroix_Trap/data-raw/`) regenerate:
* the Opitz and FishBase priors;
* the life-history x and B_MSY/K priors;
* the light index;
* the placeholder time series.
