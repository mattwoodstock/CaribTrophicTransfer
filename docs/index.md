# CaribTTM Documentation {.unnumbered}

`CaribTTM` is the trophic transfer model (TTM) of the integrated trophic frontier framework for U.S. Caribbean fisheries (SEDAR103-DW-20). It estimates **guild- and group-level multispecies MSY (MMSY)** from benthic habitat-specific and pelagic primary production. The estimates are made under prevailing oceanographic, habitat, disturbance and fishery conditions, with uncertainty propagated by Monte Carlo and conditioned on data with SIR. It produces the biological inputs, including gear-specific MMSY, effort ceilings and choke groups, needed by the stochastic production frontier (SPF) and the management strategy evaluation.

| Chapter | Contents |
|---|---|
| [User Manual](user_manual.md) | installation, quick start, workflow, running the model, ocean-model inputs, outputs and SPF hand-off, troubleshooting |
| [Input Data Dictionary](input_dictionary.md) | every input file and column, units, defaults, and St. Croix values |
| [Data Acquisition & Uncertainty](data_acquisition.md) | automated API downloads (ERDDAP, OPeNDAP/MOM6, Copernicus/GLORYS, NCCOS habitat maps, HURDAT2), user-override rules, full list of propagated uncertainties |
| [Model Description](model_description.md) | equations, capabilities, MMSY rationale, uncertainty, assumptions |
| [St. Croix Case Study](case_study_stcroix.md) | application to the St. Croix trap fishery: data, results, caveats, data-replacement checklist |
| [Review Readiness](review_readiness.md) | anticipated CIE critiques mapped to responses, evidence files and open items |
| [Data Needs](data_needs.md) | every placeholder still to be replaced, grouped by data type, with priority and suggested source |
| [St. Croix Input Values](stcroix_input_values.md) | full input tables (auto-generated) |
| [References](references.md) | literature cited |

## Building the documentation

The chapters are plain Markdown and render directly on GitHub. To build an HTML/PDF book with [Quarto](https://quarto.org):

```bash
cd docs && quarto render
```

Regenerate the auto-generated St. Croix tables after editing inputs:

```bash
Rscript docs/build_input_tables.R
```

## Presentation

`docs/presentation/CaribTTM_StCroix_Trap.pptx` is a 28-slide deck in two parts. Part 1 covers the model processes and data needs; Part 2 presents the St. Croix case study. Rebuild it from the latest run outputs with:

```bash
NODE_PATH=<dir with pptxgenjs, react-icons, react, react-dom, sharp>/node_modules node docs/presentation/build_deck.js
```
