# Primary production and energy input

## Habitat-specific net primary production

For each habitat $h$ and year $t$, energy entering the consumer food web (t wet weight yr⁻¹) is

$$
P1_h(t) = A_h\,a_h(t)\;\cdot\;\text{NPP}_h\;\cdot\;I_h(t)\;\cdot\;d_h(t)\;\cdot\;Q_h(t)^{\epsilon_h}\;\cdot\;f_h\;\cdot\;c
$$

where:
* $A_h$ is the mapped area (km²) and $a_h(t)$ a scenario area multiplier (e.g., seagrass loss);
* NPP$_h$ is net primary production (t C km⁻² yr⁻¹);
* $I_h(t)$ is an annual production index relative to its historical mean (water clarity for seagrass and macroalgae, the satellite NPP index for the pelagic habitat);
* $d_h(t)$ is a disturbance multiplier (hurricane loss with exponential recovery);
* $Q_h(t)^{\epsilon_h}$ is an optional habitat-quality elasticity;
* $f_h$ is the **food-web fraction**, the share of NPP that enters the grazing and detrital food web used by the modelled consumers;
* $c$ is the carbon-to-wet-weight conversion (9 g WW per g C; Pauly & Christensen 1995).

### Unit conversion

Published benthic production rates are reported in many units: g C, g dry weight (DW) or mmol O₂, per m² per day or per year. CaribTTM converts each to t C km⁻² yr⁻¹ using:
* the photosynthetic quotient (1 mol O₂ ≈ 1 mol C fixed);
* habitat-appropriate carbon-to-dry-weight ratios (≈ 0.34 g C per g DW for seagrass tissue);
* 365 days per year.

The function `convert_productivity()` documents each conversion. Each habitat's input row records the value as published together with its unit, so conversions can be audited.

### Benthic rates used for St. Croix

The St. Croix values (Appendix C) are literature rates, each recorded with its source and uncertainty:

| Habitat | Rate and source |
|---|---|
| Coral reef | 1,500 t C km⁻² yr⁻¹. This is within the 986–4,108 t C km⁻² yr⁻¹ measured on St. Croix reefs by Adey & Steneck (1985), and bracketed by the Indian Ocean range of Nair & Pillai (1972). CV 0.5. |
| Colonized pavement | 181 mmol O₂ m⁻² d⁻¹, the lagoon-wide net production of turf- and macroalgae-dominated hard bottom (Naumann et al. 2013). |
| Seagrass | 4.2 g DW m⁻² d⁻¹, the annual mean *Thalassia testudinum* productivity in Bon Accord Lagoon, Tobago (Juman 2005a). That lagoon is nutrient-enriched, so the value is at the high end of the CARICOMP range (van Tussenbroek et al. 2014). Applying it to St. Croix is an assumption; CV 0.5. |
| Macroalgae | 582 mmol O₂ m⁻² d⁻¹ (Wanders 1976, Curaçao). |
| Sand and mud | 7 mmol O₂ m⁻² d⁻¹ of microphytobenthic production (Naumann et al. 2013). |
| Mangrove | 400 mmol O₂ m⁻² d⁻¹ (Gattuso et al. 1998). Only the exported litter fraction enters the food web ($f$ = 0.2). |

These rates are the largest single source of uncertainty in absolute scale. They are sampled with CVs of 0.3–0.6, and their contribution to MMSY variance is reported by the value-of-information analysis (Section 9).

### Water-clarity index for light-limited habitats

Seagrass and macroalgal production is light-limited. Interannual changes in water clarity are represented with a bottom-light index derived from the ESA Ocean Colour Climate Change Initiative diffuse attenuation coefficient at 490 nm (Kd490, 1998–2024), as follows:
1. The PAR attenuation coefficient is Kd(PAR) = 0.0864 + 0.884 Kd490 (Morel et al. 2007).
2. Bottom light is exp(−Kd(PAR) z), at representative depths z = 4 m (seagrass) and 8 m (macroalgae). These depths are assumptions.
3. The index is expressed relative to its 1998–2024 mean, with an assumed elasticity of production to bottom light of 1 (light limitation; Duarte 1991).

Before 1998 the index is held at 1, with an uncertainty equal to its interannual standard deviation. St. Croix waters are clear, so the index varies by only about ±2% between years. That small effect is itself informative: water-clarity variation is not a major driver of benthic production at the shelf scale.

### Deeper shelf (30–100 m)

The deeper shelf, including Lang Bank, is light-limited mesophotic habitat. Its area is the ETOPO 2022 depth band from 30 to 100 m within the domain (41.4 km², area CV 0.3).

Its production is computed by integrating a light-limited photosynthesis–irradiance (tanh) curve over the band:
* Surface PAR comes from SeaWiFS and MODIS.
* Kd(PAR) comes from OC-CCI Kd490 (Morel et al. 2007).
* The curve is scaled to the pavement production rate at a 10 m reference depth.

The result, about 170 t C km⁻² yr⁻¹, is used with CV 0.6. Composition surveys of Lang Bank would refine both the rate and the food-web fraction.

## Pelagic production

Pelagic NPP within the shelf-plus-buffer domain is set to 85 g C m⁻² yr⁻¹, the regional magnitude used in SEDAR103-DW-20 Appendix A. The value lies within the satellite NPP ensemble range of Ryan-Keogh et al. (2023).

The annual pelagic production index (1998–2024) is computed within the package. It is the mean of two algorithms, the Vertically Generalized Production Model (VGPM; Behrenfeld & Falkowski 1997) and its Eppley-temperature variant (Eppley 1972), driven by:
* OC-CCI chlorophyll;
* Coral Reef Watch SST;
* SeaWiFS/MODIS PAR;
* day length (Forsythe et al. 1995);
* euphotic depth (Morel & Berthon 1989).

Index uncertainty combines between-algorithm and spatial variability. The computed ensemble gives absolute shelf values of 130–240 g C m⁻² yr⁻¹. These are likely biased high by bottom reflectance in shallow water, which is why the regional magnitude is used for the level and the computed series only for the interannual index.

**Climate projections** of SST and the NPP index for 2025–2045 use the delta method. Changes from a 2005–2014 baseline in CMIP6 CESM2 and ACCESS-ESM1-5 (SSP2-4.5 and SSP5-8.5) are applied to the observed baseline, with between-model spread as uncertainty.

## Food-web fraction

Not all net primary production is available to the fished food web. Some is exported, buried or consumed by microbial loops that do not reach the modelled consumers. The food-web fraction (0.20–0.50 by habitat) is an expert-judgement parameter informed by detritus flows in Ecopath models. It is one of the most influential inputs on absolute MMSY (Section 10). It is sampled with CV 0.3 and tested in the structural ensemble at ×0.67 and ×1.5.
