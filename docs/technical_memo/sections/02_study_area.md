# Study area and fishery

## The St. Croix insular shelf

St. Croix (17.7° N, 64.7° W) is the largest of the U.S. Virgin Islands. It sits on its own insular platform, separated from the Puerto Rico–Virgin Islands platform to the north by the Virgin Islands Basin, whose depths exceed 4,000 m. This isolation is why St. Croix is managed as a separate FMP unit. It also justifies treating the shelf as a closed energy system for resident reef species, while recognizing that larvae of many species (notably spiny lobster) arrive from upstream sources.

The model domain is the shelf bounded by 64.4–65.0° W and 17.6–17.95° N, which takes in:
* Lang Bank to the east;
* Buck Island Reef National Monument (BIRNM) to the northeast;
* the East End Marine Park (EEMP);
* the narrower north and south shelves.

**Benthic habitats** come from the NOAA NCCOS 2002 benthic habitat map of the U.S. Virgin Islands. The map is acquired through its ArcGIS REST service, and its detailed classes are aggregated into seven model habitats (Section 8):
* coral reef (≈42 km²);
* colonized pavement and other colonized hard bottom (≈192 km²);
* seagrass (≈59 km²);
* macroalgae (≈13 km²);
* sand and mud (≈13 km²);
* mangrove (≈1.3 km²);
* deeper shelf at 30–100 m (≈41 km²).

The deeper shelf is not resolved by the NCCOS map ("Unknown" class). An earlier configuration took its area as 388 km², which is mostly off-shelf water. It was re-derived from ETOPO 2022 bathymetry as the 30–100 m depth band, with light-limited production computed from satellite water clarity and surface irradiance (Section 4).

**The pelagic domain** is the shelf plus a ~10 km buffer (≈1,500 km²), the zone where trap and hook-and-line fishing takes place.

## Ecosystem change since 1983

The St. Croix reef ecosystem changed profoundly over the modelled period:
* **Coral cover** fell from roughly 20% or more in the early 1980s to about 3–5% by the 2020s. Causes include white-band disease, hurricanes (Hugo 1989, Marilyn 1995, Lenny 1999, Maria 2017), bleaching (notably 2005, 2023 and 2024, the highest degree-heating-week years in the Coral Reef Watch record) and, since 2021, stony coral tissue loss disease (SCTLD).
* **Sea-surface temperature** has risen by about 1–1.5 °C. Degree heating weeks reached record values in 2023–2024.
* **Lionfish** (*Pterois volitans/miles*) were first recorded in the U.S. Virgin Islands in 2008–2009 and were established by the early 2010s (Schofield 2010).
* **Pelagic *Sargassum*** inundations began in 2011 with the formation of the Great Atlantic *Sargassum* Belt (Gower et al. 2013; Wang et al. 2019). Record years in the satellite record are 2018 and 2021.

These changes alter primary production, habitat quality and the survival of reef-associated fishes. They are represented as model forcing (Section 6).

## The trap fishery

St. Croix's commercial fishery is small-scale and multigear. Fish traps (wire chevron and arrowhead traps) and lobster traps have historically dominated landings. Diving (free and SCUBA, spear and hand) and hook-and-line (bottom handline, rod and reel, and troll for pelagics) make up the rest.

Traps are non-selective and catch, among others:
* spiny lobster;
* queen triggerfish;
* parrotfishes;
* surgeonfishes;
* grunts;
* snappers;
* groupers;
* squirrelfishes;
* goatfishes;
* boxfishes;
* angelfishes;
* hogfish and other wrasses;
* porgies.

Hurricanes repeatedly destroyed large numbers of traps. Even in ordinary years, about 10% of traps are lost; in St. Thomas/St. John, about 650 of roughly 6,500 traps per year (Clark et al. 2012). Derelict traps continue to catch fish (Renchen et al. 2010; Arthur et al. 2014).

Management measures relevant to the trap fishery:
* complex-level ACLs;
* prohibition of harvest of the large parrotfishes (Parrotfish 1: midnight, blue and rainbow parrotfish);
* minimum sizes (e.g., spiny lobster carapace length);
* seasonal and area closures (e.g., the red hind closure on Lang Bank);
* no-take areas within BIRNM and the EEMP.

## Assessed stocks used as anchors

Two stocks have assessment-based overfishing limits (OFLs) that anchor the model's absolute scale:

* **Caribbean spiny lobster** (*Panulirus argus*), St. Croix: OFL 163,823 lb (SEDAR 57 Update; Federal Register 2024-09227).
* **Queen triggerfish** (*Balistes vetula*), St. Croix: OFL 22,773 lb (SEDAR 80; CFMC Scientific and Statistical Committee, April 2024).

Published ACLs for other complexes are used only for comparison, not calibration, because ACLs include management buffers:
* Parrotfish 2;
* Snapper 1–3;
* queen conch;
* dolphinfish.

## Time frame

* **History:** 1983–2024. 1983 is the start of the commercial landings series used in SEDAR 80. Biomass is driven by reported landings expanded for under-reporting.
* **Reference years:** 2022–2024, for current reference points.
* **Projections:** 2025–2045, driven by effort under scenarios.
