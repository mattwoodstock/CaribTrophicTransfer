"""
Group-level life-history and thermal traits from FishBase (v24.07 tables served
by the rfishbase project on Hugging Face: datasets/cboettig/fishbase).

For each St. Croix FMP fish complex (species lists below), species traits are
combined with weights equal to Opitz (1996) Virgin Islands reef biomass where
available (equal weights otherwise):

* M_then      : natural mortality, Then et al. (2015) growth-based estimator
                M = 4.118 K^0.73 Linf^-0.33 (Linf in cm TL), from FishBase
                'estimate' K and Linf (Linf from maximum length via Froese &
                Binohlan 2000 where not reported)
* troph       : FishBase trophic level (diet-based where available)
* r_prior     : FishBase/Froese intrinsic rate of increase prior (prior_r)
* temp_pref   : AquaMaps temperature preference (min, mean, max) used to build
                Gaussian thermal-performance curves (t_opt = mean,
                t_width = (max - min) / 4)

Run from the repository root (needs pandas + pyarrow and network access to
huggingface.co):  python3 Projects/StCroix_Trap/data-raw/build_fishbase_traits.py
Outputs: data-raw/fishbase/fishbase_species_traits.csv and
         data-raw/fishbase/fishbase_group_traits.csv
"""
import os
import urllib.request
import pandas as pd
import numpy as np

ROOT = "Projects/StCroix_Trap/data-raw"
OUT = os.path.join(ROOT, "fishbase")
os.makedirs(OUT, exist_ok=True)
BASE = "https://huggingface.co/datasets/cboettig/fishbase/resolve/main/data/fb/v24.07/parquet/"

GROUPS = {
    "parrotfish_1": ["Scarus coelestinus", "Scarus coeruleus", "Scarus guacamaia"],
    "parrotfish_2": ["Scarus vetula", "Scarus taeniopterus", "Scarus iseri", "Sparisoma viride",
                     "Sparisoma aurofrenatum", "Sparisoma chrysopterum", "Sparisoma rubripinne", "Sparisoma radians"],
    "surgeonfishes": ["Acanthurus tractus", "Acanthurus bahianus", "Acanthurus chirurgus", "Acanthurus coeruleus"],
    "queen_triggerfish": ["Balistes vetula"],
    "grunts": ["Haemulon flavolineatum", "Haemulon plumierii", "Haemulon sciurus", "Haemulon aurolineatum",
               "Haemulon album", "Haemulon parra", "Haemulon chrysargyreum", "Haemulon carbonarium",
               "Haemulon macrostomum", "Anisotremus virginicus", "Anisotremus surinamensis"],
    "snappers_shallow": ["Lutjanus analis", "Lutjanus apodus", "Lutjanus griseus", "Lutjanus jocu",
                         "Lutjanus mahogoni", "Lutjanus synagris", "Lutjanus cyanopterus"],
    "yellowtail_snapper": ["Ocyurus chrysurus"],
    "snappers_deep": ["Etelis oculatus", "Lutjanus vivanus", "Lutjanus buccanella", "Rhomboplites aurorubens",
                      "Apsilus dentatus", "Pristipomoides aquilonaris"],
    "groupers_shallow": ["Epinephelus guttatus", "Epinephelus adscensionis", "Cephalopholis fulva",
                         "Cephalopholis cruentata", "Mycteroperca bonaci", "Mycteroperca tigris",
                         "Mycteroperca venenosa", "Mycteroperca interstitialis"],
    "squirrelfishes": ["Holocentrus adscensionis", "Holocentrus rufus", "Myripristis jacobus",
                       "Neoniphon marianus", "Sargocentron vexillarium"],
    "goatfishes": ["Mulloidichthys martinicus", "Pseudupeneus maculatus"],
    "boxfishes": ["Acanthostracion polygonius", "Acanthostracion quadricornis", "Lactophrys trigonus",
                  "Lactophrys bicaudalis", "Lactophrys triqueter"],
    "angelfishes": ["Holacanthus ciliaris", "Holacanthus tricolor", "Pomacanthus arcuatus", "Pomacanthus paru"],
    "wrasses_hogfish": ["Lachnolaimus maximus", "Bodianus rufus", "Halichoeres radiatus"],
    "jacks": ["Caranx ruber", "Caranx latus", "Caranx bartholomaei", "Caranx crysos", "Caranx lugubris",
              "Seriola dumerili", "Seriola rivoliana", "Trachinotus falcatus", "Trachinotus goodei",
              "Alectis ciliaris", "Elagatis bipinnulata"],
    "porgies": ["Calamus calamus", "Calamus bajonado", "Calamus pennatula", "Archosargus rhomboidalis"],
    "pelagics": ["Coryphaena hippurus", "Acanthocybium solandri"],
}
# Opitz (1996) Table 3.6 raw VI biomass (g/m2) as weights where available
opitz = pd.read_csv(os.path.join(ROOT, "opitz1996/opitz_table3_6_fmp_species.csv"))
alias = {"Scarus iserti": "Scarus iseri", "Haemulon plumieri": "Haemulon plumierii", "Haemulon parrai": "Haemulon parra",
         "Acanthurus bahianus": "Acanthurus tractus", "Elagatis bipinnulatus": "Elagatis bipinnulata"}
opitz["species_fb"] = opitz["species"].replace(alias)
wmap = dict(zip(opitz["species_fb"], opitz["biomass_g_m2_raw"]))


def get(t):
    p = os.path.join(OUT, t + ".parquet")
    if not os.path.exists(p):
        urllib.request.urlretrieve(BASE + t + ".parquet", p)
    return pd.read_parquet(p)


sp = get("species")[["SpecCode", "Genus", "Species", "FBname"]]
sp["name"] = sp["Genus"] + " " + sp["Species"]
est = get("estimate")[["SpecCode", "Troph", "K", "Linf", "MaxLengthTL", "prior_r", "TempPrefMin", "TempPrefMean", "TempPrefMax", "a", "b"]]
# Linf from maximum length where not reported (Froese & Binohlan 2000): log Linf = 0.044 + 0.9841 log Lmax
est["Linf_used"] = est["Linf"].where(est["Linf"].notna(), 10 ** (0.044 + 0.9841 * np.log10(est["MaxLengthTL"])))

rows = []
for grp, names in GROUPS.items():
    for n in names:
        s = sp[sp["name"] == n]
        if s.empty:
            rows.append({"group": grp, "species": n, "found": False}); continue
        e = est[est["SpecCode"] == s["SpecCode"].iloc[0]]
        r = {"group": grp, "species": n, "found": True, "SpecCode": int(s["SpecCode"].iloc[0]),
             "common": s["FBname"].iloc[0]}
        if not e.empty:
            e = e.iloc[0]
            r.update({k: e[k] for k in ["Troph", "K", "Linf", "MaxLengthTL", "Linf_used", "prior_r", "TempPrefMin", "TempPrefMean", "TempPrefMax", "a", "b"]})
            if pd.notna(e["K"]) and pd.notna(e["Linf_used"]) and e["Linf_used"] > 0:
                r["M_then"] = 4.118 * e["K"] ** 0.73 * e["Linf_used"] ** -0.33
        r["weight"] = wmap.get(n, np.nan)
        rows.append(r)
spt = pd.DataFrame(rows)
for c in ["M_then", "prior_r", "Troph"]:
    if c not in spt.columns:
        spt[c] = np.nan
spt.to_csv(os.path.join(OUT, "fishbase_species_traits.csv"), index=False)
# species -> group map with NCRMP/RVC species codes (first 3 letters of genus + first 4 of species)
# and FishBase length-weight parameters (W[g] = a L[cm]^b) for the NCRMP biomass estimator
lw = spt[spt["found"]][["group", "species", "a", "b"]].copy()
lw["species_cd"] = lw["species"].apply(lambda n: (n.split()[0][:3] + " " + n.split()[1][:4]).upper())
lw.to_csv(os.path.join(OUT, "ncrmp_species_map.csv"), index=False)


def wavg(d, col):
    d = d[pd.notna(d[col])]
    if d.empty:
        return np.nan
    w = d["weight"].fillna(d["weight"].median() if d["weight"].notna().any() else 1.0).fillna(1.0)
    w = w.where(w > 0, 1e-3)
    return float(np.sum(d[col] * w) / np.sum(w))


grp = []
for g_, d in spt[spt["found"]].groupby("group"):
    tmin, tmean, tmax = wavg(d, "TempPrefMin"), wavg(d, "TempPrefMean"), wavg(d, "TempPrefMax")
    grp.append({"group": g_, "n_species": len(d), "M_then": wavg(d, "M_then"),
                "M_then_min": d["M_then"].min(), "M_then_max": d["M_then"].max(),
                "troph": wavg(d, "Troph"), "r_prior": wavg(d, "prior_r"),
                "t_pref_min": tmin, "t_opt": tmean, "t_pref_max": tmax,
                "t_width": (tmax - tmin) / 4 if pd.notna(tmax) and pd.notna(tmin) else np.nan})
pd.DataFrame(grp).round(3).to_csv(os.path.join(OUT, "fishbase_group_traits.csv"), index=False)
print(pd.DataFrame(grp).round(3).to_string(index=False))
missing = spt[~spt["found"]]["species"].tolist()
if missing:
    print("Not found in FishBase:", missing)
