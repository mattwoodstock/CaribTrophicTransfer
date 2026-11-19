// =============================================================================
// CaribTTM presentation builder (pptxgenjs)
// Part 1: model processes and data needs; Part 2: St. Croix trap case study.
// Case-study numbers and charts are read from Projects/StCroix_Trap/output.
// Run from the repository root:  node docs/presentation/build_deck.js
// =============================================================================
const fs = require("fs");
const path = require("path");
const pptxgen = require("pptxgenjs");
const React = require("react");
const ReactDOMServer = require("react-dom/server");
const sharp = require("sharp");
const fa = require("react-icons/fa");
const gi = require("react-icons/gi");
const wi = require("react-icons/wi");

const ROOT = process.cwd();
const PROJ = path.join(ROOT, "Projects/StCroix_Trap");
const OUT = path.join(PROJ, "output");
const FIG = path.join(OUT, "figures");
const TAB = path.join(OUT, "tables");
const DECK = path.join(ROOT, "docs/presentation/CaribTTM_StCroix_Trap.pptx");
const SKILL = process.env.PPTX_SKILL_DIR;

// ---------------------------------------------------------------- data helpers
function readCSV(f) {
  const txt = fs.readFileSync(f, "utf8").trim().split(/\r?\n/);
  const split = (line) => {
    const out = []; let cur = ""; let q = false;
    for (const ch of line) {
      if (ch === '"') q = !q; else if (ch === "," && !q) { out.push(cur); cur = ""; } else cur += ch;
    }
    out.push(cur); return out;
  };
  const head = split(txt[0]);
  return txt.slice(1).map((l) => { const v = split(l); const o = {}; head.forEach((h, i) => (o[h] = v[i])); return o; });
}
const num = (x) => Number(x);
const fmt = (x, d = 0) => Number(x).toLocaleString("en-US", { maximumFractionDigits: d, minimumFractionDigits: d });
const fmtK = (lb) => (lb >= 1e6 ? fmt(lb / 1e6, 2) + "M" : fmt(lb / 1e3, 0) + "k");

const rp = readCSV(path.join(TAB, "spf_group_reference_points.csv")).filter((r) => r.scenario === "Baseline");
const guild = readCSV(path.join(TAB, "spf_guild_mmsy.csv")).filter((r) => r.level === "guild");
const ceil = readCSV(path.join(TAB, "spf_gear_effort_ceilings.csv"));
const bench = readCSV(path.join(TAB, "benchmark_comparison.csv"));
const hab = readCSV(path.join(PROJ, "data/acquired/habitats.csv")).filter((r) => r.area_km2 !== "");
const log = fs.readFileSync(path.join(OUT, "run_log.txt"), "utf8");
const essM = log.match(/ESS = ([0-9.]+), unique = ([0-9]+)/);
const ESS = essM ? Math.round(num(essM[1])) : NaN;
const NUNIQ = essM ? num(essM[2]) : NaN;
const priorM = log.match(/prior draws: ([0-9]+)/);
const NPRIOR = priorM ? num(priorM[1]) : 5000;
const g = (name) => rp.find((r) => r.group === name);
const fmpRows = rp.filter((r) => r.group_type === "fmp" && r.harvest_allowed === "TRUE");
const totMMSY = fmpRows.reduce((s, r) => s + num(r.MMSY_p50_lb), 0);
const totCatch = fmpRows.reduce((s, r) => s + num(r.catch_lb), 0);
const lob = g("spiny_lobster"), qt = g("queen_triggerfish");
const trapCeil = ceil.find((r) => r.gear === "trap_fish");
const lobCeil = ceil.find((r) => r.gear === "trap_lobster");
const calRows = fmpRows.filter((r) => r.calibrated === "TRUE");
const calMMSY = calRows.reduce((s, r) => s + num(r.MMSY_p50_lb), 0);
const calCatch = calRows.reduce((s, r) => s + num(r.catch_lb), 0);
const selfT = readCSV(path.join(TAB, "self_test_summary.csv"));
const loo = readCSV(path.join(TAB, "validation_loo_benchmarks.csv"));
const ensP = readCSV(path.join(TAB, "structural_ensemble_pooled.csv"));
const eco = readCSV(path.join(TAB, "ecosystem_overfishing_indices.csv"));
const diagS = readCSV(path.join(TAB, "sir_diagnostics_summary.csv"))[0];
const scen = readCSV(path.join(TAB, "spf_scenario_reference_points.csv"));
const cov90 = selfT.map((r) => num(r.coverage90));
const ensG = (gname) => ensP.find((r) => r.group === gname);
const ecoLast = eco.filter((r) => ["2022", "2023", "2024"].includes(r.year));
const sc = (sn, gname) => scen.find((r) => r.scenario === sn && r.group === gname);
const rdOpt = (f) => (fs.existsSync(path.join(TAB, f)) ? readCSV(path.join(TAB, f)) : []);
const lhx = fs.existsSync(path.join(PROJ, "data-raw/life_history/xmsy_priors.csv")) ? readCSV(path.join(PROJ, "data-raw/life_history/xmsy_priors.csv")) : [];
const mcmp = rdOpt("method_comparison_catch_only.csv");
const voiS = rdOpt("value_of_information_streams.csv").filter((r) => r.output === "total_focus_MMSY");
const mseS = rdOpt("mse_performance.csv");
const pdx = rdOpt("process_diagnostics.csv");

// ---------------------------------------------------------------- theme
const THEME = {
  name: "Reef and Deep Ocean",
  headFontFace: "Cambria",
  bodyFontFace: "Calibri",
  colors: {
    dk1: "0B2A3C", lt1: "FFFFFF", dk2: "1D4E5F", lt2: "EEF5F4",
    accent1: "0E7C86", accent2: "F07167", accent3: "F4B860", accent4: "00AFB9",
    accent5: "6A994E", accent6: "8D6A9F", hlink: "0E7C86", folHlink: "6A994E",
  },
};
const HEX = THEME.colors;

// ---------------------------------------------------------------- icons
async function icon(Comp, color = "FFFFFF", size = 256) {
  const svg = ReactDOMServer.renderToStaticMarkup(React.createElement(Comp, { color: "#" + color, size: String(size) }));
  const buf = await sharp(Buffer.from(svg)).png().toBuffer();
  return "image/png;base64," + buf.toString("base64");
}

(async () => {
  const pres = new pptxgen();
  pres.layout = "LAYOUT_WIDE"; // 13.33 x 7.5
  pres.title = "CaribTTM: trophic transfer model and St. Croix trap fishery case study";
  pres.author = "CaribTTM contributors";
  pres.theme = { headFontFace: THEME.headFontFace, bodyFontFace: THEME.bodyFontFace };
  const C = pres.SchemeColor;

  // ---- layouts -------------------------------------------------------------
  pres.defineSlideMaster({
    title: "TITLE_DARK", background: { color: HEX.dk1 },
    objects: [
      { placeholder: { options: { name: "title", type: "title", x: 0.8, y: 2.3, w: 11.7, h: 1.6, fontFace: THEME.headFontFace, fontSize: 44, bold: true, color: C.background1, valign: "bottom", align: "left" }, text: "" } },
      { placeholder: { options: { name: "body", type: "body", x: 0.8, y: 4.05, w: 11.7, h: 1.4, fontFace: THEME.bodyFontFace, fontSize: 20, color: "CADFE3", valign: "top", align: "left" }, text: "" } },
    ],
  });
  pres.defineSlideMaster({
    title: "SECTION_DARK", background: { color: HEX.dk2 },
    objects: [
      { placeholder: { options: { name: "title", type: "title", x: 0.8, y: 2.9, w: 11.7, h: 1.2, fontFace: THEME.headFontFace, fontSize: 40, bold: true, color: C.background1, valign: "middle", align: "left" }, text: "" } },
      { placeholder: { options: { name: "body", type: "body", x: 0.8, y: 4.15, w: 11.7, h: 1.0, fontFace: THEME.bodyFontFace, fontSize: 18, color: "CADFE3", valign: "top", align: "left" }, text: "" } },
    ],
  });
  pres.defineSlideMaster({
    title: "CONTENT", background: { color: HEX.lt1 },
    slideNumber: { x: 12.3, y: 7.05, w: 0.6, h: 0.3, fontFace: THEME.bodyFontFace, fontSize: 10, color: "6B7F87", align: "right" },
    objects: [
      { text: { text: "CaribTTM  |  SEDAR 103 trophic frontier framework", options: { x: 0.5, y: 7.05, w: 8, h: 0.3, fontFace: THEME.bodyFontFace, fontSize: 10, color: "6B7F87" } } },
      { placeholder: { options: { name: "title", type: "title", x: 0.5, y: 0.3, w: 12.3, h: 0.85, fontFace: THEME.headFontFace, fontSize: 28, bold: true, color: C.text1, valign: "middle", align: "left", margin: 0 }, text: "" } },
    ],
  });

  // ---- reusable pieces ---------------------------------------------------------
  const ICON = {};
  const iconList = {
    water: fa.FaWater, fish: fa.FaFish, leaf: fa.FaLeaf, sat: fa.FaSatellite, chart: fa.FaChartLine, db: fa.FaDatabase,
    cloud: fa.FaCloudDownloadAlt, ship: fa.FaShip, scale: fa.FaBalanceScale, random: fa.FaRandom, filter: fa.FaFilter,
    cogs: fa.FaCogs, layers: fa.FaLayerGroup, thermo: fa.FaThermometerHalf, map: fa.FaMapMarkedAlt, anchor: fa.FaAnchor,
    warn: fa.FaExclamationTriangle, check: fa.FaCheckCircle, sun: fa.FaSun, diagram: fa.FaProjectDiagram, edit: fa.FaUserEdit,
    table: fa.FaTable, clip: fa.FaClipboardList, globe: fa.FaGlobeAmericas, seed: fa.FaSeedling, lock: fa.FaLock,
    flask: fa.FaFlask, recycle: fa.FaRecycle, target: fa.FaBullseye, coral: gi.GiCoral,
    net: gi.GiFishingNet, wave: gi.GiWaveCrest, hurricane: wi.WiHurricane,
  };
  for (const k in iconList) ICON[k] = await icon(iconList[k]);

  let objN = 0;
  const nm = (p) => `${p}_${++objN}`;
  function iconCircle(slide, key, x, y, d = 0.62, fill = HEX.accent1) {
    slide.addShape(pres.shapes.OVAL, { x, y, w: d, h: d, fill: { color: fill }, line: { color: fill }, objectName: nm("iconbg") });
    const p = d * 0.22;
    slide.addImage({ data: ICON[key], x: x + p, y: y + p, w: d - 2 * p, h: d - 2 * p, objectName: nm("icon"), altText: key });
  }
  function card(slide, x, y, w, h, fill = HEX.lt2) {
    slide.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y, w, h, rectRadius: 0.12, fill: { color: fill }, line: { color: fill }, objectName: nm("card") });
  }
  function txt(slide, text, o) {
    slide.addText(text, Object.assign({ isTextBox: true, fontFace: THEME.bodyFontFace, fontSize: 14, color: HEX.dk1, valign: "top", margin: 0, objectName: nm("txt") }, o));
  }
  function bullets(slide, items, o) {
    const runs = items.map((t, i) => ({ text: t, options: { bullet: true, breakLine: i < items.length - 1 } }));
    slide.addText(runs, Object.assign({ isTextBox: true, fontFace: THEME.bodyFontFace, fontSize: 14, color: HEX.dk1, valign: "top", paraSpaceAfter: 6, margin: 0.02, objectName: nm("list") }, o));
  }
  function arrow(slide, x1, y1, x2, y2, color = HEX.dk2) {
    slide.addShape(pres.shapes.LINE, { x: Math.min(x1, x2), y: Math.min(y1, y2), w: Math.abs(x2 - x1), h: Math.abs(y2 - y1), flipH: x2 < x1, flipV: y2 < y1, line: { color, width: 2, endArrowType: "triangle" }, objectName: nm("arrow") });
  }
  function content(title, section) {
    const s = pres.addSlide({ masterName: "CONTENT", sectionTitle: section });
    s.addText(title, { placeholder: "title" });
    return s;
  }
  function figure(slide, file, x, y, w, h, alt) {
    const f = path.join(FIG, file);
    if (fs.existsSync(f)) slide.addImage({ path: f, x, y, w, h, altText: alt, objectName: nm("fig"), sizing: { type: "contain", w, h } });
    else txt(slide, `[missing figure: ${file}]`, { x, y, w, h, color: HEX.accent2 });
  }
  function stat(slide, value, label, x, y, w, color = HEX.accent1) {
    txt(slide, value, { x, y, w, h: 0.75, fontFace: THEME.headFontFace, fontSize: 36, bold: true, color });
    txt(slide, label, { x, y: y + 0.78, w, h: 0.6, fontSize: 12, color: "4A5A61" });
  }

  const S1 = "Part 1 - The CaribTTM model";
  const S2 = "Part 2 - St. Croix trap fishery case study";

  // =========================================================================
  // TITLE
  pres.addSection({ title: "Introduction" });
  let s = pres.addSlide({ masterName: "TITLE_DARK", sectionTitle: "Introduction" });
  s.addText("CaribTTM: an island-based trophic transfer model", { placeholder: "title" });
  s.addText("Multispecies MSY for U.S. Caribbean fisheries under changing ocean, habitat and fishing conditions, with a St. Croix trap fishery case study", { placeholder: "body" });
  iconCircle(s, "coral", 0.8, 1.15, 0.85, HEX.accent2);
  iconCircle(s, "fish", 1.85, 1.15, 0.85, HEX.accent1);
  iconCircle(s, "wave", 2.9, 1.15, 0.85, HEX.accent4);
  txt(s, "SEDAR 103 integrated trophic frontier framework  |  model and data documentation", { x: 0.8, y: 6.5, w: 11, h: 0.4, fontSize: 14, color: "9FC3C9" });
  s.addNotes("CaribTTM implements Step 1 of the SEDAR103-DW-20 framework: ecosystem- and guild-level MMSY caps. Part 1 describes how the model works and what data it needs. Part 2 applies it to the St. Croix trap fishery with real public data and clearly flagged placeholders.");

  // WHY
  s = content("Single-species ACLs fit poorly on multispecies island fisheries", "Introduction");
  const why = [
    ["anchor", "Finite shelf area", "Small insular platforms with fixed habitat and finite productivity"],
    ["net", "Multispecies gear", "Traps catch dozens of species at once; single-species controls are hard to apply"],
    ["clip", "Data limitations", "Self-reported landings with unknown under-reporting; few stock assessments"],
    ["scale", "Need for input controls", "Gear-level effort limits tied to ecosystem caps are more practical and enforceable"],
  ];
  why.forEach((w, i) => {
    const x = 0.5 + i * 3.1;
    card(s, x, 1.55, 2.9, 3.6);
    iconCircle(s, w[0], x + 0.3, 1.85, 0.7, i % 2 ? HEX.accent2 : HEX.accent1);
    txt(s, w[1], { x: x + 0.3, y: 2.75, w: 2.4, h: 0.75, fontSize: 18, bold: true, fontFace: THEME.headFontFace });
    txt(s, w[2], { x: x + 0.3, y: 3.6, w: 2.4, h: 1.45, fontSize: 14, color: "34474F" });
  });
  txt(s, "CaribTTM converts what can be measured (habitat maps, satellite ocean data, published productivity rates, landings) into biological caps on the energy available to each FMP complex.", { x: 0.5, y: 5.55, w: 12.3, h: 0.9, fontSize: 16, italic: true, color: HEX.dk2 });

  // FRAMEWORK
  s = content("CaribTTM is Step 1 of a four-step management framework", "Introduction");
  const steps = [
    ["1", "Trophic transfer model", "Ecosystem, guild and group MMSY caps from primary production", HEX.accent2, "CaribTTM"],
    ["2", "Stochastic production frontier", "Fleet capacity and efficiency by gear from trip tickets", HEX.accent1, "collaborators"],
    ["3", "Management framework", "Align caps with capacity: effort ceilings, choke groups, HCR", HEX.accent1, "joint"],
    ["4", "Simulation testing (MSE)", "Closed-loop tests under shocks and observation error", HEX.accent1, "joint"],
  ];
  steps.forEach((st, i) => {
    const x = 0.5 + i * 3.15;
    card(s, x, 1.7, 2.85, 3.3, i === 0 ? "FDE3E0" : HEX.lt2);
    s.addShape(pres.shapes.OVAL, { x: x + 0.25, y: 1.95, w: 0.7, h: 0.7, fill: { color: st[3] }, line: { color: st[3] }, objectName: nm("num") });
    txt(s, st[0], { x: x + 0.25, y: 1.95, w: 0.7, h: 0.7, fontSize: 24, bold: true, color: "FFFFFF", align: "center", valign: "middle", fontFace: THEME.headFontFace });
    txt(s, st[1], { x: x + 0.25, y: 2.85, w: 2.4, h: 0.7, fontSize: 18, bold: true, fontFace: THEME.headFontFace });
    txt(s, st[2], { x: x + 0.25, y: 3.6, w: 2.4, h: 1.0, fontSize: 14, color: "34474F" });
    txt(s, st[4], { x: x + 0.25, y: 4.55, w: 2.4, h: 0.35, fontSize: 12, italic: true, color: i === 0 ? HEX.accent2 : HEX.accent1 });
    if (i < 3) arrow(s, x + 2.88, 3.35, x + 3.12, 3.35);
  });
  txt(s, "CaribTTM delivers the biological side of Steps 3-4: group and guild MMSY distributions, gear-specific MMSY, effort ceilings and choke groups, written as SPF-ready tables.", { x: 0.5, y: 5.45, w: 12.3, h: 0.9, fontSize: 16, color: HEX.dk2 });

  // =========================================================================
  // PART 1
  pres.addSection({ title: S1 });
  s = pres.addSlide({ masterName: "SECTION_DARK", sectionTitle: S1 });
  s.addText("Part 1  |  How the model works", { placeholder: "title" });
  s.addText("Energy in, energy shared, biomass dynamics, reference points, uncertainty, data", { placeholder: "body" });

  // ARCHITECTURE
  s = content("Model architecture: from photons to gear-level effort limits", S1);
  const arch = [
    ["sat", "Primary production", "Habitat area x net PP (benthic); satellite / CMIP6 NPP (pelagic)"],
    ["layers", "Energy pools", "TL1-TL4 per habitat with transfer efficiency and fishing feedback"],
    ["diagram", "Partitioning", "Diet x habitat preference x demand weights -> group production"],
    ["recycle", "Dynamics", "Logistic biomass, time-varying K, landings then effort"],
    ["target", "Reference points", "MMSY, OFL proxy, P* target, B/BMSY, EU, gear ceilings"],
  ];
  arch.forEach((a, i) => {
    const x = 0.5 + i * 2.5;
    card(s, x, 1.55, 2.25, 2.9);
    iconCircle(s, a[0], x + 0.8, 1.75, 0.65, i === 4 ? HEX.accent2 : HEX.accent1);
    txt(s, a[1], { x: x + 0.1, y: 2.5, w: 2.05, h: 0.4, fontSize: 15, bold: true, align: "center", fontFace: THEME.headFontFace });
    txt(s, a[2], { x: x + 0.15, y: 3.0, w: 1.95, h: 1.35, fontSize: 12, align: "center", color: "34474F" });
    if (i < 4) arrow(s, x + 2.27, 3.0, x + 2.48, 3.0);
  });
  const drivers = [
    ["thermo", "Oceanography", "SST, DHW, NPP index, Kd490 (APIs)"],
    ["coral", "Habitat", "Area, coral cover, refuge quality"],
    ["hurricane", "Disturbance", "Hurricanes, bleaching, disease"],
    ["ship", "Fisheries", "Landings, effort, gear catchability"],
    ["random", "Uncertainty", "Monte Carlo + SIR conditioning"],
  ];
  drivers.forEach((d, i) => {
    const x = 0.5 + i * 2.5;
    iconCircle(s, d[0], x, 4.85, 0.5, HEX.accent4);
    txt(s, d[1], { x: x + 0.6, y: 4.83, w: 1.75, h: 0.3, fontSize: 14, bold: true });
    txt(s, d[2], { x: x + 0.6, y: 5.15, w: 1.75, h: 0.75, fontSize: 12, color: "34474F" });
  });
  txt(s, "Annual time step, 1983-2045 in the case study; every arrow carries uncertainty", { x: 0.5, y: 6.25, w: 12.3, h: 0.4, fontSize: 14, italic: true, color: HEX.dk2 });

  // PRIMARY PRODUCTION
  s = content("Primary production is resolved by habitat in one currency", S1);
  card(s, 0.5, 1.45, 6.2, 5.3);
  txt(s, "Energy entering each habitat (t wet weight / yr)", { x: 0.75, y: 1.6, w: 5.8, h: 0.4, fontSize: 16, bold: true, fontFace: THEME.headFontFace });
  txt(s, "P1h = Area x NPP x index(t) x disturbance(t) x quality^e x food-web fraction x C:WW", { x: 0.75, y: 2.05, w: 5.8, h: 0.75, fontSize: 15, color: HEX.accent1, bold: true });
  bullets(s, [
    "Net (not gross) production; TTE applies to net PP",
    "Any literature unit: g C, mmol O2, g dry weight, kg C/ha, organic matter",
    "Pelagic NPP from satellite ensemble (VGPM, Eppley-VGPM) or CMIP6 / MOM6",
    "Food-web fraction removes export, burial and microbial respiration",
    "1 g C = 9 g wet weight (Pauly & Christensen 1995)",
  ], { x: 0.75, y: 2.9, w: 5.75, h: 3.6, fontSize: 14 });
  const ppRows = [
    ["Habitat", "Net PP (as published)", "t C/km2/yr"],
    ["Coral reef", "986-4108 t C (St. Croix)", "1500"],
    ["Colonized pavement", "181 mmol O2/m2/d", "793"],
    ["Seagrass", "4.2 g DW/m2/d", "521"],
    ["Macroalgae", "582 mmol O2/m2/d", "2549"],
    ["Sand (microphytobenthos)", "7 mmol O2/m2/d", "31"],
    ["Mangrove", "400 mmol O2/m2/d", "1752"],
    ["Pelagic", "85 g C/m2/yr", "85"],
  ];
  s.addTable(ppRows.map((r, i) => r.map((c) => ({ text: c, options: { bold: i === 0, color: i === 0 ? "FFFFFF" : HEX.dk1, fill: { color: i === 0 ? HEX.dk2 : (i % 2 ? "FFFFFF" : HEX.lt2) } } }))),
    { x: 7.0, y: 1.45, w: 5.8, colW: [2.2, 2.4, 1.2], fontFace: THEME.bodyFontFace, fontSize: 12, border: { type: "solid", pt: 0.5, color: "D5E3E5" }, rowH: 0.42, objectName: nm("pptable") });
  txt(s, "Sources: Adey & Steneck 1985; Naumann et al. 2013; Juman 2005; Wanders 1976; Gattuso et al. 1998; SEDAR103-DW-20 App. A", { x: 7.0, y: 5.05, w: 5.8, h: 0.6, fontSize: 11, color: "6B7F87" });

  // ENERGY POOLS
  s = content("Energy climbs trophic pools; catch is removed where it was produced", S1);
  const tl = [["TL4", "Piscivores' prey production"], ["TL3", "Invertivores and small fish"], ["TL2", "Grazers, detritivores, zooplankton"], ["TL1", "Algae, seagrass, phytoplankton, detritus"]];
  ["Benthic (per habitat)", "Pelagic"].forEach((lab, j) => {
    const x0 = 0.6 + j * 3.6;
    txt(s, lab, { x: x0, y: 1.4, w: 3.2, h: 0.4, fontSize: 16, bold: true, align: "center", fontFace: THEME.headFontFace });
    tl.forEach((t, i) => {
      const w = 1.6 + i * 0.5, x = x0 + (3.2 - w) / 2, y = 1.95 + i * 1.12;
      s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y, w, h: 0.8, rectRadius: 0.08, fill: { color: [HEX.accent2, HEX.accent3, HEX.accent4, j ? HEX.accent4 : HEX.accent5][i] }, line: { color: "FFFFFF" }, objectName: nm("tl") });
      txt(s, t[0], { x, y: y + 0.05, w, h: 0.35, fontSize: 15, bold: true, align: "center", color: HEX.dk1 });
      txt(s, t[1], { x, y: y + 0.4, w, h: 0.35, fontSize: 11, align: "center", color: HEX.dk1 });
    });
  });
  card(s, 7.9, 1.45, 4.9, 5.25);
  txt(s, "Transfer and feedback", { x: 8.15, y: 1.6, w: 4.4, h: 0.4, fontSize: 18, bold: true, fontFace: THEME.headFontFace });
  txt(s, "E(k+1) = TTE x [ E(k) - catch(k, t-1) ]", { x: 8.15, y: 2.1, w: 4.4, h: 0.45, fontSize: 16, bold: true, color: HEX.accent1 });
  bullets(s, [
    "TTE 15% (10-20% sensitivity), per pathway",
    "Fractional trophic levels from diet: TL = 1 + sum(diet x pool TL)",
    "Last year's catch is removed from the pool it was produced in, so fishing prey lowers predator energy (cascade)",
    "Production of all consumers of a pool sums exactly to TTE x pool: energy is conserved (unit-tested)",
  ], { x: 8.15, y: 2.7, w: 4.45, h: 3.9, fontSize: 14 });

  // PARTITIONING
  s = content("Each pool is shared by demand: diet x habitat preference x weight", S1);
  txt(s, "share(i,h,k) = w(i) D(i,k) pref(i,h) / sum over consumers", { x: 0.5, y: 1.35, w: 12.3, h: 0.45, fontSize: 18, bold: true, color: HEX.accent1 });
  const pm = [
    ["lock", "fixed_biomass", "Calibrated so unfished biomass equals an assessment or survey B0"],
    ["fish", "biomass", "Survey or Ecopath density (t/km2) x P/B"],
    ["net", "catch_proportion", "Share in non-selective gear catch among same-TL species"],
    ["clip", "landings_proportion", "Share of reported landings (backup)"],
    ["random", "uniform_pb", "No information: split by turnover (P/B)"],
    ["edit", "weight", "Expert-specified weights"],
  ];
  pm.forEach((p, i) => {
    const col = i % 3, row = Math.floor(i / 3);
    const x = 0.5 + col * 4.15, y = 2.05 + row * 1.75;
    card(s, x, y, 3.95, 1.55);
    iconCircle(s, p[0], x + 0.25, y + 0.3, 0.6, row ? HEX.accent4 : HEX.accent1);
    txt(s, p[1], { x: x + 1.05, y: y + 0.22, w: 2.75, h: 0.4, fontSize: 16, bold: true, fontFace: "Courier New" });
    txt(s, p[2], { x: x + 1.05, y: y + 0.65, w: 2.75, h: 0.8, fontSize: 13, color: "34474F" });
  });
  txt(s, "Methods mix freely across groups (SEDAR 103 slide 9). Non-fishery competitors (zooplankton, benthic grazers, detritivores, predatory invertebrates, small reef fish, sharks) take their share, so FMP groups never get energy that supports other taxa.", { x: 0.5, y: 5.7, w: 12.3, h: 0.95, fontSize: 14, color: HEX.dk2 });

  // HABITAT LINKAGE
  s = content("Habitat availability reaches species through three routes", S1);
  const routes = [
    ["sun", "1  Production", "Habitat km2 x habitat-specific NPP sets the energy entering that habitat's pools. Losing seagrass area means less benthic TL1 there."],
    ["map", "2  Access", "Habitat pools are shared only among groups that use the habitat, weighted by preference (e.g., grunts: reef by day, seagrass by night)."],
    ["coral", "3  Survival / refuge", "Habitat quality (coral cover, rugosity) scales carrying capacity: K x quality^refuge. Lobster and groupers lose capacity as structure declines."],
  ];
  routes.forEach((r, i) => {
    const x = 0.5 + i * 4.15;
    card(s, x, 1.5, 3.95, 3.45);
    iconCircle(s, r[0], x + 0.3, 1.8, 0.75, [HEX.accent3, HEX.accent1, HEX.accent2][i]);
    txt(s, r[1], { x: x + 0.3, y: 2.75, w: 3.4, h: 0.45, fontSize: 18, bold: true, fontFace: THEME.headFontFace });
    txt(s, r[2], { x: x + 0.3, y: 3.25, w: 3.4, h: 1.6, fontSize: 14, color: "34474F" });
  });
  card(s, 0.5, 5.2, 12.3, 1.5, "FDE3E0");
  iconCircle(s, "coral", 0.8, 5.5, 0.6, HEX.accent2);
  txt(s, "Coral cover is a state variable: observed where data exist; otherwise logistic growth minus bleaching (DHW above 4 degC-weeks), disease (e.g., SCTLD) and hurricane losses.", { x: 1.65, y: 5.45, w: 10.9, h: 1.1, fontSize: 15, color: HEX.dk1 });

  // DYNAMICS & MMSY
  s = content("MMSY = x M K: a defensible cap, not total production", S1);
  card(s, 0.5, 1.45, 6.0, 5.25);
  txt(s, "Biomass dynamics", { x: 0.8, y: 1.6, w: 5.4, h: 0.4, fontSize: 18, bold: true, fontFace: THEME.headFontFace });
  txt(s, "dB/dt = (1-phi) r B (1-B/K) + phi 2xM (K s - B) - C - G + I", { x: 0.8, y: 2.05, w: 5.4, h: 0.4, fontSize: 14, bold: true, color: HEX.accent1 });
  bullets(s, [
    "K(t) = production x environment / M x refuge quality x nursery index",
    "r = 4 x M: Schaefer form consistent with the reference point",
    "Open recruitment, ghost fishing and no-take areas in the dynamics",
    "History driven by landings (x expansion); projections by effort with estimated catchability",
    "Annual process error; immigration hook for connectivity",
  ], { x: 0.8, y: 2.6, w: 5.45, h: 4.0, fontSize: 14 });
  card(s, 6.8, 1.45, 6.0, 5.25, "FDE3E0");
  txt(s, "Which x?", { x: 7.1, y: 1.6, w: 5.4, h: 0.4, fontSize: 18, bold: true, fontFace: THEME.headFontFace });
  stat(s, "x M B0", "Gulland (1971) form on natural mortality; x from per-recruit theory", 7.1, 2.1, 5.4, HEX.accent2);
  bullets(s, [
    "Total production (x = 1) also feeds natural mortality and predators: an upper bound, not MSY",
    "Per-recruit theory gives x = 0.17-0.31 for St. Croix groups; 0.5 is an upper bound (Beddington & Cooke 1983)",
    "Reported: MMSY quantiles, OFL proxy (median), P* target (40th percentile), P(overfishing), B/BMSY",
  ], { x: 7.1, y: 3.6, w: 5.45, h: 3.0, fontSize: 14 });

  // PERTURBATIONS
  s = content("Ocean, habitat, disturbance and fishery conditions drive the model", S1);
  const pert = [
    ["thermo", "Covariate responses", "Linear, log-linear, threshold or thermal-performance curves on production or K"],
    ["hurricane", "Hurricanes", "Immediate loss of NPP, coral, trap effort and production with exponential recovery"],
    ["coral", "Bleaching and disease", "DHW-driven coral mortality; SCTLD-type disease scenarios"],
    ["map", "Habitat change", "Area trends (seagrass loss, mangrove change) and quality indices"],
    ["globe", "Climate projections", "CMIP6 / MOM6 SST and NPP via cov_from scenarios (SSP2-4.5, SSP5-8.5)"],
    ["ship", "Fishery change", "Effort multipliers, trap reduction, closures, prohibited stocks"],
  ];
  pert.forEach((p, i) => {
    const col = i % 3, row = Math.floor(i / 3);
    const x = 0.5 + col * 4.15, y = 1.5 + row * 2.55;
    card(s, x, y, 3.95, 2.3);
    iconCircle(s, p[0], x + 0.3, y + 0.3, 0.7, row ? HEX.accent1 : HEX.accent2);
    txt(s, p[1], { x: x + 1.2, y: y + 0.4, w: 2.6, h: 0.5, fontSize: 17, bold: true, fontFace: THEME.headFontFace });
    txt(s, p[2], { x: x + 0.3, y: y + 1.15, w: 3.4, h: 1.05, fontSize: 13, color: "34474F" });
  });

  // UNCERTAINTY & SIR
  s = content("Uncertainty is propagated end to end and conditioned on data", S1);
  const stg = [
    ["random", "Stage 1  Prior", `${fmt(NPRIOR)} draws in adaptive importance-sampling rounds; Baseline history simulated in parallel`],
    ["filter", "Stage 2  SIR", "Weights from assessment OFLs and catch feasibility for trap focus groups, corrected for the proposal mixture; resample"],
    ["layers", "Stage 3  Scenarios", "All scenarios run for the retained draws only (paired, memory-light)"],
  ];
  stg.forEach((t, i) => {
    const x = 0.5 + i * 4.15;
    card(s, x, 1.45, 3.95, 2.3);
    iconCircle(s, t[0], x + 0.3, 1.7, 0.65, [HEX.accent1, HEX.accent2, HEX.accent4][i]);
    txt(s, t[1], { x: x + 1.1, y: 1.8, w: 2.7, h: 0.45, fontSize: 17, bold: true, fontFace: THEME.headFontFace });
    txt(s, t[2], { x: x + 0.3, y: 2.55, w: 3.45, h: 1.1, fontSize: 13, color: "34474F" });
    if (i < 2) arrow(s, x + 3.97, 2.6, x + 4.13, 2.6);
  });
  txt(s, "Sampled uncertainty", { x: 0.5, y: 4.0, w: 6, h: 0.4, fontSize: 16, bold: true, fontFace: THEME.headFontFace });
  bullets(s, [
    "Habitat area, NPP, food-web fraction, TTE, C:WW",
    "P/B, M, x_msy, partition weights, initial depletion, open recruitment, diet (Dirichlet)",
    "Covariate observation error (API spatial / ensemble SD)",
    "Landings, effort and expansion-factor errors; hurricane intensity",
  ], { x: 0.5, y: 4.45, w: 6.0, h: 2.3, fontSize: 13 });
  txt(s, "Likelihood components", { x: 6.9, y: 4.0, w: 6, h: 0.4, fontSize: 16, bold: true, fontFace: THEME.headFontFace });
  bullets(s, [
    "Indices (CPUE, surveys): trend shape, analytical q; role fit or held out to validate",
    "Assessment OFLs (SEDAR 57, 80): absolute scale",
    "Catch feasibility (stock reduction analysis): stock must supply every year's landings",
  ], { x: 6.9, y: 4.45, w: 5.9, h: 2.3, fontSize: 13 });

  // ROBUSTNESS TOOLKIT
  s = content("Built for review: calibration is tested, not just performed", S1);
  const rob = [
    ["filter", "Adaptive importance sampling", "Factorized per-group proposals re-centred only for conflicted groups; deterministic-mixture weights (Owen & Zhou 2000); ESS, Pareto k, shrinkage reported"],
    ["flask", "Simulation self-test", "Simulate data from a known draw, refit, and score bias and interval coverage"],
    ["check", "Out-of-sample validation", "Indices held out by role; leave-one-benchmark-out; retrospective peel"],
    ["chart", "Global sensitivity", "Rank regression (SRRC, PRCC) of MMSY on every sampled input"],
    ["diagram", "Structural ensemble", "x_msy, TTE, food-web fraction, thermal and fishing feedbacks, pelagic NPP source; equal-weight pooling"],
    ["globe", "Ecosystem overfishing", "Ryther and Fogarty indices (Link & Watson 2019) against published thresholds"],
  ];
  rob.forEach((o, i) => {
    const col = i % 2, row = Math.floor(i / 2);
    const x = 0.5 + col * 6.2, y = 1.45 + row * 1.75;
    card(s, x, y, 6.0, 1.55);
    iconCircle(s, o[0], x + 0.25, y + 0.42, 0.65, col ? HEX.accent1 : HEX.accent6);
    txt(s, o[1], { x: x + 1.1, y: y + 0.2, w: 4.7, h: 0.42, fontSize: 17, bold: true, fontFace: THEME.headFontFace });
    txt(s, o[2], { x: x + 1.1, y: y + 0.65, w: 4.7, h: 0.8, fontSize: 13, color: "34474F" });
  });

  // THEORY REVIEW
  s = content("A theory review added the processes ecologists expect", S1);
  const thy = [
    ["fish", "Natural-mortality basis", "K = P/M and MMSY = x M K: Gulland on M, not on Ecopath P/B (= Z)"],
    ["chart", "Life-history x priors", "Per-recruit MSY/(M B0) from growth, maturity, trap selectivity, steepness"],
    ["globe", "Open recruitment", "Imported larvae (spiny lobster): local depletion cannot collapse supply"],
    ["leaf", "Nursery habitats", "Seagrass / mangrove / macroalgae area x quality scales capacity"],
    ["coral", "Herbivory feedback", "Parrotfish and surgeonfish grazing sets coral recovery"],
    ["lock", "No-take areas", "Only the fishable share of each stock supplies landings"],
    ["net", "Ghost fishing", "Traps lost in hurricanes keep fishing, decaying over time"],
    ["wave", "Lionfish and Sargassum", "Satellite Sargassum index (AFAI); lionfish invasion effects"],
  ];
  thy.forEach((o, i) => {
    const col = i % 2, row = Math.floor(i / 2);
    const x = 0.5 + col * 6.2, y = 1.4 + row * 1.33;
    card(s, x, y, 6.0, 1.2);
    iconCircle(s, o[0], x + 0.22, y + 0.28, 0.62, col ? HEX.accent1 : HEX.accent5);
    txt(s, o[1], { x: x + 1.05, y: y + 0.14, w: 4.8, h: 0.4, fontSize: 16, bold: true, fontFace: THEME.headFontFace });
    txt(s, o[2], { x: x + 1.05, y: y + 0.56, w: 4.8, h: 0.55, fontSize: 12, color: "34474F" });
  });
  txt(s, "Each process can be switched off and is a variant in the structural ensemble. Details: docs/ecological_review.md", { x: 0.5, y: 6.75, w: 12.3, h: 0.3, fontSize: 12, italic: true, color: "6B7F87" });

  // DECISION TOOLS
  s = content("Decision support: triangulate, prioritize data, test the harvest rule", S1);
  const dtl = [
    ["scale", "Independent comparator", "Catch-only surplus-production MSY (CMSY-style) shares no energy-budget assumption with the TTM"],
    ["target", "Value of information", "Share of MMSY variance each data stream could remove, so data collection is ranked by payoff"],
    ["recycle", "Closed-loop MSE", "SIR-conditioned operating models test status quo, trap cuts, the SPF effort-ceiling rule (unbiased and biased) and a catch-rate rule"],
  ];
  dtl.forEach((t, i) => {
    const x = 0.5 + i * 4.15;
    card(s, x, 1.45, 3.95, 4.9);
    iconCircle(s, t[0], x + 0.3, 1.7, 0.72, [HEX.accent1, HEX.accent2, HEX.accent4][i]);
    txt(s, t[1], { x: x + 0.3, y: 2.65, w: 3.4, h: 0.5, fontSize: 18, bold: true, fontFace: THEME.headFontFace });
    txt(s, t[2], { x: x + 0.3, y: 3.25, w: 3.4, h: 2.8, fontSize: 14, color: "34474F" });
  });

  // DATA ACQUISITION
  s = content("Public-API inputs are acquired automatically; user values win", S1);
  const apiRows = [
    ["Input", "Source (API)", "Uncertainty"],
    ["SST", "Coral Reef Watch monthly; Pathfinder (ERDDAP)", "spatial SD"],
    ["Degree heating weeks", "Coral Reef Watch (ERDDAP)", "spatial SD"],
    ["Pelagic NPP index", "VGPM + Eppley-VGPM from OC-CCI chl, SST, PAR", "algorithm spread"],
    ["Climate projections", "CMIP6 CESM2 + ACCESS-ESM1-5 (ERDDAP)", "between-model SD"],
    ["Habitat areas", "NOAA NCCOS benthic habitat maps (ArcGIS REST)", "class-mapping CV"],
    ["Deeper shelf", "ETOPO 2022 depth + Kd490 + PAR, light-limited NPP", "CV 0.6"],
    ["Species traits", "FishBase (M, Linf, TL, thermal range)", "between-species SD"],
    ["Hurricanes", "NOAA NHC HURDAT2 best tracks", "intensity CV"],
    ["Optional", "MOM6-NWA12 (THREDDS), GLORYS12 (Copernicus)", "spatial SD"],
  ];
  s.addTable(apiRows.map((r, i) => r.map((c) => ({ text: c, options: { bold: i === 0, color: i === 0 ? "FFFFFF" : HEX.dk1, fill: { color: i === 0 ? HEX.dk2 : (i % 2 ? "FFFFFF" : HEX.lt2) } } }))),
    { x: 0.5, y: 1.45, w: 8.0, colW: [2.0, 4.3, 1.7], fontFace: THEME.bodyFontFace, fontSize: 12, border: { type: "solid", pt: 0.5, color: "D5E3E5" }, rowH: 0.45, objectName: nm("apitable") });
  card(s, 8.85, 1.45, 3.95, 5.25);
  iconCircle(s, "edit", 9.15, 1.7, 0.65, HEX.accent2);
  txt(s, "Override rules", { x: 9.95, y: 1.82, w: 2.7, h: 0.45, fontSize: 18, bold: true, fontFace: THEME.headFontFace });
  bullets(s, [
    "Covariates: user rows win, except rows flagged PLACEHOLDER",
    "Habitats: API fills only blank area / NPP cells",
    "Disturbances: API events only for years the user did not list",
    "use_acquired = FALSE ignores API data",
    "Every request logged with time and source",
  ], { x: 9.15, y: 2.6, w: 3.45, h: 4.0, fontSize: 14 });

  // OUTPUTS FOR SPF
  s = content("Outputs are SPF-ready tables for the frontier analysis", S1);
  const outs = [
    ["target", "Group reference points", "MMSY quantiles (t, lb), OFL proxy, P* target, B/BMSY, EU, P(overfishing), status"],
    ["layers", "Guild and complex MMSY", "Draw-level sums keep covariance among groups"],
    ["table", "Gear targeting matrix", "Catch composition W(g,i) and share of each group's catch"],
    ["net", "Gear MMSY", "MMSY(g,i) = MMSY(i) x share of catch (or slide-11 rule)"],
    ["scale", "Effort ceilings", "m(g) = min over groups of MMSY(g,i) / C(g,i); choke group and its probability"],
    ["db", "Full distribution", "Draw-level MMSY table for propagation in the MSE"],
  ];
  outs.forEach((o, i) => {
    const col = i % 2, row = Math.floor(i / 2);
    const x = 0.5 + col * 6.2, y = 1.45 + row * 1.75;
    card(s, x, y, 6.0, 1.55);
    iconCircle(s, o[0], x + 0.25, y + 0.42, 0.65, col ? HEX.accent1 : HEX.accent4);
    txt(s, o[1], { x: x + 1.1, y: y + 0.2, w: 4.7, h: 0.42, fontSize: 17, bold: true, fontFace: THEME.headFontFace });
    txt(s, o[2], { x: x + 1.1, y: y + 0.65, w: 4.7, h: 0.8, fontSize: 13, color: "34474F" });
  });

  // DATA NEEDS
  s = content("Data needs: what to replace before results inform management", S1);
  const needs = [
    ["ship", "A  Fishery-dependent", "Landings by gear x group (1983-), effort, expansion factor, gear splits", "High", HEX.accent2],
    ["clip", "B  Assessments", "SEDAR 57 / 80 / 84 indices, B0, natural mortality", "High", HEX.accent2],
    ["fish", "C  Surveys and biology", "NCRMP biomass densities (pipeline ready), thermal response for lobster, habitat use", "High", HEX.accent2],
    ["map", "D  Habitat and production", "Pavement production, food-web fraction, TTE, early coral cover", "High", HEX.accent2],
    ["thermo", "E  Driver effects", "Thermal sensitivities, hurricane losses, bleaching frequency", "Medium", HEX.accent3],
    ["cloud", "F  Automated already", "SST, DHW, NPP, CMIP6, habitat areas, hurricanes", "Done", HEX.accent5],
  ];
  needs.forEach((n, i) => {
    const col = i % 3, row = Math.floor(i / 3);
    const x = 0.5 + col * 4.15, y = 1.45 + row * 2.6;
    card(s, x, y, 3.95, 2.4);
    iconCircle(s, n[0], x + 0.3, y + 0.3, 0.65, n[4]);
    txt(s, n[1], { x: x + 1.1, y: y + 0.38, w: 2.7, h: 0.5, fontSize: 16, bold: true, fontFace: THEME.headFontFace });
    txt(s, n[2], { x: x + 0.3, y: y + 1.1, w: 3.4, h: 0.9, fontSize: 13, color: "34474F" });
    txt(s, "Priority: " + n[3], { x: x + 0.3, y: y + 1.95, w: 3.4, h: 0.3, fontSize: 12, bold: true, color: n[4] === HEX.accent3 ? "B07A1E" : n[4] });
  });
  txt(s, "Full itemized list with files, columns and suggested sources: docs/data_needs.md", { x: 0.5, y: 6.65, w: 12.3, h: 0.35, fontSize: 12, italic: true, color: "6B7F87" });

  // =========================================================================
  // PART 2
  pres.addSection({ title: S2 });
  s = pres.addSlide({ masterName: "SECTION_DARK", sectionTitle: S2 });
  s.addText("Part 2  |  St. Croix trap fishery", { placeholder: "title" });
  s.addText("Real public data where available, flagged placeholders elsewhere: results illustrate the method, not management advice", { placeholder: "body" });

  // DOMAIN & HABITAT (native chart)
  s = content("St. Croix: an isolated shelf dominated by colonized hardbottom", S2);
  const habOrder = hab.slice().sort((a, b) => num(b.area_km2) - num(a.area_km2));
  const habLab = { colonized_pavement: "Colonized pavement", coral_reef: "Coral reef", deep_hardbottom: "Deeper shelf 30-100 m", seagrass: "Seagrass", macroalgae: "Macroalgae", sand: "Sand / mud", mangrove: "Mangrove" };
  s.addChart(pres.charts.BAR, [{ name: "Area (km2)", labels: habOrder.map((h) => habLab[h.habitat] || h.habitat), values: habOrder.map((h) => Math.round(num(h.area_km2) * 10) / 10) }], {
    x: 0.5, y: 1.4, w: 7.4, h: 5.3, barDir: "bar", chartColors: [HEX.accent1], showValue: true, dataLabelPosition: "outEnd", dataLabelFormatCode: "#,##0.0",
    dataLabelFontFace: "+mn-lt", dataLabelFontSize: 12, dataLabelColor: "34474F",
    catAxisLabelFontFace: "+mn-lt", catAxisLabelFontSize: 12, catAxisLabelColor: "34474F", catAxisOrientation: "maxMin",
    valAxisHidden: true, valGridLine: { style: "none" }, catGridLine: { style: "none" }, showLegend: false,
    showTitle: true, title: "Benthic habitat area (km2), NOAA NCCOS map + ETOPO via API", titleFontFace: "+mn-lt", titleFontSize: 14, titleColor: "34474F",
  });
  card(s, 8.3, 1.4, 4.5, 5.3);
  stat(s, fmt(habOrder.reduce((a, h) => a + num(h.area_km2), 0)) + " km2", "benthic habitat in the model domain", 8.6, 1.6, 4.0);
  bullets(s, [
    "Single FMP unit, separated by deep trenches from the PR-VI platform",
    "Pelagic area: shelf + 10 km buffer (1,500 km2)",
    "1983-2024 history (SEDAR 80 landings start); 2025-2045 projections",
    "19 FMP groups + 10 competitor groups; 13 trap groups + 2 assessed stocks calibrated",
  ], { x: 8.6, y: 3.15, w: 4.0, h: 3.4, fontSize: 14 });

  // ENVIRONMENT
  s = content("Four decades of warming, bleaching and coral loss drive the model", S2);
  figure(s, "fig09_environment.png", 0.5, 1.35, 8.3, 5.0, "Environmental drivers 1983-2024");
  card(s, 9.1, 1.35, 3.7, 5.3);
  bullets(s, [
    "SST up ~1.5 degC since the 1980s; 2024 record warm",
    "DHW: 2005 = 11.7, 2023 = 16.7, 2024 = 21.3 degC-weeks",
    "Pelagic NPP index varies +/-30%; 2009-10 Orinoco/Amazon plume years",
    "Coral cover ~22% (placeholder) to ~3% in 2024",
  ], { x: 9.35, y: 1.6, w: 3.3, h: 4.9, fontSize: 14 });

  // HURRICANES
  s = content("Hurricane disturbances come straight from NHC best tracks", S2);
  figure(s, "fig10_hurricanes.png", 0.5, 1.35, 12.3, 4.4, "Hurricane intensity timeline");
  txt(s, "Intensity = (max wind - 34 kt)/(137 - 34 kt) x distance decay; losses to NPP, coral cover, trap effort and lobster production scale with intensity and recover with half-lives of 1.5-2 years. Each event carries a 30% intensity CV.", { x: 0.5, y: 5.95, w: 12.3, h: 0.8, fontSize: 14, color: HEX.dk2 });

  // ENERGY BUDGET
  s = content("Colonized pavement supplies most benthic energy", S2);
  figure(s, "fig01_energy_budget.png", 0.5, 1.35, 8.0, 4.6, "Primary production by habitat");
  card(s, 8.8, 1.35, 4.0, 5.3);
  bullets(s, [
    "Large area beats high per-area rates: pavement outweighs coral reef",
    "Deeper shelf re-mapped from ETOPO: 41 km2 at 30-100 m with light-limited NPP (the 388 km2 unmapped class was mostly off-shelf)",
    "Bars show the 90% interval across draws",
  ], { x: 9.05, y: 1.6, w: 3.5, h: 4.9, fontSize: 14 });

  // SIR
  s = content("Adaptive SIR anchors scale on assessed stocks and rejects infeasible draws", S2);
  figure(s, "fig08_sir.png", 0.5, 1.35, 8.3, 4.6, "Prior vs SIR posterior MMSY");
  card(s, 9.1, 1.35, 3.7, 5.3);
  stat(s, fmt(NPRIOR), "prior Monte Carlo draws", 9.35, 1.55, 3.3);
  stat(s, isNaN(ESS) ? "-" : fmt(ESS), "effective sample size after SIR", 9.35, 3.05, 3.3, HEX.accent2);
  txt(s, `${fmt(NUNIQ)} unique draws; largest weight ${fmt(100 * num(diagS.max_weight), 1)}%. Dashed lines: SEDAR OFLs. Placeholder indices are held out for validation, not fitted.`, { x: 9.35, y: 4.6, w: 3.3, h: 1.9, fontSize: 13, color: "34474F" });

  // GROUP MMSY
  s = content("Group MMSY with current catch and management benchmarks", S2);
  figure(s, "fig02_group_mmsy.png", 0.5, 1.35, 7.6, 5.55, "Group MMSY intervals");
  card(s, 8.4, 1.35, 4.4, 5.3);
  stat(s, fmtK(num(lob.MMSY_p50_lb)) + " lb", "spiny lobster median MMSY (SEDAR 57 OFL 164k lb)", 8.65, 1.5, 4.0);
  stat(s, fmtK(num(qt.MMSY_p50_lb)) + " lb", "queen triggerfish median MMSY (SEDAR 80 OFL 23k lb)", 8.65, 3.0, 4.0, HEX.accent2);
  txt(s, "Unassessed complexes are bounded below by catch feasibility and above by shared energy. Conch, yellowtail, deep snappers, jacks and pelagics are outside the trap focus and are not calibrated.", { x: 8.65, y: 4.6, w: 4.0, h: 1.9, fontSize: 13, color: "34474F" });

  // GUILD (native chart)
  s = content("Guild-level MMSY compared with current (expanded) catch", S2);
  const gd = guild.slice().sort((a, b) => num(b.MMSY_p50_lb) - num(a.MMSY_p50_lb));
  s.addChart(pres.charts.BAR, [
    { name: "MMSY median (lb)", labels: gd.map((r) => r.name), values: gd.map((r) => Math.round(num(r.MMSY_p50_lb))) },
    { name: "P* target (lb)", labels: gd.map((r) => r.name), values: gd.map((r) => Math.round(num(r.target_t) * 2204.62)) },
    { name: "Current catch (lb)", labels: gd.map((r) => r.name), values: gd.map((r) => Math.round(num(r.catch_lb))) },
  ], {
    x: 0.5, y: 1.4, w: 8.4, h: 5.3, barDir: "col", barGrouping: "clustered", chartColors: [HEX.accent1, HEX.accent4, HEX.accent2],
    catAxisLabelFontFace: "+mn-lt", catAxisLabelFontSize: 12, catAxisLabelColor: "34474F",
    valAxisLabelFontFace: "+mn-lt", valAxisLabelFontSize: 11, valAxisLabelColor: "34474F", valAxisLabelFormatCode: "#,##0",
    valGridLine: { color: "E3ECEE", size: 0.5 }, catGridLine: { style: "none" },
    showLegend: true, legendPos: "t", legendFontFace: "+mn-lt", legendFontSize: 12, legendColor: "34474F",
  });
  card(s, 9.2, 1.4, 3.6, 5.3);
  stat(s, fmtK(calMMSY) + " lb", "summed median MMSY, calibrated trap groups", 9.45, 1.6, 3.2);
  stat(s, fmtK(calCatch) + " lb", "their current expanded catch", 9.45, 3.1, 3.2, HEX.accent2);
  txt(s, "Guild sums are computed draw by draw, so uncertainty reflects shared energy among groups.", { x: 9.45, y: 4.6, w: 3.2, h: 1.9, fontSize: 13, color: "34474F" });

  // SCENARIOS
  s = content("Scenario stoplight: catch relative to MMSY in 2043-2045", S2);
  figure(s, "fig03_scenario_status.png", 0.5, 1.3, 9.0, 5.6, "Scenario status heat map");
  card(s, 9.8, 1.35, 3.0, 5.3);
  bullets(s, [
    `SSP5-8.5 cuts Parrotfish 2 MMSY ${fmt(100 * (1 - num(sc("Climate_SSP585", "parrotfish_2").MMSY_p50_lb) / num(sc("Baseline", "parrotfish_2").MMSY_p50_lb)))}% and queen triggerfish ${fmt(100 * (1 - num(sc("Climate_SSP585", "queen_triggerfish").MMSY_p50_lb) / num(sc("Baseline", "queen_triggerfish").MMSY_p50_lb)))}% by 2043-45`,
    `Combined stress: queen triggerfish P(overfishing) ${fmt(100 * num(sc("Baseline", "queen_triggerfish").P_overfishing))}% to ${fmt(100 * num(sc("Combined_stress", "queen_triggerfish").P_overfishing))}%`,
    "Lobster has no thermal curve yet: its climate insensitivity is an assumption",
  ], { x: 10.05, y: 1.6, w: 2.6, h: 4.9, fontSize: 13 });

  // TRAJECTORIES
  s = content("Projected stock status under alternative futures", S2);
  figure(s, "fig04_bbmsy_trajectories.png", 0.5, 1.3, 12.3, 5.6, "B/BMSY trajectories");

  // EFFORT CEILINGS
  s = content("Gear effort ceilings and choke groups feed the frontier analysis", S2);
  figure(s, "fig07_effort_ceilings.png", 0.5, 1.35, 8.3, 4.6, "Gear effort ceilings");
  card(s, 9.1, 1.35, 3.7, 5.3);
  stat(s, "x" + fmt(num(lobCeil.effort_multiplier_p50), 2), `lobster-trap effort multiplier; choke: ${lobCeil.choke_group.replace(/_/g, " ")} (${fmt(100 * num(lobCeil.P_choke))}%)`, 9.35, 1.55, 3.3);
  stat(s, "x" + fmt(num(trapCeil.effort_multiplier_p50), 2), `fish-trap effort multiplier; choke: ${trapCeil.choke_group.replace(/_/g, " ")} (${fmt(100 * num(trapCeil.P_choke))}%)`, 9.35, 3.05, 3.3, HEX.accent2);
  txt(s, "Only SIR-calibrated groups can be choke groups. Small, low-MMSY bycatch groups, not lobster, bind first.", { x: 9.35, y: 4.6, w: 3.3, h: 1.9, fontSize: 13, color: "34474F" });

  // ===== THEORY-BASED PRIORS AND DECISION TOOLS =====
  if (lhx.length) {
    s = content("Life-history theory puts x well below Gulland's 0.5", S2);
    const lo = lhx.slice().sort((a, b) => num(a.x_msy) - num(b.x_msy));
    s.addChart(pres.charts.BAR, [{ name: "x = MSY / (M B0)", labels: lo.map((r) => r.group.replace(/_/g, " ")), values: lo.map((r) => Math.round(num(r.x_msy) * 1000) / 1000) }], {
      x: 0.5, y: 1.4, w: 7.6, h: 5.4, barDir: "bar", chartColors: [HEX.accent1], showValue: true, dataLabelPosition: "outEnd", dataLabelFormatCode: "0.00",
      dataLabelFontFace: "+mn-lt", dataLabelFontSize: 10, dataLabelColor: "34474F",
      catAxisLabelFontFace: "+mn-lt", catAxisLabelFontSize: 10, catAxisLabelColor: "34474F",
      valAxisMinVal: 0, valAxisMaxVal: 0.5, valAxisLabelFontFace: "+mn-lt", valAxisLabelFontSize: 10, valAxisLabelColor: "34474F",
      valGridLine: { color: "E3ECEE", size: 0.5 }, catGridLine: { style: "none" }, showLegend: false,
      showTitle: true, title: "Per-recruit x by group (mean of 400 draws)", titleFontFace: "+mn-lt", titleFontSize: 13, titleColor: "34474F",
    });
    card(s, 8.4, 1.4, 4.4, 5.3);
    stat(s, fmt(Math.min(...lhx.map((r) => num(r.x_msy))), 2) + "-" + fmt(Math.max(...lhx.map((r) => num(r.x_msy))), 2), "x across trap and hook groups (Gulland: 0.5)", 8.65, 1.6, 4.0);
    stat(s, fmt(Math.min(...lhx.map((r) => num(r.Bmsy_over_B0))), 2) + "-" + fmt(Math.max(...lhx.map((r) => num(r.Bmsy_over_B0))), 2), "B_MSY / B0, in line with meta-analyses", 8.65, 3.1, 4.0, HEX.accent2);
    txt(s, "FishBase growth and length-weight, maturity from L-infinity, trap retention ~15 cm, steepness 0.75 +/- 0.1. Lobster blends open (h -> 1) and local recruitment.", { x: 8.65, y: 4.6, w: 4.0, h: 1.9, fontSize: 12, color: "34474F" });
  }
  if (mcmp.length) {
    s = content("An independent catch-only model corroborates the assessed stocks", S2);
    figure(s, "fig16_method_comparison.png", 0.5, 1.35, 8.3, 5.3, "TTM vs catch-only MSY");
    card(s, 9.1, 1.35, 3.7, 5.3);
    const mq = mcmp.find((r) => r.group === "queen_triggerfish"), ml = mcmp.find((r) => r.group === "spiny_lobster");
    bullets(s, [
      mq ? `Queen triggerfish: TTM ${fmtK(num(mq.MMSY_p50_lb))}, catch-only ${fmtK(num(mq.CMSY_p50_lb))}, OFL ${fmtK(num(mq.OFL_lb))} lb` : "",
      ml ? `Spiny lobster: TTM ${fmtK(num(ml.MMSY_p50_lb))}, catch-only ${fmtK(num(ml.CMSY_p50_lb))}, OFL ${fmtK(num(ml.OFL_lb))} lb` : "",
      "The catch-only model uses only landings and resilience: agreement is independent corroboration",
    ].filter((x) => x), { x: 9.35, y: 1.6, w: 3.3, h: 4.9, fontSize: 13 });
  }
  if (voiS.length) {
    s = content("Value of information: which data would narrow MMSY most", S2);
    figure(s, "fig17_value_of_information.png", 0.5, 1.35, 8.6, 5.2, "Value of information by data stream");
    card(s, 9.4, 1.35, 3.4, 5.3);
    const top = voiS.slice().sort((a, b) => num(b.S1_sum) - num(a.S1_sum));
    stat(s, fmt(100 * num(top[0].S1_sum)) + "%", `of total focus-MMSY variance: ${top[0].data_stream}`, 9.6, 1.55, 3.0);
    if (top[1]) stat(s, fmt(100 * num(top[1].S1_sum)) + "%", top[1].data_stream, 9.6, 3.15, 3.0, HEX.accent2);
    txt(s, "First-order variance shares from the prior Monte Carlo: the expected uncertainty removed if that input were known exactly.", { x: 9.6, y: 4.75, w: 3.0, h: 1.8, fontSize: 12, color: "34474F" });
  }
  if (mseS.length) {
    s = content("Closed-loop MSE: how the SPF effort-ceiling rule performs", S2);
    figure(s, "fig18_mse.png", 0.5, 1.35, 8.6, 5.2, "Management strategy evaluation");
    card(s, 9.4, 1.35, 3.4, 5.3);
    const g1 = (n) => mseS.find((r) => r.procedure === n);
    bullets(s, [
      g1("Status_quo") ? `Status quo: P(any focus group < 0.5 BMSY) = ${fmt(100 * num(g1("Status_quo").P_any_focus_overfished))}%` : "",
      g1("SPF_ceiling") ? `SPF ceiling rule: ${fmt(100 * num(g1("SPF_ceiling").P_any_focus_overfished))}%, catch ${fmtK(num(g1("SPF_ceiling").focus_catch_lb_p50))} lb` : "",
      g1("SPF_ceiling_biased") ? `With a +50% biased assessment: ${fmt(100 * num(g1("SPF_ceiling_biased").P_any_focus_overfished))}%` : "",
      g1("CPUE_HCR") ? `Catch-rate rule: ${fmt(100 * num(g1("CPUE_HCR").P_any_focus_overfished))}%` : "",
    ].filter((x) => x), { x: 9.6, y: 1.6, w: 3.0, h: 4.9, fontSize: 13 });
  }

  // ===== ROBUSTNESS AND VALIDATION =====
  s = content("Calibration passes a simulation self-test", S2);
  figure(s, "fig11_sensitivity_total_focus_MMSY.png", 0.5, 1.35, 7.4, 5.0, "Global sensitivity of total focus MMSY");
  card(s, 8.2, 1.35, 4.6, 5.3);
  stat(s, `${fmt(Math.min(...cov90), 2)}-${fmt(Math.max(...cov90), 2)}`, "self-test 90% interval coverage across 13 trap groups (100 replicates)", 8.45, 1.55, 4.1);
  stat(s, fmt(num(diagS.ESS)), `effective sample size from ${fmt(num(diagS.n_prior))} prior draws`, 8.45, 3.05, 4.1, HEX.accent2);
  txt(s, "Left: rank-regression sensitivity of total trap-focus MMSY. Pavement TTE and NPP, competitor biomass and food-web fraction dominate. Hogfish and porgies are biased low in the self-test (about -35%), a conservative error for choke groups.", { x: 8.45, y: 4.55, w: 4.1, h: 2.0, fontSize: 12, color: "34474F" });

  s = content("Out-of-sample: queen triggerfish passes, lobster needs its assessment", S2);
  figure(s, "fig13_loo_benchmarks.png", 0.5, 1.35, 7.6, 3.0, "Leave-one-benchmark-out predictions");
  figure(s, "fig12_validation.png", 0.5, 4.45, 7.6, 2.45, "Held-out index validation");
  card(s, 8.4, 1.35, 4.4, 5.3);
  const looQ = loo.find((r) => r.held_out.startsWith("queen")), looL = loo.find((r) => r.held_out.startsWith("spiny"));
  bullets(s, [
    `Queen triggerfish OFL withheld: predicted ${fmtK(num(looQ.pred_p50))} lb (90% ${fmtK(num(looQ.pred_p05))}-${fmtK(num(looQ.pred_p95))}) vs ${fmtK(num(looQ.benchmark_lb))}: inside`,
    `Spiny lobster OFL withheld: predicted ${fmtK(num(looL.pred_p50))} vs ${fmtK(num(looL.benchmark_lb))}: outside. Lobster scale is identified only by assessment information`,
    "Held-out indices are placeholders: the bottom panel demonstrates the machinery, not skill",
  ], { x: 8.65, y: 1.6, w: 4.0, h: 4.9, fontSize: 13 });

  s = content("Structural uncertainty: anchored stocks are robust, unassessed groups are not", S2);
  figure(s, "fig15_structural_ensemble.png", 0.5, 1.35, 8.3, 5.3, "Structural ensemble MMSY by variant");
  card(s, 9.1, 1.35, 3.7, 5.3);
  stat(s, fmt(num(ensG("spiny_lobster").between_variant_cv), 2) + " / " + fmt(num(ensG("queen_triggerfish").between_variant_cv), 2), "between-variant CV: lobster / queen triggerfish", 9.35, 1.55, 3.3);
  stat(s, fmt(num(ensG("parrotfish_2").between_variant_cv), 2), "between-variant CV: Parrotfish 2", 9.35, 3.05, 3.3, HEX.accent2);
  txt(s, "10 variants: x_msy 0.3/0.4, TTE 0.10/0.20, food-web fraction x0.67/x1.5, no thermal effects, no fishing feedback, satellite pelagic NPP.", { x: 9.35, y: 4.6, w: 3.3, h: 1.9, fontSize: 12, color: "34474F" });

  s = content("Ecosystem-level overfishing indicators", S2);
  figure(s, "fig14_eco_indices.png", 0.5, 1.35, 8.3, 5.3, "Ryther and Fogarty indices");
  card(s, 9.1, 1.35, 3.7, 5.3);
  stat(s, fmt(Math.min(...ecoLast.map((r) => num(r.ryther_p50))), 2) + "-" + fmt(Math.max(...ecoLast.map((r) => num(r.ryther_p50))), 2), "Ryther index 2022-24 (t/km2/yr): below the 1 t threshold", 9.35, 1.55, 3.3, HEX.accent5);
  stat(s, fmt(Math.min(...ecoLast.map((r) => num(r.fogarty_permil_p50))), 2) + "-" + fmt(Math.max(...ecoLast.map((r) => num(r.fogarty_permil_p50))), 2), "Fogarty index (per mil): inside the 0.22-0.92 caution band", 9.35, 3.05, 3.3, HEX.accent3);
  txt(s, "Link & Watson (2019). Catch relative to area and to primary production.", { x: 9.35, y: 4.6, w: 3.3, h: 1.9, fontSize: 12, color: "34474F" });

  s = content("Review readiness: what is addressed and what still needs data", S2);
  const rr = [
    ["check", "Addressed", HEX.accent5, ["Degenerate SIR (ESS 1 to 86)", "Input-uncertainty propagation", "Self-test coverage and bias", "Global sensitivity", "Structural ensemble", "Status of uncalibrated groups", "Reproducible parallel runs"]],
    ["warn", "Depends on placeholders", HEX.accent3, ["Ecopath / FishBase priors", "Gear choke groups", "Scenario magnitudes", "Ecosystem indices"]],
    ["clip", "Needs data", HEX.accent2, ["SEDAR 57 / 80 CPUE as fit indices", "Real landings and effort by gear", "NCRMP densities", "Lobster thermal response"]],
  ];
  rr.forEach((c, i) => {
    const x = 0.5 + i * 4.15;
    card(s, x, 1.45, 3.95, 5.25);
    iconCircle(s, c[0], x + 0.3, 1.7, 0.65, c[2]);
    txt(s, c[1], { x: x + 1.1, y: 1.82, w: 2.75, h: 0.45, fontSize: 17, bold: true, fontFace: THEME.headFontFace });
    bullets(s, c[3], { x: x + 0.3, y: 2.6, w: 3.45, h: 3.9, fontSize: 13 });
  });
  txt(s, "Critique-by-critique responses and evidence files: docs/review_readiness.md", { x: 0.5, y: 6.75, w: 12.3, h: 0.3, fontSize: 12, italic: true, color: "6B7F87" });

  // CAVEATS
  s = content("What is real, what is placeholder", S2);
  card(s, 0.5, 1.45, 6.0, 5.25, "E3F2EC");
  iconCircle(s, "check", 0.8, 1.7, 0.65, HEX.accent5);
  txt(s, "Real public data", { x: 1.6, y: 1.82, w: 4.7, h: 0.45, fontSize: 18, bold: true, fontFace: THEME.headFontFace });
  bullets(s, [
    "Habitat areas (NOAA NCCOS); deeper shelf (ETOPO)",
    "SST, DHW, chlorophyll, PAR, computed NPP (satellite)",
    "CMIP6 SST and NPP projections",
    "Hurricane tracks (HURDAT2)",
    "SEDAR 57 / 80 OFLs; FMP ACLs; queen conch 2021 landings",
    "Ecopath (Opitz 1996) and FishBase priors",
  ], { x: 0.8, y: 2.6, w: 5.5, h: 4.0, fontSize: 14 });
  card(s, 6.8, 1.45, 6.0, 5.25, "FDE3E0");
  iconCircle(s, "warn", 7.1, 1.7, 0.65, HEX.accent2);
  txt(s, "Placeholders (flagged)", { x: 7.9, y: 1.82, w: 4.7, h: 0.45, fontSize: 18, bold: true, fontFace: THEME.headFontFace });
  bullets(s, [
    "Landings and effort by gear (confidential trip tickets)",
    "Assessment CPUE indices and survey densities",
    "Conch, pelagic and deep-snapper priors; habitat preferences",
    "Pre-2000 coral cover; lobster thermal response",
    "Thermal sensitivities and hurricane loss magnitudes",
  ], { x: 7.1, y: 2.6, w: 5.5, h: 4.0, fontSize: 14 });

  // NEXT STEPS
  s = pres.addSlide({ masterName: "TITLE_DARK", sectionTitle: S2 });
  s.addText("Next steps", { placeholder: "title" });
  s.addText([
    { text: "1. Fit SEDAR 57 / 80 CPUE; keep NCRMP held out for validation", options: { breakLine: true } },
    { text: "2. Load confidential landings and effort by gear (A1-A6)", options: { breakLine: true } },
    { text: "3. Ingest NCRMP densities (pipeline ready) and add a lobster thermal curve", options: { breakLine: true } },
    { text: "4. Re-run, refresh validation, hand SPF tables to the frontier team" },
  ], { placeholder: "body" });

  await pres.writeFile({ fileName: DECK });
  if (SKILL) {
    const { applyTheme } = require(path.join(SKILL, "scripts/apply_theme.js"));
    await applyTheme(DECK, THEME);
  }
  console.log("Wrote", DECK);
})();
