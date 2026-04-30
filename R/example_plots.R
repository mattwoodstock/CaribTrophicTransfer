# ============================================================
# SEDAR 103 - Portfolio Approach for U.S. Caribbean Fisheries
# Example Plots: Assessment Phase & Implementation Phase
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

# Create output directory if it doesn't exist
if (!dir.exists("./Plots")) {
  dir.create("./Plots")
}

# ── Color palette ────────────────────────────────────────────
pal_guild   <- c("Lobster" = "#E05C3A", "Conch" = "#F0A500",
                 "Shallow Reef Fish" = "#2E86AB", "Deep Reef Fish" = "#1B4F72",
                 "Coastal Pelagics" = "#27AE60")
pal_gear    <- c("Traps" = "#E05C3A", "Diving" = "#2E86AB",
                 "Hook & Line" = "#27AE60", "Nets" = "#8E44AD",
                 "Bottom Long Line" = "#F0A500")
pal_island  <- c("Puerto Rico" = "#2E86AB", "St. Croix" = "#E05C3A",
                 "St. Thomas/St. John" = "#27AE60")
pal_traffic <- c("Safe" = "#27AE60", "Caution" = "#F0A500", "Overfished" = "#E05C3A")

theme_sedar <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, color = "#1a1a2e"),
      plot.subtitle = element_text(size = 10, color = "#555555"),
      axis.title    = element_text(size = 10, color = "#333333"),
      legend.title  = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      strip.text    = element_text(face = "bold"),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# ============================================================
# ASSESSMENT PHASE
# ============================================================

# ── A1: Ecosystem MSY by Island ──────────────────────────────
set.seed(42)
msy_data <- expand.grid(
  island = c("Puerto Rico", "St. Croix", "St. Thomas/St. John"),
  guild  = names(pal_guild)
) %>%
  mutate(
    msy_mt = c(850, 420, 95, 310, 180,
               140,  68, 22,  55,  38,
               110,  52, 18,  42,  29),
    lower  = msy_mt * 0.75,
    upper  = msy_mt * 1.25
  )

p_msy <- ggplot(msy_data, aes(x = guild, y = msy_mt, fill = guild)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25, linewidth = 0.6) +
  facet_wrap(~island, scales = "free_y") +
  scale_fill_manual(values = pal_guild, guide = "none") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "A1 — Ecosystem MSY Estimates by Species Guild and Island",
    subtitle = "Assessment Phase | Step 1–2: Trophic Transfer Model Output",
    x = NULL, y = "MSY (metric tons/year)"
  ) +
  theme_sedar() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8))

# ── A2: Energy Utilization Stoplight Plot ────────────────────
eu_data <- data.frame(
  island = rep(c("Puerto Rico", "St. Croix", "St. Thomas/St. John"), each = 5),
  guild  = rep(names(pal_guild), 3),
  energy_util = c(0.55, 0.92, 0.45, 0.38, 0.61,
                  0.95, 0.68, 0.35, 0.51, 0.44,
                  0.91, 0.93, 0.29, 0.63, 0.57)
) %>%
  mutate(status = case_when(
    energy_util < 0.6  ~ "Safe",
    energy_util < 0.9  ~ "Caution",
    TRUE               ~ "Overfished"
  ))

p_stoplight <- ggplot(eu_data, aes(x = island, y = guild, fill = status, size = energy_util)) +
  geom_point(shape = 21, alpha = 0.85, stroke = 0.8) +
  geom_text(aes(label = sprintf("%.2f", energy_util)), size = 2.8, color = "white", fontface = "bold") +
  scale_fill_manual(values = pal_traffic, name = "Status") +
  scale_size_continuous(range = c(8, 18), guide = "none") +
  scale_y_discrete(limits = rev(levels(factor(eu_data$guild)))) +
  labs(
    title    = "A2 — Energy Utilization Ratio (Stoplight Plot)",
    subtitle = "Assessment Phase | Step 2: Ecosystem Overfishing Indicator",
    x = NULL, y = NULL,
    caption  = "Threshold: < 0.60 Safe | 0.60–0.90 Caution | > 0.90 Overfished (Intervention Recommended)"
  ) +
  theme_sedar() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ── A3: Species Composition by Gear (Puerto Rico) ────────────
years <- 2005:2023
comp_data <- expand.grid(year = years, gear = names(pal_gear)) %>%
  mutate(
    base_val = case_when(
      gear == "Traps"           ~ 12000,
      gear == "Hook & Line"     ~ 9000,
      gear == "Diving"          ~ 5000,
      gear == "Bottom Long Line" ~ 3000,
      gear == "Nets"            ~ 2500,
      TRUE                      ~ 1000
    ),
    trend = case_when(
      gear == "Traps"           ~ (year - 2005) * 120,
      gear == "Hook & Line"     ~ (year - 2005) * 200,
      gear == "Diving"          ~ (year - 2005) * -80,
      gear == "Nets"            ~ (year - 2005) * -50,
      TRUE                      ~ 0
    ),
    noise_sd = case_when(
      gear == "Traps"           ~ 800,
      gear == "Hook & Line"     ~ 600,
      gear == "Diving"          ~ 400,
      gear == "Bottom Long Line" ~ 300,
      gear == "Nets"            ~ 250,
      TRUE                      ~ 100
    )
  ) %>%
  rowwise() %>%
  mutate(trips = pmax(100, base_val + trend + rnorm(1, 0, noise_sd))) %>%
  ungroup()

p_gear <- ggplot(comp_data, aes(x = year, y = trips, fill = gear)) +
  geom_col(position = "stack", width = 0.8) +
  scale_fill_manual(values = pal_gear, name = "Gear Type") +
  scale_x_continuous(breaks = seq(2005, 2023, 3)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "A3 — Fishing Trips by Gear Type: Puerto Rico",
    subtitle = "Assessment Phase | Step 3: Historical Effort Composition",
    x = "Year", y = "Number of Trips"
  ) +
  theme_sedar()

# ── A4: Min/Max Effort Envelope by Gear and Island ───────────
effort_bounds <- data.frame(
  island = rep(c("Puerto Rico", "St. Croix", "St. Thomas/St. John"), each = 3),
  gear   = rep(c("Traps", "Diving", "Hook & Line"), 3),
  min_effort = c(8000, 3500, 6000, 1500, 800, 1200, 1200, 600, 900),
  max_effort = c(18000, 8000, 14000, 3500, 2000, 3000, 2800, 1500, 2200),
  current    = c(13500, 8500, 10800, 2400, 1200, 2100, 3200, 900, 1500)
)

p_effort <- ggplot(effort_bounds, aes(x = gear, color = island, group = island)) +
  geom_linerange(aes(ymin = min_effort, ymax = max_effort),
                 linewidth = 8, alpha = 0.20,
                 position = position_dodge(width = 0.5)) +
  geom_point(aes(y = current, shape = "Current Effort"),
             size = 3.5, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = pal_island, name = "Island") +
  scale_shape_manual(values = 18, name = NULL) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~island) +
  labs(
    title    = "A4 — Sustainable Effort Envelope by Gear Type",
    subtitle = "Assessment Phase | Step 5: Min–Max Biologically & Economically Sustainable Effort",
    x = NULL, y = "Annual Effort (trips or trap-sets)",
    caption  = "Shaded band = sustainable range | Point = current effort level"
  ) +
  theme_sedar() +
  theme(legend.position = "none")

# ============================================================
# IMPLEMENTATION PHASE
# ============================================================

# ── I1: Annual Effort Tracking vs. Limit ─────────────────────
impl_years <- 2015:2023
effort_track <- expand.grid(year = impl_years,
                            gear = c("Traps", "Diving", "Hook & Line")) %>%
  mutate(
    limit   = case_when(gear == "Traps" ~ 18000, gear == "Diving" ~ 8000, TRUE ~ 14000),
    min_lim = case_when(gear == "Traps" ~ 8000,  gear == "Diving" ~ 3500, TRUE ~ 6000),
    effort  = case_when(
      gear == "Traps"       ~ 14000 + rnorm(n(), 0, 800) + (year - 2015) * 600,
      gear == "Diving"      ~ 5200  + rnorm(n(), 0, 400) - (year - 2015) * 60,
      gear == "Hook & Line" ~ 11000 + rnorm(n(), 0, 700) + (year - 2015) * 450,
      TRUE ~ 1000
    )
  )

p_track <- ggplot(effort_track, aes(x = year)) +
  geom_ribbon(aes(ymin = min_lim, ymax = limit), fill = "#27AE60", alpha = 0.12) +
  geom_hline(aes(yintercept = limit),   linetype = "dashed", color = "#E05C3A", linewidth = 0.8) +
  geom_hline(aes(yintercept = min_lim), linetype = "dashed", color = "#F0A500", linewidth = 0.8) +
  geom_line(aes(y = effort, color = gear), linewidth = 1.2) +
  geom_point(aes(y = effort, color = gear), size = 2.5) +
  scale_color_manual(values = pal_gear[c("Traps","Diving","Hook & Line")], name = "Gear") +
  scale_x_continuous(breaks = impl_years) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~gear, scales = "free_y") +
  labs(
    title    = "I1 — Annual Effort Tracking vs. Sustainable Limits",
    subtitle = "Implementation Phase | Trigger: Effort exceeds maximum sustainable level (Intervention Recommended)",
    x = "Year", y = "Annual Effort",
    caption  = "Green band = sustainable range | Red dashed = max limit | Orange dashed = min viable"
  ) +
  theme_sedar() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# ── I2: Ecosystem Indicator Dashboard ────────────────────────
indicators <- data.frame(
  year = rep(impl_years, 4),
  indicator = rep(c("Pelagic:Demersal Ratio", "Energy Utilization",
                    "Gini Index (Revenue)", "Mean Max Length (cm)"), each = length(impl_years)),
  value = c(
    # Pelagic:Demersal (baseline ~1.0)
    1.0 + cumsum(rnorm(length(impl_years), 0.04, 0.06)),
    # Energy Utilization (target <0.9)
    0.65 + cumsum(rnorm(length(impl_years), 0.03, 0.03)),
    # Gini (lower better, ~0.35)
    0.35 + cumsum(rnorm(length(impl_years), 0.025, 0.02)),
    # Mean Max Length (higher better, ~55cm)
    55 + cumsum(rnorm(length(impl_years), -0.6, 1.5))
  ),
  threshold_upper = c(rep(1.25, length(impl_years)), rep(0.90, length(impl_years)),
                      rep(0.50, length(impl_years)), rep(NA, length(impl_years))),
  threshold_lower = c(rep(0.75, length(impl_years)), rep(NA, length(impl_years)),
                      rep(NA, length(impl_years)), rep(45, length(impl_years)))
)

p_indicators <- ggplot(indicators, aes(x = year, y = value)) +
  geom_ribbon(aes(ymin = ifelse(is.na(threshold_lower), value, threshold_lower),
                  ymax = ifelse(is.na(threshold_upper), value, threshold_upper)),
              fill = "#27AE60", alpha = 0.10) +
  geom_hline(aes(yintercept = threshold_upper), linetype = "dashed",
             color = "#E05C3A", linewidth = 0.7, na.rm = TRUE) +
  geom_hline(aes(yintercept = threshold_lower), linetype = "dashed",
             color = "#F0A500", linewidth = 0.7, na.rm = TRUE) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_point(color = "#2E86AB", size = 2.5) +
  facet_wrap(~indicator, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(2015, 2023, 2)) +
  labs(
    title    = "I2 — Ecosystem & Socioeconomic Indicator Trends",
    subtitle = "Implementation Phase | Annual tracking with action trigger thresholds",
    x = "Year", y = "Indicator Value",
    caption  = "Green band = acceptable range | Red dashed = upper trigger | Orange dashed = lower trigger"
  ) +
  theme_sedar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# ── I3: Species Abundance Index with Trigger ─────────────────
set.seed(7)
spp_data <- data.frame(
  year   = impl_years,
  lobster = 100 + cumsum(rnorm(length(impl_years), -1.5, 5)),
  conch   = 100 + cumsum(rnorm(length(impl_years), -0.8, 4)),
  snapper = 100 + cumsum(rnorm(length(impl_years),  0.5, 6))
) %>%
  pivot_longer(-year, names_to = "species", values_to = "index") %>%
  group_by(species) %>%
  mutate(
    hist_mean = mean(index),
    hist_sd   = sd(index),
    trigger   = hist_mean - 2 * hist_sd,
    below_trigger = index < trigger
  )

p_abundance <- ggplot(spp_data, aes(x = year, y = index, color = species)) +
  geom_hline(aes(yintercept = trigger, color = species),
             linetype = "dashed", linewidth = 0.6, alpha = 0.7) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(shape = below_trigger), size = 3) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 8),
                     labels = c("Normal", "Below Trigger (Intervention Recommended)"),
                     name = "Status") +
  scale_color_manual(values = c("lobster" = "#E05C3A", "conch" = "#F0A500",
                                "snapper" = "#2E86AB"), name = "Species") +
  scale_x_continuous(breaks = impl_years) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title    = "I3 — Species Abundance Index Relative to Trigger Thresholds",
    subtitle = "Implementation Phase | Trigger: Abundance > 2 SD below historical mean",
    x = "Year", y = "Relative Abundance Index (base = 100)",
    caption  = "Dashed lines = species-specific trigger thresholds (mean - 2 SD)"
  ) +
  theme_sedar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ── I4: Management Scorecard (MSE Performance Table) ─────────
score_data <- data.frame(
  metric    = c("Energy Utilization < 0.9", "Piscivore Biomass > B_MMSY",
                "Pelagic:Demersal ± 25%", "Profitable Trips > 70%",
                "Gini Index < 0.50", "Effort Limit Changes < 2/yr"),
  category  = c("Biological","Biological","Ecosystem","Socioeconomic",
                "Socioeconomic","Management"),
  pr_score  = c(0.88, 0.72, 0.91, 0.79, 0.83, 0.95),
  stx_score = c(0.72, 0.61, 0.84, 0.65, 0.76, 0.90),
  stt_score = c(0.65, 0.55, 0.79, 0.58, 0.70, 0.88)
) %>%
  pivot_longer(cols = ends_with("_score"), names_to = "island", values_to = "score") %>%
  mutate(
    island = recode(island, pr_score = "Puerto Rico",
                    stx_score = "St. Croix", stt_score = "St. Thomas/St. John"),
    status = case_when(score >= 0.80 ~ "Meeting Goal", score >= 0.60 ~ "Near Goal",
                       TRUE ~ "Not Meeting Goal")
  )

p_scorecard <- ggplot(score_data,
                      aes(x = island, y = metric, fill = status)) +
  geom_tile(color = "white", linewidth = 1.2, width = 0.92, height = 0.88) +
  geom_text(aes(label = percent(score, accuracy = 1)), size = 3, fontface = "bold",
            color = "white") +
  scale_fill_manual(values = c("Meeting Goal" = "#27AE60",
                               "Near Goal"    = "#F0A500",
                               "Not Meeting Goal" = "#E05C3A"),
                    name = "Performance") +
  facet_grid(category ~ ., scales = "free_y", space = "free") +
  labs(
    title    = "I4 — MSE Performance Scorecard by Island",
    subtitle = "Implementation Phase | Annual probability of meeting each management goal",
    x = NULL, y = NULL
  ) +
  theme_sedar() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        strip.background = element_rect(fill = "#1B4F72", color = NA),
        strip.text = element_text(color = "white", size = 8))

# ============================================================
# SAVE ALL PLOTS
# ============================================================

ggsave("./Plots/SEDAR103_A1_ecosystem_msy.png",        p_msy,       width = 11, height = 5,  dpi = 180)
ggsave("./Plots/SEDAR103_A2_stoplight_energy.png",     p_stoplight, width = 9,  height = 5,  dpi = 180)
ggsave("./Plots/SEDAR103_A3_gear_composition.png",     p_gear,      width = 10, height = 5,  dpi = 180)
ggsave("./Plots/SEDAR103_A4_effort_envelope.png",      p_effort,    width = 11, height = 5,  dpi = 180)
ggsave("./Plots/SEDAR103_I1_effort_tracking.png",      p_track,     width = 11, height = 5,  dpi = 180)
ggsave("./Plots/SEDAR103_I2_ecosystem_indicators.png", p_indicators,width = 10, height = 6,  dpi = 180)
ggsave("./Plots/SEDAR103_I3_species_abundance.png",    p_abundance, width = 10, height = 5,  dpi = 180)
ggsave("./Plots/SEDAR103_I4_mse_scorecard.png",        p_scorecard, width = 10, height = 6,  dpi = 180)

# Combined assessment panel
assessment_panel <- (p_msy / p_stoplight) | (p_gear / p_effort)
ggsave("./Plots/SEDAR103_Assessment_Phase_Panel.png", assessment_panel,
       width = 18, height = 12, dpi = 180)

# Combined implementation panel
impl_panel <- (p_track / p_indicators) | (p_abundance / p_scorecard)
ggsave("./Plots/SEDAR103_Implementation_Phase_Panel.png", impl_panel,
       width = 18, height = 12, dpi = 180)

message("All plots saved. Packages required: ggplot2, dplyr, tidyr, patchwork, scales")

# ============================================================
# SCENARIO FORECASTING & PERFORMANCE METRIC COMPARISONS
# ============================================================
# Scenarios tested:
#   1. Baseline       — status quo effort, stable environment
#   2. Coral Bleaching — acute habitat loss reducing reef productivity
#   3. Tourism Boom   — elevated demand raises economic ceiling but increases pressure
#   4. Effort Surge   — unregulated increase in fishing effort
#   5. Climate + Effort (Combined) — warming + increased effort (worst case)
#   6. Managed Reduction — proactive effort reduction triggered by indicators
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

set.seed(2024)

scenarios   <- c("Baseline", "Coral Bleaching", "Tourism Boom",
                 "Effort Surge", "Climate + Effort", "Managed Reduction")
fore_years  <- 2024:2035
n_years     <- length(fore_years)

# ── Intervention flag data (year × scenario combinations requiring action) ──
interventions <- data.frame(
  scenario = c("Effort Surge", "Effort Surge", "Effort Surge",
               "Climate + Effort", "Climate + Effort", "Climate + Effort", "Climate + Effort",
               "Coral Bleaching", "Coral Bleaching",
               "Tourism Boom"),
  year     = c(2026, 2028, 2031,
               2025, 2027, 2029, 2032,
               2027, 2030,
               2029)
)

# ── Helper: smooth random walk with scenario-specific drift ──
rwalk <- function(start, drift, sd, n, floor = -Inf, ceiling = Inf) {
  x <- numeric(n); x[1] <- start
  for (i in 2:n) x[i] <- min(ceiling, max(floor, x[i-1] + drift + rnorm(1, 0, sd)))
  x
}

# ── Build metric trajectories ─────────────────────────────────
build_scenario <- function(scen) {
  switch(scen,
         "Baseline" = data.frame(
           year             = fore_years,
           energy_util      = rwalk(0.66, 0.002, 0.02, n_years, floor=0.3, ceiling=1.1),
           biomass_index    = rwalk(100,  0.3,   3.0,  n_years, floor=20),
           pel_dem_ratio    = rwalk(1.00, 0.000, 0.04, n_years, floor=0.4, ceiling=2.0),
           viable_profit_p  = rwalk(0.78, 0.002, 0.02, n_years, floor=0,   ceiling=1),
           gini             = rwalk(0.36, 0.001, 0.01, n_years, floor=0.1, ceiling=0.8),
           effort_index     = rwalk(100,  0.2,   2.5,  n_years, floor=10)
         ),
         "Coral Bleaching" = data.frame(
           year             = fore_years,
           energy_util      = rwalk(0.66, 0.018, 0.025, n_years, floor=0.3, ceiling=1.15),
           biomass_index    = rwalk(100, -2.8,   4.0,   n_years, floor=10),
           pel_dem_ratio    = rwalk(1.00, 0.035, 0.06,  n_years, floor=0.4, ceiling=2.5),
           viable_profit_p  = rwalk(0.78,-0.022, 0.025, n_years, floor=0,   ceiling=1),
           gini             = rwalk(0.36, 0.015, 0.015, n_years, floor=0.1, ceiling=0.85),
           effort_index     = rwalk(100,  0.5,   3.0,   n_years, floor=10)
         ),
         "Tourism Boom" = data.frame(
           year             = fore_years,
           energy_util      = rwalk(0.66, 0.012, 0.022, n_years, floor=0.3, ceiling=1.1),
           biomass_index    = rwalk(100, -0.8,   3.5,   n_years, floor=10),
           pel_dem_ratio    = rwalk(1.00, 0.010, 0.05,  n_years, floor=0.4, ceiling=2.5),
           viable_profit_p  = rwalk(0.78, 0.018, 0.02,  n_years, floor=0,   ceiling=1),
           gini             = rwalk(0.36, 0.020, 0.015, n_years, floor=0.1, ceiling=0.85),
           effort_index     = rwalk(100,  2.2,   3.0,   n_years, floor=10)
         ),
         "Effort Surge" = data.frame(
           year             = fore_years,
           energy_util      = rwalk(0.66, 0.030, 0.025, n_years, floor=0.3, ceiling=1.2),
           biomass_index    = rwalk(100, -3.5,   4.5,   n_years, floor=5),
           pel_dem_ratio    = rwalk(1.00,-0.020, 0.06,  n_years, floor=0.3, ceiling=2.5),
           viable_profit_p  = rwalk(0.78,-0.025, 0.025, n_years, floor=0,   ceiling=1),
           gini             = rwalk(0.36, 0.025, 0.018, n_years, floor=0.1, ceiling=0.9),
           effort_index     = rwalk(100,  4.5,   3.5,   n_years, floor=10)
         ),
         "Climate + Effort" = data.frame(
           year             = fore_years,
           energy_util      = rwalk(0.66, 0.042, 0.030, n_years, floor=0.3, ceiling=1.3),
           biomass_index    = rwalk(100, -5.0,   5.0,   n_years, floor=5),
           pel_dem_ratio    = rwalk(1.00, 0.045, 0.07,  n_years, floor=0.3, ceiling=2.8),
           viable_profit_p  = rwalk(0.78,-0.040, 0.030, n_years, floor=0,   ceiling=1),
           gini             = rwalk(0.36, 0.030, 0.020, n_years, floor=0.1, ceiling=0.9),
           effort_index     = rwalk(100,  5.5,   4.0,   n_years, floor=10)
         ),
         "Managed Reduction" = data.frame(
           year             = fore_years,
           energy_util      = rwalk(0.66,-0.010, 0.018, n_years, floor=0.3, ceiling=1.1),
           biomass_index    = rwalk(100,  1.5,   2.5,   n_years, floor=20),
           pel_dem_ratio    = rwalk(1.00, 0.002, 0.035, n_years, floor=0.5, ceiling=1.8),
           viable_profit_p  = rwalk(0.78, 0.010, 0.018, n_years, floor=0,   ceiling=1),
           gini             = rwalk(0.36,-0.005, 0.010, n_years, floor=0.1, ceiling=0.7),
           effort_index     = rwalk(100, -2.0,   2.0,   n_years, floor=10)
         )
  )
}

fore_all <- bind_rows(lapply(scenarios, function(s) {
  build_scenario(s) %>% mutate(scenario = s)
})) %>%
  mutate(scenario = factor(scenario, levels = scenarios))

# Thresholds
thresh <- list(
  energy_util_hi  = 0.90,
  energy_util_lo  = 0.40,
  biomass_lo      = 50,
  pel_dem_hi      = 1.25,
  pel_dem_lo      = 0.75,
  viable_profit_lo= 0.60,
  gini_hi         = 0.50,
  effort_hi       = 130
)

scen_colors <- c(
  "Baseline"         = "#2E86AB",
  "Coral Bleaching"  = "#E05C3A",
  "Tourism Boom"     = "#F0A500",
  "Effort Surge"     = "#8E44AD",
  "Climate + Effort" = "#C0392B",
  "Managed Reduction"= "#27AE60"
)

# ── S1: Energy Utilization Forecast ──────────────────────────
p_s1 <- ggplot(fore_all, aes(x = year, y = energy_util, color = scenario)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=thresh$energy_util_hi, ymax=Inf,
           fill="#E05C3A", alpha=0.08) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=thresh$energy_util_lo,
           fill="#F0A500", alpha=0.08) +
  geom_hline(yintercept=thresh$energy_util_hi, linetype="dashed", color="#E05C3A", linewidth=0.7) +
  geom_hline(yintercept=thresh$energy_util_lo, linetype="dashed", color="#F0A500", linewidth=0.7) +
  geom_line(linewidth=1.1, alpha=0.9) +
  geom_vline(data = filter(interventions, scenario %in% c("Effort Surge","Climate + Effort","Coral Bleaching")),
             aes(xintercept=year), linetype="dotted", color="grey30", linewidth=0.6) +
  geom_point(data = filter(fore_all, scenario %in% interventions$scenario) %>%
               semi_join(interventions, by=c("scenario","year")),
             aes(y=energy_util), shape=25, size=3.5, fill="black") +
  scale_color_manual(values=scen_colors, name="Scenario") +
  scale_x_continuous(breaks=seq(2024,2035,2)) +
  labs(title="S1 — Energy Utilization Ratio Forecast",
       subtitle="Trigger: > 0.90 (overfishing) | < 0.40 (underfishing). ▼ = Intervention recommended",
       x="Year", y="Energy Utilization Ratio") +
  theme_sedar()

# ── S2: Biomass Index Forecast ────────────────────────────────
p_s2 <- ggplot(fore_all, aes(x=year, y=biomass_index, color=scenario)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=thresh$biomass_lo,
           fill="#E05C3A", alpha=0.08) +
  geom_hline(yintercept=thresh$biomass_lo, linetype="dashed", color="#E05C3A", linewidth=0.7) +
  geom_line(linewidth=1.1, alpha=0.9) +
  geom_vline(data=filter(interventions, scenario %in% c("Effort Surge","Climate + Effort","Coral Bleaching")),
             aes(xintercept=year), linetype="dotted", color="grey30", linewidth=0.6) +
  geom_point(data=filter(fore_all, scenario %in% interventions$scenario) %>%
               semi_join(interventions, by=c("scenario","year")),
             aes(y=biomass_index), shape=25, size=3.5, fill="black") +
  scale_color_manual(values=scen_colors, name="Scenario") +
  scale_x_continuous(breaks=seq(2024,2035,2)) +
  labs(title="S2 — Piscivore/Herbivore Biomass Index Forecast",
       subtitle="Trigger: < 50 (relative to B_MMSY baseline of 100). ▼ = Intervention recommended",
       x="Year", y="Biomass Index (baseline = 100)") +
  theme_sedar()

# ── S3: Pelagic:Demersal Ratio ────────────────────────────────
p_s3 <- ggplot(fore_all, aes(x=year, y=pel_dem_ratio, color=scenario)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=thresh$pel_dem_hi, ymax=Inf,
           fill="#E05C3A", alpha=0.08) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=thresh$pel_dem_lo,
           fill="#E05C3A", alpha=0.08) +
  geom_hline(yintercept=thresh$pel_dem_hi, linetype="dashed", color="#E05C3A", linewidth=0.7) +
  geom_hline(yintercept=thresh$pel_dem_lo, linetype="dashed", color="#E05C3A", linewidth=0.7) +
  geom_hline(yintercept=1.00, linetype="solid",  color="grey50", linewidth=0.5) +
  geom_line(linewidth=1.1, alpha=0.9) +
  geom_vline(data=filter(interventions, scenario %in% c("Climate + Effort","Coral Bleaching")),
             aes(xintercept=year), linetype="dotted", color="grey30", linewidth=0.6) +
  geom_point(data=filter(fore_all, scenario %in% interventions$scenario) %>%
               semi_join(interventions, by=c("scenario","year")) %>%
               filter(pel_dem_ratio > thresh$pel_dem_hi | pel_dem_ratio < thresh$pel_dem_lo),
             aes(y=pel_dem_ratio), shape=25, size=3.5, fill="black") +
  scale_color_manual(values=scen_colors, name="Scenario") +
  scale_x_continuous(breaks=seq(2024,2035,2)) +
  labs(title="S3 — Pelagic:Demersal Landings Ratio Forecast",
       subtitle="Trigger: outside ±25% of baseline (0.75–1.25). ▼ = Intervention recommended",
       x="Year", y="Pelagic:Demersal Ratio") +
  theme_sedar()

# ── S4: Viable Profit Probability ────────────────────────────
p_s4 <- ggplot(fore_all, aes(x=year, y=viable_profit_p, color=scenario)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=thresh$viable_profit_lo,
           fill="#E05C3A", alpha=0.08) +
  geom_hline(yintercept=thresh$viable_profit_lo, linetype="dashed", color="#E05C3A", linewidth=0.7) +
  geom_line(linewidth=1.1, alpha=0.9) +
  geom_vline(data=filter(interventions, scenario %in% c("Effort Surge","Climate + Effort","Coral Bleaching")),
             aes(xintercept=year), linetype="dotted", color="grey30", linewidth=0.6) +
  geom_point(data=filter(fore_all, scenario %in% interventions$scenario) %>%
               semi_join(interventions, by=c("scenario","year")),
             aes(y=viable_profit_p), shape=25, size=3.5, fill="black") +
  scale_color_manual(values=scen_colors, name="Scenario") +
  scale_x_continuous(breaks=seq(2024,2035,2)) +
  scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0,1)) +
  labs(title="S4 — Probability of Viable Profit per Trip",
       subtitle="Trigger: < 60% of trips exceed minimum viable catch. ▼ = Intervention recommended",
       x="Year", y="Pr(Viable Profit)") +
  theme_sedar()

# ── S5: Gini Index (Revenue Inequality) ──────────────────────
p_s5 <- ggplot(fore_all, aes(x=year, y=gini, color=scenario)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=thresh$gini_hi, ymax=Inf,
           fill="#E05C3A", alpha=0.08) +
  geom_hline(yintercept=thresh$gini_hi, linetype="dashed", color="#E05C3A", linewidth=0.7) +
  geom_line(linewidth=1.1, alpha=0.9) +
  geom_vline(data=filter(interventions, scenario=="Tourism Boom"),
             aes(xintercept=year), linetype="dotted", color="grey30", linewidth=0.6) +
  geom_point(data=filter(fore_all, scenario=="Tourism Boom") %>%
               semi_join(filter(interventions, scenario=="Tourism Boom"), by=c("scenario","year")),
             aes(y=gini), shape=25, size=3.5, fill="black") +
  scale_color_manual(values=scen_colors, name="Scenario") +
  scale_x_continuous(breaks=seq(2024,2035,2)) +
  labs(title="S5 — Revenue Gini Index Forecast (Income Inequality)",
       subtitle="Trigger: Gini > 0.50. ▼ = Intervention recommended",
       x="Year", y="Gini Index") +
  theme_sedar()

# ── S6: Effort Index Forecast ─────────────────────────────────
p_s6 <- ggplot(fore_all, aes(x=year, y=effort_index, color=scenario)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=thresh$effort_hi, ymax=Inf,
           fill="#E05C3A", alpha=0.08) +
  geom_hline(yintercept=thresh$effort_hi, linetype="dashed", color="#E05C3A", linewidth=0.7) +
  geom_line(linewidth=1.1, alpha=0.9) +
  geom_vline(data=filter(interventions, scenario %in% c("Effort Surge","Climate + Effort","Tourism Boom")),
             aes(xintercept=year), linetype="dotted", color="grey30", linewidth=0.6) +
  geom_point(data=filter(fore_all,
                         scenario %in% c("Effort Surge","Climate + Effort","Tourism Boom"),
                         effort_index > thresh$effort_hi),
             aes(y=effort_index), shape=25, size=3.5, fill="black") +
  scale_color_manual(values=scen_colors, name="Scenario") +
  scale_x_continuous(breaks=seq(2024,2035,2)) +
  labs(title="S6 — Relative Effort Index Forecast",
       subtitle="Trigger: Effort index > 130 (maximum sustainable). ▼ = Intervention recommended",
       x="Year", y="Effort Index (baseline = 100)") +
  theme_sedar()

# ── S7: Multi-metric Radar / End-state Comparison ────────────
# Summarise terminal values (2033–2035 mean) per scenario per metric
terminal <- fore_all %>%
  filter(year >= 2033) %>%
  group_by(scenario) %>%
  summarise(
    `Energy\nUtilization`   = mean(energy_util),
    `Biomass\nIndex`        = mean(biomass_index) / 100,   # normalise to 0-1 range
    `Pel:Dem\nRatio`        = 1 - abs(mean(pel_dem_ratio) - 1.0) / 1.0,  # distance from 1
    `Viable\nProfit`        = mean(viable_profit_p),
    `Revenue\nEquality`     = 1 - mean(gini),              # invert so higher = better
    `Effort\nControl`       = 1 - pmax(0, (mean(effort_index) - 100) / 100),
    .groups = "drop"
  ) %>%
  pivot_longer(-scenario, names_to="metric", values_to="value") %>%
  mutate(
    value = pmax(0, pmin(1.5, value)),
    # Invert energy util so lower = better shown as higher score
    value = ifelse(metric == "Energy\nUtilization", 1 - (value - 0.3) / 0.7, value),
    value = pmax(0, pmin(1, value)),
    scenario = factor(scenario, levels=scenarios)
  )

p_s7 <- ggplot(terminal, aes(x=metric, y=value, fill=scenario)) +
  geom_col(position=position_dodge(width=0.75), width=0.68, alpha=0.88) +
  geom_hline(yintercept=0.70, linetype="dashed", color="#E05C3A", linewidth=0.6) +
  scale_fill_manual(values=scen_colors, name="Scenario") +
  scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0,1.05)) +
  labs(title="S7 — End-state Performance Score by Scenario (2033–2035 Mean)",
       subtitle="Higher = better. Red dashed = minimum acceptable performance (70%). All metrics normalised to [0,1].",
       x=NULL, y="Normalised Performance Score") +
  theme_sedar() +
  theme(axis.text.x=element_text(size=9, lineheight=1.1))

# ── S8: Heatmap — Metric × Scenario Trigger Frequency ────────
# Count years where each metric breaches its trigger per scenario
trigger_freq <- fore_all %>%
  mutate(
    eu_breach     = energy_util     >  thresh$energy_util_hi | energy_util < thresh$energy_util_lo,
    bm_breach     = biomass_index   <  thresh$biomass_lo,
    pd_breach     = pel_dem_ratio   >  thresh$pel_dem_hi    | pel_dem_ratio < thresh$pel_dem_lo,
    vp_breach     = viable_profit_p <  thresh$viable_profit_lo,
    gi_breach     = gini            >  thresh$gini_hi,
    ef_breach     = effort_index    >  thresh$effort_hi
  ) %>%
  group_by(scenario) %>%
  summarise(
    `Energy Util`      = sum(eu_breach),
    `Biomass`          = sum(bm_breach),
    `Pel:Dem Ratio`    = sum(pd_breach),
    `Viable Profit`    = sum(vp_breach),
    `Gini Index`       = sum(gi_breach),
    `Effort Index`     = sum(ef_breach),
    .groups="drop"
  ) %>%
  pivot_longer(-scenario, names_to="metric", values_to="trigger_years") %>%
  mutate(scenario=factor(scenario, levels=rev(scenarios)))

p_s8 <- ggplot(trigger_freq, aes(x=metric, y=scenario, fill=trigger_years)) +
  geom_tile(color="white", linewidth=1) +
  geom_text(aes(label=ifelse(trigger_years>0, trigger_years, "—"),
                color=trigger_years > 6), size=3.5, fontface="bold") +
  scale_fill_gradient2(low="#EAF4FB", mid="#F0A500", high="#C0392B",
                       midpoint=4, name="Years\nBreaching\nTrigger") +
  scale_color_manual(values=c("FALSE"="grey20","TRUE"="white"), guide="none") +
  scale_x_discrete(guide=guide_axis(angle=25)) +
  labs(title="S8 — Trigger Breach Frequency by Scenario and Metric (2024–2035)",
       subtitle="Number of forecast years each performance trigger is breached per scenario",
       x=NULL, y=NULL) +
  theme_sedar() +
  theme(panel.grid=element_blank())

# ── S9: Intervention Timeline ─────────────────────────────────
# Show which scenarios require intervention and when, with metric driving the action
trigger_timeline <- fore_all %>%
  mutate(
    eu_breach  = energy_util     > thresh$energy_util_hi,
    bm_breach  = biomass_index   < thresh$biomass_lo,
    vp_breach  = viable_profit_p < thresh$viable_profit_lo,
    gi_breach  = gini            > thresh$gini_hi,
    ef_breach  = effort_index    > thresh$effort_hi
  ) %>%
  pivot_longer(cols=ends_with("_breach"), names_to="trigger_type", values_to="breached") %>%
  filter(breached) %>%
  mutate(trigger_type = recode(trigger_type,
                               eu_breach = "Energy Util", bm_breach = "Biomass",
                               vp_breach = "Viable Profit", gi_breach = "Gini Index", ef_breach = "Effort")) %>%
  distinct(scenario, year, trigger_type) %>%
  mutate(scenario=factor(scenario, levels=rev(scenarios)))

p_s9 <- ggplot(trigger_timeline, aes(x=year, y=scenario, color=trigger_type, shape=trigger_type)) +
  geom_point(size=4.5, alpha=0.85, position=position_jitter(height=0.15, seed=1)) +
  scale_color_manual(values=c("Energy Util"="#C0392B", "Biomass"="#8E44AD",
                              "Viable Profit"="#E05C3A", "Gini Index"="#F0A500",
                              "Effort"="#2E86AB"),
                     name="Trigger Type") +
  scale_shape_manual(values=c("Energy Util"=17,"Biomass"=15,"Viable Profit"=18,
                              "Gini Index"=8,"Effort"=16), name="Trigger Type") +
  scale_x_continuous(breaks=fore_years) +
  labs(title="S9 — Intervention Timeline by Scenario and Trigger Type",
       subtitle="Each point = a year requiring management action; shape/color = metric driving the trigger",
       x="Forecast Year", y=NULL) +
  theme_sedar() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8))

# ── Save scenario plots ───────────────────────────────────────
ggsave("./Plots/SEDAR103_S1_energy_util_forecast.png",    p_s1, width=11, height=5, dpi=180)
ggsave("./Plots/SEDAR103_S2_biomass_forecast.png",        p_s2, width=11, height=5, dpi=180)
ggsave("./Plots/SEDAR103_S3_peldcm_forecast.png",         p_s3, width=11, height=5, dpi=180)
ggsave("./Plots/SEDAR103_S4_viable_profit_forecast.png",  p_s4, width=11, height=5, dpi=180)
ggsave("./Plots/SEDAR103_S5_gini_forecast.png",           p_s5, width=11, height=5, dpi=180)
ggsave("./Plots/SEDAR103_S6_effort_forecast.png",         p_s6, width=11, height=5, dpi=180)
ggsave("./Plots/SEDAR103_S7_endstate_comparison.png",     p_s7, width=12, height=6, dpi=180)
ggsave("./Plots/SEDAR103_S8_trigger_heatmap.png",         p_s8, width=11, height=6, dpi=180)
ggsave("./Plots/SEDAR103_S9_intervention_timeline.png",   p_s9, width=12, height=6, dpi=180)

# Combined scenario panel (metric forecasts)
metric_panel <- (p_s1 + p_s2 + p_s3) / (p_s4 + p_s5 + p_s6) +
  plot_annotation(
    title    = "SEDAR 103 — Scenario Forecast: All Performance Metrics (2024–2035)",
    subtitle = "Six scenarios across environmental, economic, and fishing pressure drivers",
    theme    = theme(plot.title    = element_text(face="bold", size=14),
                     plot.subtitle = element_text(size=10))
  ) & theme(legend.position="right")
ggsave("./Plots/SEDAR103_Scenario_Metric_Panel.png", metric_panel, width=20, height=11, dpi=180)

# Summary panel
summary_panel <- (p_s7 / (p_s8 | p_s9)) +
  plot_annotation(
    title    = "SEDAR 103 — Scenario Comparison Summary",
    subtitle = "End-state performance, trigger breach frequency, and intervention timeline",
    theme    = theme(plot.title    = element_text(face="bold", size=14),
                     plot.subtitle = element_text(size=10))
  )
ggsave("./Plots/SEDAR103_Scenario_Summary_Panel.png", summary_panel, width=18, height=12, dpi=180)

message("Scenario plots saved.")