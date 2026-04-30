# ==============================================================================
# Island-Based Trophic Transfer Model (TTM) - U.S. Caribbean
# ==============================================================================
# This script simulates a quantitative trophic transfer food web designed for 
# data-limited environments. 
# 
# APPROACH: Links management complexes to general Trophic Levels (TL) using 
# accessible data. Incorporates functional group-specific thermal sensitivities 
# and targeted fishing effort multipliers to forecast systemic vulnerabilities.
# 
# MULTI-SCENARIO OUTPUT: Runs 5 distinct environmental and economic scenarios
# and compares the Ecosystem Overfishing Indicator side-by-side.
# 
# CONCEPTUAL DIAGRAM: Generates a native ggplot2 network schematic of the model.
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. GLOBAL BASELINE PARAMETERIZATION
# ------------------------------------------------------------------------------
set.seed(42)

TTE <- 0.10  # 10% Trophic Transfer Efficiency

# Primary Production (P1) inputs (Baseline state to compare across scenarios)
P1_benthic <- rnorm(1, mean = 500000, sd = 50000)
P1_pelagic <- rnorm(1, mean = 800000, sd = 80000)

# Trophic Level (TL) Calculations
energy_benthic <- c(TL1 = P1_benthic, TL2 = P1_benthic * TTE, TL3 = P1_benthic * (TTE^2), TL4 = P1_benthic * (TTE^3))
energy_pelagic <- c(TL1 = P1_pelagic, TL2 = P1_pelagic * TTE, TL3 = P1_pelagic * (TTE^2), TL4 = P1_pelagic * (TTE^3))

energy_available_tl <- c(
  B_TL2 = energy_benthic[["TL2"]], B_TL3 = energy_benthic[["TL3"]], B_TL4 = energy_benthic[["TL4"]],
  P_TL2 = energy_pelagic[["TL2"]], P_TL3 = energy_pelagic[["TL3"]], P_TL4 = energy_pelagic[["TL4"]]
)

guilds <- c("Parrotfishes", "Surgeonfishes", "Spiny Lobster", "Queen Conch", 
            "Triggerfishes", "Grunts", "Snappers", "Groupers", 
            "Tunas", "Mackerels", "Dolphinfishes")

# General Diet Matrix
diet_matrix <- matrix(c(
  # B_TL2  B_TL3  B_TL4  P_TL2  P_TL3  P_TL4
  0.85,  0.00,  0.00,  0.00,  0.00,  0.00,  # Parrotfishes
  0.90,  0.00,  0.00,  0.00,  0.00,  0.00,  # Surgeonfishes
  0.30,  0.50,  0.00,  0.00,  0.00,  0.00,  # Spiny Lobster
  0.95,  0.00,  0.00,  0.00,  0.00,  0.00,  # Queen Conch
  0.10,  0.70,  0.05,  0.00,  0.00,  0.00,  # Triggerfishes
  0.20,  0.60,  0.00,  0.05,  0.05,  0.00,  # Grunts
  0.00,  0.30,  0.40,  0.00,  0.15,  0.05,  # Snappers
  0.00,  0.20,  0.55,  0.00,  0.10,  0.05,  # Groupers
  0.00,  0.00,  0.00,  0.10,  0.40,  0.40,  # Tunas
  0.00,  0.00,  0.00,  0.20,  0.50,  0.20,  # Mackerels
  0.00,  0.00,  0.00,  0.05,  0.35,  0.50   # Dolphinfishes
), nrow = 11, byrow = TRUE)

colnames(diet_matrix) <- names(energy_available_tl)
rownames(diet_matrix) <- guilds
base_available_energy  <- as.vector(diet_matrix %*% energy_available_tl)

# Thermal Sensitivity & Production/Biomass Ratios
thermal_sensitivity <- c(
  "Parrotfishes" = -0.15, "Surgeonfishes" = -0.10, "Spiny Lobster" = -0.20, 
  "Queen Conch" = -0.15, "Triggerfishes" = -0.05, "Grunts" = -0.10, 
  "Snappers" = -0.08, "Groupers" = -0.12, 
  "Tunas" = -0.02, "Mackerels" = -0.02, "Dolphinfishes" = -0.02
)

pb_ratios <- c("Parrotfishes"=1.1, "Surgeonfishes"=1.2, "Spiny Lobster"=0.9, "Queen Conch"=0.8, 
               "Triggerfishes"=0.7, "Grunts"=0.8, "Snappers"=0.35, "Groupers"=0.25, 
               "Tunas"=0.5, "Mackerels"=0.6, "Dolphinfishes"=0.8)

base_exploitation_rate <- c("Parrotfishes"=0.45, "Surgeonfishes"=0.30, "Spiny Lobster"=0.82, 
                            "Queen Conch"=0.78, "Triggerfishes"=0.50, "Grunts"=0.40, 
                            "Snappers"=0.75, "Groupers"=0.70, "Tunas"=0.55, 
                            "Mackerels"=0.50, "Dolphinfishes"=0.45)

# ------------------------------------------------------------------------------
# 2. SCENARIO SIMULATION ENGINE
# ------------------------------------------------------------------------------
# Function to simulate a specific scenario given a temp anomaly and effort vector
simulate_scenario <- function(scenario_name, temp_anomaly, guild_effort_multipliers) {
  
  # 1. Apply Environmental Modifier
  env_modifier <- pmax(0, 1 + (thermal_sensitivity * temp_anomaly))
  available_energy_env <- base_available_energy * env_modifier
  available_biomass_env <- available_energy_env / pb_ratios
  names(available_biomass_env) <- guilds
  
  # 2. Simulate Harvest (adding slight random noise for catchability)
  catch_noise <- runif(length(guilds), min = 0.95, max = 1.05)
  harvest_biomass <- base_exploitation_rate * guild_effort_multipliers * available_biomass_env * catch_noise
  names(harvest_biomass) <- guilds
  
  # 3. Calculate Energy Utilization
  energy_caught <- harvest_biomass * pb_ratios
  energy_util_ratio <- energy_caught / available_energy_env
  energy_util_ratio[is.na(energy_util_ratio) | is.infinite(energy_util_ratio)] <- 0
  
  # 4. Return results dataframe
  data.frame(
    Scenario = scenario_name,
    Complex = guilds,
    Temp_Anomaly = temp_anomaly,
    Available_Biomass = available_biomass_env,
    Harvest_Biomass = harvest_biomass,
    Energy_Caught = energy_caught,
    Energy_Utilization = energy_util_ratio
  ) %>%
    mutate(
      Status = case_when(
        Energy_Utilization < 0.60 ~ "Safe",
        Energy_Utilization < 0.90 ~ "Caution",
        TRUE ~ "Overfished"
      ),
      Status = factor(Status, levels = c("Safe", "Caution", "Overfished"))
    )
}

# ------------------------------------------------------------------------------
# 3. DEFINE AND RUN SCENARIOS
# ------------------------------------------------------------------------------
# Scenario 1: Baseline (Status quo temp, status quo effort)
eff_base <- rep(1.0, length(guilds)); names(eff_base) <- guilds
res_base <- simulate_scenario("1. Baseline", 0.0, eff_base)

# Scenario 2: Coral Bleaching (+1.5C anomaly, status quo effort)
res_bleach <- simulate_scenario("2. Coral Bleaching", 1.5, eff_base)

# Scenario 3: Effort Surge (Status quo temp, 30% uniform effort increase)
eff_surge <- rep(1.3, length(guilds)); names(eff_surge) <- guilds
res_surge <- simulate_scenario("3. Effort Surge", 0.0, eff_surge)

# Scenario 4: Climate + Targeted Effort (+1.2C anomaly, uneven market-driven effort)
eff_target <- c("Parrotfishes"=1.0, "Surgeonfishes"=1.0, "Spiny Lobster"=1.30, 
                "Queen Conch"=1.15, "Triggerfishes"=1.0, "Grunts"=1.0, 
                "Snappers"=1.25, "Groupers"=1.10, "Tunas"=1.0, 
                "Mackerels"=1.05, "Dolphinfishes"=1.10)
res_worst <- simulate_scenario("4. Climate + Targeted Effort", 1.2, eff_target)

# Scenario 5: Managed Reduction (+0.5C anomaly, proactive effort reduction)
eff_managed <- rep(0.85, length(guilds)); names(eff_managed) <- guilds
res_managed <- simulate_scenario("5. Managed Reduction", 0.5, eff_managed)

# Combine all scenario results
all_results <- bind_rows(res_base, res_bleach, res_surge, res_worst, res_managed)

# ------------------------------------------------------------------------------
# 4. VISUALIZATION: MULTI-SCENARIO STOPLIGHT PLOT
# ------------------------------------------------------------------------------
pal_traffic <- c("Safe" = "#27AE60", "Caution" = "#F0A500", "Overfished" = "#E05C3A")

# Order the y-axis complexes logically based on their Baseline Energy Utilization
complex_order <- res_base %>% arrange(desc(Energy_Utilization)) %>% pull(Complex)
all_results$Complex <- factor(all_results$Complex, levels = rev(complex_order))

p_stoplight_multi <- ggplot(all_results, aes(x = Scenario, y = Complex)) +
  geom_tile(color = "white", fill = "gray97", alpha = 0.5, linewidth = 1) +
  geom_point(aes(fill = Status, size = Energy_Utilization), shape = 21, alpha = 0.9, stroke = 1) +
  geom_text(aes(label = sprintf("%.2f", Energy_Utilization)), size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = pal_traffic, drop = FALSE) +
  scale_size_continuous(range = c(9, 18), guide = "none") +
  scale_x_discrete(position = "top") +
  labs(
    title = "Ecosystem Overfishing Indicator Across Forecast Scenarios",
    subtitle = "Comparing impacts of thermal anomalies and shifting fishing effort on U.S. Caribbean fisheries",
    x = NULL, y = "Fishery Management Complex",
    fill = "Ecosystem Status",
    caption = "Thresholds: < 0.60 Safe | 0.60-0.90 Caution | > 0.90 Overfished (Action Required)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "#555555", margin = margin(b = 15)),
    panel.grid = element_blank(),
    axis.text.y = element_text(face = "bold", size = 11, color = "#333333"),
    axis.text.x.top = element_text(face = "bold", size = 10, angle = 15, hjust = 0, color = "#1a1a2e"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

print(p_stoplight_multi)

# ------------------------------------------------------------------------------
# 5. CONCEPTUAL DIAGRAM: FULL FOOD WEB & MODEL STRUCTURE
# ------------------------------------------------------------------------------
# Generates a detailed network diagram mapping all specific complexes

# Define Node Coordinates and Properties
nodes <- data.frame(
  name = c("Temp", "P1_B", "B_TL2", "B_TL3", "B_TL4",
           "Parrotfishes", "Surgeonfishes", "Queen Conch", "Spiny Lobster", 
           "Triggerfishes", "Grunts", "Snappers", "Groupers",
           "Stoplight", "Harvest",
           "Tunas", "Mackerels", "Dolphinfishes",
           "P1_P", "P_TL2", "P_TL3", "P_TL4", "Effort"),
  label = c("Thermal\nAnomaly", 
            "Benthic\nPrimary Prod", "Benthic TL2", "Benthic TL3", "Benthic TL4",
            "Parrotfishes", "Surgeonfishes", "Queen Conch", "Spiny Lobster", 
            "Triggerfishes", "Grunts", "Snappers", "Groupers",
            "Energy Utilization\n(Indicator)", "Harvest\nBiomass",
            "Tunas", "Mackerels", "Dolphinfishes",
            "Pelagic\nPrimary Prod", "Pelagic TL2", "Pelagic TL3", "Pelagic TL4", 
            "Targeted\nFishing Effort"),
  x = c(1,   # Temp
        2, 2, 2, 2, # Benthic TLs
        4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, # Benthic Complexes
        6.5, 6.5,       # Stoplight, Harvest (Center funnel)
        8.5, 8.5, 8.5,       # Pelagic Complexes
        11, 11, 11, 11, # Pelagic TLs
        12),        # Effort
  y = c(7,   # Temp
        1.5, 3.5, 5.5, 7.5, # Benthic TLs
        1, 2, 3, 4, 5, 6, 7, 8,    # Benthic Complexes (Bottom to Top)
        8.5, 1.5,     # Stoplight, Harvest
        3, 4.5, 6,   # Pelagic Complexes
        1.5, 3.5, 5.5, 7.5, # Pelagic TLs
        7),       # Effort
  category = c("Environment",
               "Environment", "Trophic Level", "Trophic Level", "Trophic Level",
               rep("Fishery Complex", 8),
               "Model Output", "Model Output",
               rep("Fishery Complex", 3),
               "Environment", "Trophic Level", "Trophic Level", "Trophic Level",
               "Fishery Driver")
)

# Dynamically construct edges from the diet matrix to ensure perfect accuracy
diet_df <- as.data.frame(as.table(diet_matrix))
names(diet_df) <- c("to", "from", "prop")
edges_dm <- diet_df %>%
  filter(prop > 0) %>%
  select(from, to) %>%
  mutate(edge_type = "Diet Matrix")

# Define other Edge Connections
edges_ef <- data.frame(
  from = c("P1_B", "B_TL2", "B_TL3", "P1_P", "P_TL2", "P_TL3"),
  to   = c("B_TL2", "B_TL3", "B_TL4", "P_TL2", "P_TL3", "P_TL4"),
  edge_type = "Energy Flow"
)
edges_ti <- data.frame(from = "Temp", to = guilds, edge_type = "Thermal Impact")
edges_fp <- data.frame(from = "Effort", to = guilds, edge_type = "Fishing Pressure")
edges_ho <- data.frame(from = guilds, to = "Harvest", edge_type = "Harvest Output")
edges_ic <- data.frame(from = "Harvest", to = "Stoplight", edge_type = "Indicator Calculation")

edges <- bind_rows(edges_ef, edges_dm, edges_ti, edges_fp, edges_ho, edges_ic)

# Merge coordinates for plotting segments
edges_coords <- edges %>%
  left_join(nodes %>% select(name, x, y), by = c("from" = "name")) %>%
  rename(x_from = x, y_from = y) %>%
  left_join(nodes %>% select(name, x, y), by = c("to" = "name")) %>%
  rename(x_to = x, y_to = y)

# Build Diagram
p_diagram <- ggplot() +
  geom_segment(data = edges_coords,
               aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
                   color = edge_type, linetype = edge_type),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               linewidth = 0.6, alpha = 0.7) +
  geom_label(data = nodes,
             aes(x = x, y = y, label = label, fill = category),
             color = "white", fontface = "bold", size = 3,
             label.padding = unit(0.3, "lines"), label.r = unit(0.3, "lines")) +
  scale_fill_manual(values = c("Environment" = "#2E86AB", "Trophic Level" = "#1B4F72",
                               "Fishery Complex" = "#F0A500", "Fishery Driver" = "#E05C3A",
                               "Model Output" = "#27AE60")) +
  scale_color_manual(values = c("Energy Flow" = "#1B4F72", "Diet Matrix" = "#F0A500",
                                "Thermal Impact" = "#E05C3A", "Fishing Pressure" = "#E05C3A",
                                "Harvest Output" = "#27AE60", "Indicator Calculation" = "#27AE60")) +
  scale_linetype_manual(values = c("Energy Flow" = "solid", "Diet Matrix" = "solid",
                                   "Thermal Impact" = "dashed", "Fishing Pressure" = "dashed",
                                   "Harvest Output" = "solid", "Indicator Calculation" = "dotted")) +
  coord_cartesian(xlim = c(0, 13), ylim = c(0.5, 9.5), clip = "off") +
  theme_void() +
  theme(
    legend.position = "bottom", legend.box = "horizontal", legend.margin = margin(t = 10),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#555555", margin = margin(b = 10))
  ) +
  labs(title = "Detailed Food Web & Trophic Model Structure",
       subtitle = "Explicit mapping of dietary connections across all managed fisheries complexes",
       fill = "Node Category", color = "Interaction Type", linetype = "Interaction Type")

print(p_diagram)

# ------------------------------------------------------------------------------
# 6. OUTPUT AND ADDITIONAL PLOTS
# ------------------------------------------------------------------------------
# Create directory for outputs if needed
if (!dir.exists("./Plots")) dir.create("./Plots")

# Save raw output data
write.csv(all_results, "./Plots/TTM_All_Scenario_Results.csv", row.names = FALSE)

# Plot: Available Biomass Comparison
p_biomass <- ggplot(all_results, aes(x = Scenario, y = Available_Biomass, fill = Complex)) +
  geom_col(position = "dodge", color = "white", linewidth = 0.2) +
  labs(title = "Available Biomass Capacity by Scenario and Complex",
       subtitle = "Demonstrates impact of thermal anomalies on carrying capacity",
       y = "Available Biomass", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
        plot.title = element_text(face = "bold"))

# Plot: Harvest Biomass Comparison
p_harvest <- ggplot(all_results, aes(x = Scenario, y = Harvest_Biomass, fill = Complex)) +
  geom_col(position = "dodge", color = "white", linewidth = 0.2) +
  labs(title = "Harvested Biomass by Scenario and Complex",
       subtitle = "Demonstrates combined impact of shifting capacity and targeted effort",
       y = "Harvested Biomass", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
        plot.title = element_text(face = "bold"))

print(p_biomass)
print(p_harvest)

# Save Plots Locally
ggsave("./Plots/TTM_Results.png", p_stoplight_multi, width = 14, height = 9, dpi = 300, bg = "white")

ggsave("./Plots/TTM_Conceptual_Diagram_Detailed.png", p_diagram, width = 14, height = 9, dpi = 300, bg = "white")
ggsave("./Plots/TTM_Available_Biomass_Bar.png", p_biomass, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("./Plots/TTM_Harvest_Biomass_Bar.png", p_harvest, width = 10, height = 6, dpi = 300, bg = "white")