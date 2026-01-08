# ==============================================================================
# Script: 05_Cluster_Source_Profiles.R
# Purpose: Calculate weighted source profiles and generate bar charts.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(RColorBrewer)

# 2. Load Clustered Data --------------------------------------------------
message("Loading Clustered Data...")

if(!file.exists("data/processed/04_Clustered_Data.rds")) {
  stop("Error: Input file not found. Run Script 04 first.")
}
Data_Clustered <- readRDS("data/processed/04_Clustered_Data.rds")

# Elements list
elements <- c('Cl', 'Ca', 'Si', 'Fe', 'Al', 'K', 'Ti', 
              'Cu', 'Ba', 'Zn', 'Te', 'Br', 'Pd')

# 3. Membership Weighting Calculation -------------------------------------
message("Calculating Membership-Weighted Concentrations...")

# The logic: Multiply element concentration by the probability of belonging to that cluster.
# This reduces the noise from "weak" members of a cluster.

weighted_data_list <- list()

for(k in 1:3) {
  prob_col <- paste0("Prob_Cluster_", k)
  
  # Filter for rows primarily assigned to cluster k
  subset_k <- Data_Clustered %>% filter(Cluster == k)
  
  # Multiply elements by probability
  weighted_k <- subset_k
  for(el in elements) {
    weighted_k[[el]] <- weighted_k[[el]] * weighted_k[[prob_col]]
  }
  
  weighted_data_list[[k]] <- weighted_k
}

# Combine back
Data_Weighted <- bind_rows(weighted_data_list)

# 4. Normalization (Min-Max) ----------------------------------------------
message("Normalizing Data (Min-Max)...")

minMax <- function(x) {
  if(max(x, na.rm=TRUE) == min(x, na.rm=TRUE)) return(rep(0, length(x))) # Avoid div by zero
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

# Apply scaling to the weighted elements
Data_Scaled_Weighted <- Data_Weighted
Data_Scaled_Weighted[elements] <- lapply(Data_Weighted[elements], minMax)

# 5. Calculate Contributions ----------------------------------------------
message("Calculating Percent Contributions...")

# Reshape to Long Format
Data_Long <- reshape2::melt(Data_Scaled_Weighted, 
                            id.vars = "Cluster", 
                            measure.vars = elements,
                            variable.name = "Pollutant", 
                            value.name = "Concentration")

# Aggregate: Mean concentration per Cluster & Pollutant
Profile_Agg <- Data_Long %>%
  group_by(Cluster, Pollutant) %>%
  summarise(Mean_Conc = mean(Concentration, na.rm=TRUE), .groups = 'drop')

# Calculate Contribution (Percent of Total in that Cluster)
Profile_Agg <- Profile_Agg %>%
  group_by(Cluster) %>%
  mutate(Total_Conc = sum(Mean_Conc),
         Contribution = Mean_Conc / Total_Conc) %>%
  ungroup()

# 6. Visualization --------------------------------------------------------
message("Generating Source Profile Plots...")

# Define Colors (24 custom colors from your original script)
c24 <- c("#E31A1C", "green4", "#6A3D9A", "#FF7F00", "gold1",
         "skyblue2", "#FB9A99", "palegreen2", "#CAB2D6", "darkred",
         "#FDBF6F", "gray70", "khaki2", "maroon", "orchid1", "deeppink1", 
         "blue1", "steelblue4", "darkturquoise", "green1", "yellow4", 
         "yellow3", "darkorange4")

if(!dir.exists("plots")) dir.create("plots")

# Plot 1: Side-by-Side Bar Chart
p1 <- ggplot(Profile_Agg, aes(x = factor(Cluster), y = Contribution, fill = Pollutant)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c24) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.3)) +
  labs(title = "Source Profiles: Side-by-Side (Weighted)",
       x = "Cluster", y = "Normalized Contribution") +
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = c(1.5, 2.5), linetype="dashed", color="grey")

print(p1)

# Plot 2: Stacked Bar Chart
p2 <- ggplot(Profile_Agg, aes(x = factor(Cluster), y = Contribution, fill = Pollutant)) +
  geom_col(position = "fill", width = 0.7, color="black", size=0.2) +
  scale_fill_manual(values = c24) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Source Profiles: Stacked (Weighted)",
       x = "Cluster", y = "Contribution") +
  theme_minimal(base_size = 14) +
  # Add labels for contributions > 10%
  geom_text(aes(label = ifelse(Contribution > 0.10, as.character(Pollutant), "")), 
            position = position_stack(vjust = 0.5), size = 3)

print(p2)

# 7. MANOVA Statistics (Optional) -----------------------------------------
message("Running MANOVA to test cluster differences...")

# Check if clusters are statistically different based on chemistry
manova_fit <- manova(cbind(Cl, Ca, Si, Fe, Al, K, Ti, Cu, Ba, Zn, Te, Br, Pd) ~ Cluster, 
                     data = Data_Scaled_Weighted)
print(summary(manova_fit))

# 8. Save Outputs ---------------------------------------------------------
ggsave("plots/05_Source_Profile_Dodge.png", p1, width = 12, height = 7)
ggsave("plots/05_Source_Profile_Stacked.png", p2, width = 10, height = 7)

# Save the profile data for reporting
write.csv(Profile_Agg, "data/processed/05_Cluster_Source_Profiles.csv", row.names=FALSE)

message("Script 05 Complete. Profiles and Plots saved.")