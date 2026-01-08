# ==============================================================================
# Script: 06_Environmental_Analysis.R
# Purpose: Process Weather data, Merge with Clusters, Plot Histograms & Wind Roses.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(openair)   # For windRose
library(reshape2)
library(RColorBrewer)

# 2. Load Clustered Data --------------------------------------------------
message("Loading Clustered Data...")

if(!file.exists("data/processed/04_Clustered_Data.rds")) {
  stop("Error: Input file not found. Run Script 04 first.")
}
Data_Clustered <- readRDS("data/processed/04_Clustered_Data.rds")
# Ensure date is POSIXct
Data_Clustered$date <- as.POSIXct(Data_Clustered$date, tz = "UTC")

# 3. Load Weather Data ----------------------------------------------------
message("Loading Weather Data...")

# Load Temp/Humidity Files (Flat structure in 'data/')
temp_files <- c("data/Temp_201907to202012.csv", 
                "data/Temp_202101to202212.csv", 
                "data/Temp_202301to202303.csv")

# Check if files exist before loading
if(!all(file.exists(temp_files))) stop("Error: Some Temperature files are missing in 'data/'")

Temp_Data <- do.call(rbind, lapply(temp_files, read.csv))
Temp_Data$date <- as.POSIXct(Temp_Data$date, tz = "UTC")
colnames(Temp_Data)[4] <- "Humi" # Renaming 4th col to Humi

# Load Wind Files (Flat structure in 'data/')
wind_files <- c("data/Wind_201907to202012.csv", 
                "data/Wind_202101to202112.csv", 
                "data/Wind_202201to202212.csv", 
                "data/Wind_202301to202303.csv")

if(!all(file.exists(wind_files))) stop("Error: Some Wind files are missing in 'data/'")

Wind_Data <- do.call(rbind, lapply(wind_files, read.csv))
Wind_Data$date <- as.POSIXct(Wind_Data$date, tz = "UTC")

# Merge Temp and Wind
Met_Data <- right_join(Temp_Data, Wind_Data, by="date")

# 4. Aggregate Weather to 3-Hour Resolution -------------------------------
message("Aggregating Weather to 3-Hour Means...")

# Select numeric columns
met_cols <- c("Temp_C", "Pressure_mbar", "Humi", "ws", "wd")

Met_3hr <- Met_Data %>%
  select(date, all_of(met_cols)) %>%
  group_by(date = ceiling_date(date, '3 hour')) %>%
  summarise(across(all_of(met_cols), mean, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame()

# 5. Merge Weather with Clusters ------------------------------------------
message("Merging Weather with Clustered Events...")

# Left join to keep only the High Concentration Events
Cluster_Met <- left_join(Data_Clustered, Met_3hr, by = "date")

# Extract Time Features
Cluster_Met$hour  <- hour(Cluster_Met$date)
Cluster_Met$month <- month(Cluster_Met$date)

# 6. Plotting: Frequency Histograms ---------------------------------------
message("Generating Environmental Histograms...")

if(!dir.exists("plots")) dir.create("plots")

# Define helper function for histograms
plot_env_hist <- function(data, x_var, bin_width, title, x_lab, breaks_seq) {
  ggplot(data, aes_string(x = x_var, fill = "as.factor(Cluster)")) +
    geom_histogram(binwidth = bin_width, color = "white", alpha = 0.7, position = "stack") +
    scale_x_continuous(breaks = breaks_seq) +
    labs(title = title, x = x_lab, y = "Frequency", fill = "Cluster") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}

# A. Monthly Distribution
p1 <- plot_env_hist(Cluster_Met, "month", 1, "Events by Month", "Month", 1:12)
print(p1)

# B. Hourly Distribution
p2 <- plot_env_hist(Cluster_Met, "hour", 1, "Events by Hour of Day", "Hour", seq(0, 23, 3))
print(p2)

# C. Wind Speed Distribution
p3 <- plot_env_hist(Cluster_Met, "ws", 0.5, "Events by Wind Speed", "Wind Speed (m/s)", seq(0, 10, 1))
print(p3)

# D. Wind Direction Distribution
p4 <- plot_env_hist(Cluster_Met, "wd", 30, "Events by Wind Direction", "Degrees", seq(0, 360, 45))
print(p4)

# Save Histograms
ggsave("plots/06_Hist_Month.png", p1, width=8, height=6)
ggsave("plots/06_Hist_Hour.png", p2, width=8, height=6)
ggsave("plots/06_Hist_WindSpeed.png", p3, width=8, height=6)

# 7. Plotting: Wind Roses -------------------------------------------------
message("Generating Wind Roses...")

# Set up colors for wind rose (OrRd palette)
# We output directly to PNG because openair uses lattice graphics
png("plots/06_WindRose_Clusters.png", width = 1000, height = 600)

windRose(Cluster_Met, 
         type = "Cluster", 
         layout = c(3, 1), 
         pad = 1,
         main = "Wind Rose by Cluster",
         cols = "OrRd",
         breaks = c(0, 2, 4, 6, 8, 10),
         key.footer = "ws (m/s)")

dev.off()

# 8. Save Merged Data -----------------------------------------------------
saveRDS(Cluster_Met, "data/processed/06_Clusters_with_Weather.rds")
message("Script 06 Complete. Environmental analysis saved.")